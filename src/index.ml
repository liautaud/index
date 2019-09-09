module Private = struct
  module Fan = Fan
  module Search = Search
end

module type Key = sig
  type t

  val equal : t -> t -> bool

  val hash : t -> int

  val hash_size : int

  val encode : t -> string

  val decode : string -> int -> t

  val encoded_size : int

  val pp : t Fmt.t
end

module type Value = sig
  type t

  val encode : t -> string

  val decode : string -> int -> t

  val encoded_size : int

  val pp : t Fmt.t
end

module type S = sig
  type t

  type key

  type value

  val v : ?fresh:bool -> ?readonly:bool -> log_size:int -> string -> t

  val clear : t -> unit

  val find : t -> key -> value

  val mem : t -> key -> bool

  exception Invalid_key_size of key

  exception Invalid_value_size of value

  val replace : t -> key -> value -> unit

  val iter : (key -> value -> unit) -> t -> unit

  val flush : t -> unit

  val close : t -> unit

  val force_merge : t -> key -> value -> unit
end

let may f = function None -> () | Some bf -> f bf

exception RO_not_allowed

let src = Logs.Src.create "index" ~doc:"Index"

module Log = (val Logs.src_log src : Logs.LOG)

module Make (K : Key) (V : Value) (Raw : Fs.Raw) = struct
  module FS = Fs.Make(Raw)

  type key = K.t

  type value = V.t

  type entry = { key : key; key_hash : int; value : value }

  let entry_size = K.encoded_size + V.encoded_size

  let entry_sizeL = Int64.of_int entry_size

  exception Invalid_key_size of key

  exception Invalid_value_size of value

  let append_entry io e =
    let encoded_key = K.encode e.key in
    let encoded_value = V.encode e.value in
    if String.length encoded_key <> K.encoded_size then
      raise (Invalid_key_size e.key);
    if String.length encoded_value <> V.encoded_size then
      raise (Invalid_value_size e.value);
    FS.append io encoded_key;
    FS.append io encoded_value

  let decode_entry bytes off =
    let string = Bytes.unsafe_to_string bytes in
    let key = K.decode string off in
    let value = V.decode string (off + K.encoded_size) in
    { key; key_hash = K.hash key; value }

  module Tbl = Hashtbl.Make (K)

  type config = { log_size : int; readonly : bool }

  type index = { io : FS.t; fan_out : Fan.t }

  type t = {
    config : config;
    root : string;
    mutable generation : int64;
    mutable index : index option;
    log : FS.t;
    log_mem : entry Tbl.t;
    mutable counter : int;
  }

  let clear t =
    Log.debug (fun l -> l "clear %S" t.root);
    t.generation <- 0L;
    FS.clear t.log;
    Tbl.clear t.log_mem;
    may
      (fun i ->
        FS.clear i.io;
        FS.close i.io)
      t.index;
    t.index <- None

  let ( // ) = Filename.concat

  let index_dir root = root // "index"

  let log_path root = index_dir root // "log"

  let index_path root = index_dir root // "data"

  let merge_path root = index_dir root // "merge"

  let page_size = Int64.mul entry_sizeL 1_000L

  let iter_io_off ?(min = 0L) ?max f io =
    let max = match max with None -> FS.offset io | Some m -> m in
    let rec aux offset =
      let remaining = Int64.sub max offset in
      if remaining <= 0L then ()
      else
        let size = Stdlib.min remaining page_size in
        let raw = Bytes.create (Int64.to_int size) in
        let n = FS.read io ~off:offset raw in
        let rec read_page page off =
          if off = n then ()
          else
            let entry = decode_entry page off in
            f Int64.(add (of_int off) offset) entry;
            (read_page [@tailcall]) page (off + entry_size)
        in
        read_page raw 0;
        (aux [@tailcall]) Int64.(add offset page_size)
    in
    (aux [@tailcall]) min

  let iter_io ?min ?max f io = iter_io_off ?min ?max (fun _ e -> f e) io

  module Entry = struct
    type t = entry

    module Key = K
    module Value = V

    let to_key e = e.key

    let to_value e = e.value
  end

  module FSArray = struct
    type buffer = { buf : bytes; low_off : int64; high_off : int64 }

    type t = { io : FS.t; mutable buffer : buffer option }

    let v io = { io; buffer = None }

    let get_entry_from_io io off =
      let buf = Bytes.create entry_size in
      let n = FS.read io ~off buf in
      assert (n = entry_size);
      decode_entry buf 0

    let ( -- ) = Int64.sub

    let get_entry_from_buffer buf off =
      let buf_off = Int64.(to_int @@ (off -- buf.low_off)) in
      assert (buf_off <= Bytes.length buf.buf);
      decode_entry buf.buf buf_off

    type elt = entry

    let is_in_buffer t off =
      match t.buffer with
      | None -> false
      | Some b ->
          Int64.compare off b.low_off >= 0 && Int64.compare off b.high_off <= 0

    let get t i =
      let off = Int64.(mul i entry_sizeL) in
      match t.buffer with
      | Some b when is_in_buffer t off -> (
          try get_entry_from_buffer b off with _ -> assert false )
      | _ -> get_entry_from_io t.io off

    let length t = Int64.(div (FS.offset t.io) entry_sizeL)

    let set_buffer t ~low ~high =
      let range = entry_size * (1 + Int64.to_int (high -- low)) in
      let low_off = Int64.mul low entry_sizeL in
      let high_off = Int64.mul high entry_sizeL in
      let buf = Bytes.create range in
      let n = FS.read t.io ~off:low_off buf in
      assert (n = range);
      t.buffer <- Some { buf; low_off; high_off }

    let pre_fetch t ~low ~high =
      let range = entry_size * (1 + Int64.to_int (high -- low)) in
      if Int64.compare low high > 0 then
        Logs.warn (fun m ->
            m "Requested pre-fetch region is empty: [%Ld, %Ld]" low high)
      else if range > 4096 then
        Logs.debug (fun m ->
            m "Requested pre-fetch [%Ld, %Ld] is larger than 4096" low high)
      else
        match t.buffer with
        | Some b ->
            let low_buf, high_buf =
              Int64.(div b.low_off entry_sizeL, div b.high_off entry_sizeL)
            in
            if low >= low_buf && high <= high_buf then
              Logs.debug (fun m ->
                  m
                    "Pre-existing buffer [%Ld, %Ld] encloses requested \
                     pre-fetch [%Ld, %Ld]"
                    low_buf high_buf low high)
            else (
              Logs.debug (fun m ->
                  m
                    "Current buffer [%Ld, %Ld] insufficient. Prefetching in \
                     range [%Ld, %Ld]"
                    low_buf high_buf low high);
              set_buffer t ~low ~high )
        | None ->
            Logs.debug (fun m ->
                m "No existing buffer. Prefetching in range [%Ld, %Ld]" low
                  high);
            set_buffer t ~low ~high
  end

  module Search =
    Search.Make (Entry) (FSArray)
      (struct
        type t = int

        module Entry = Entry

        let compare : int -> int -> int = compare

        let of_entry e = e.key_hash

        let of_key = K.hash

        let linear_interpolate ~low:(low_index, low_metric)
            ~high:(high_index, high_metric) key_metric =
          let low_in = float_of_int low_metric in
          let high_in = float_of_int high_metric in
          let target_in = float_of_int key_metric in
          let low_out = Int64.to_float low_index in
          let high_out = Int64.to_float high_index in
          (* Fractional position of [target_in] along the line from [low_in] to [high_in] *)
          let proportion = (target_in -. low_in) /. (high_in -. low_in) in
          (* Convert fractional position to position in output space *)
          let position = low_out +. (proportion *. (high_out -. low_out)) in
          let rounded = ceil (position -. 0.5) +. 0.5 in
          Int64.of_float rounded
      end)

  let with_cache ~v ~clear =
    let roots = Hashtbl.create 0 in
    let f ?(fresh = false) ?(readonly = false) ~log_size root =
      try
        if not (Sys.file_exists (index_dir root)) then (
          Log.debug (fun l ->
              l "[%s] does not exist anymore, cleaning up the fd cache"
                (Filename.basename root));
          Hashtbl.remove roots (root, true);
          Hashtbl.remove roots (root, false);
          raise Not_found );
        let t = Hashtbl.find roots (root, readonly) in
        if t.counter <> 0 then (
          Log.debug (fun l -> l "%s found in cache" root);
          t.counter <- t.counter + 1;
          if fresh then clear t;
          t )
        else (
          Hashtbl.remove roots (root, readonly);
          raise Not_found )
      with Not_found ->
        Log.debug (fun l ->
            l "[%s] v fresh=%b readonly=%b" (Filename.basename root) fresh
              readonly);
        let t = v ~fresh ~readonly ~log_size root in
        Hashtbl.add roots (root, readonly) t;
        t
    in
    `Staged f

  let v_no_cache ~fresh ~readonly ~log_size root =
    let config = { log_size = log_size * entry_size; readonly } in
    let log_path = log_path root in
    let index_path = index_path root in
    let log_mem = Tbl.create 1024 in
    let log = FS.v ~fresh ~readonly ~generation:0L ~fan_size:0L log_path in
    let generation = FS.get_generation log in
    let index =
      if Sys.file_exists index_path then
        let io =
          FS.v ~fresh ~readonly ~generation:0L ~fan_size:0L index_path
        in
        let fan_out = Fan.import ~hash_size:K.hash_size (FS.get_fanout io) in
        Some { fan_out; io }
      else None
    in
    iter_io (fun e -> Tbl.replace log_mem e.key e) log;
    { config; generation; log_mem; root; log; index; counter = 1 }

  let (`Staged v) = with_cache ~v:v_no_cache ~clear

  let interpolation_search index key =
    let hashed_key = K.hash key in
    let low_bytes, high_bytes = Fan.search index.fan_out hashed_key in
    let low, high =
      Int64.(div low_bytes entry_sizeL, div high_bytes entry_sizeL)
    in
    Search.interpolation_search (FSArray.v index.io) key ~low ~high

  let sync_log t =
    let generation = FS.get_generation t.log in
    let log_offset = FS.offset t.log in
    let () = FS.force_offset t.log in
    let new_log_offset = FS.offset t.log in
    let add_log_entry e = Tbl.replace t.log_mem e.key e in
    if t.generation <> generation then (
      Tbl.clear t.log_mem;
      iter_io add_log_entry t.log;
      may (fun i -> FS.close i.io) t.index;
      if Int64.equal generation 0L then t.index <- None
      else
        let index_path = index_path t.root in
        let io =
          FS.v ~fresh:false ~readonly:true ~generation ~fan_size:0L index_path
        in
        let fan_out =
          Fan.import ~hash_size:K.encoded_size (FS.get_fanout io)
        in
        t.index <- Some { fan_out; io };
        t.generation <- generation )
    else if log_offset < new_log_offset then
      iter_io add_log_entry t.log ~min:log_offset
    else if log_offset > new_log_offset then assert false

  let find t key =
    Log.debug (fun l -> l "find %a" K.pp key);
    if t.config.readonly then sync_log t;
    let look_on_disk () =
      match Tbl.find t.log_mem key with
      | e -> e.value
      | exception Not_found -> (
          match t.index with
          | Some index -> interpolation_search index key
          | None -> raise Not_found )
    in
    look_on_disk ()

  let mem t key =
    Log.debug (fun l -> l "mem %a" K.pp key);
    match find t key with _ -> true | exception Not_found -> false

  let append_buf_fanout fan_out hash buf_str dst_io =
    Fan.update fan_out hash (FS.offset dst_io);
    FS.append dst_io buf_str

  let append_entry_fanout fan_out entry dst_io =
    Fan.update fan_out entry.key_hash (FS.offset dst_io);
    append_entry dst_io entry

  let rec merge_from_log fan_out log log_i hash_e dst_io =
    if log_i >= Array.length log then log_i
    else
      let v = log.(log_i) in
      if v.key_hash > hash_e then log_i
      else (
        append_entry_fanout fan_out v dst_io;
        (merge_from_log [@tailcall]) fan_out log (log_i + 1) hash_e dst_io )

  let append_remaining_log fan_out log log_i dst_io =
    for log_i = log_i to Array.length log - 1 do
      append_entry_fanout fan_out log.(log_i) dst_io
    done

  (** Merge [log] with [t] into [dst_io].
      [log] must be sorted by key hashes. *)
  let merge_with log index dst_io =
    let entries = 10_000 in
    let buf = Bytes.create (entries * entry_size) in
    let refill off = ignore (FS.read index.io ~off buf) in
    let index_end = FS.offset index.io in
    let fan_out = index.fan_out in
    refill 0L;
    let rec go index_offset buf_offset log_i =
      if index_offset >= index_end then
        append_remaining_log fan_out log log_i dst_io
      else
        let buf_str = Bytes.sub_string buf buf_offset entry_size in
        let index_offset = Int64.add index_offset entry_sizeL in
        let key_e = K.decode buf_str 0 in
        let hash_e = K.hash key_e in
        let log_i = merge_from_log fan_out log log_i hash_e dst_io in
        if
          log_i >= Array.length log
          ||
          let key = log.(log_i).key in
          not (K.equal key key_e)
        then append_buf_fanout fan_out hash_e buf_str dst_io;
        let buf_offset =
          let n = buf_offset + entry_size in
          if n >= Bytes.length buf then (
            refill index_offset;
            0 )
          else n
        in
        (go [@tailcall]) index_offset buf_offset log_i
    in
    (go [@tailcall]) 0L 0 0

  let merge ~witness t =
    Log.debug (fun l -> l "unforced merge %S\n" t.root);
    let merge_path = merge_path t.root in
    let generation = Int64.succ t.generation in
    let log =
      let compare_entry e e' = compare e.key_hash e'.key_hash in
      let b = Array.make (Tbl.length t.log_mem) witness in
      Tbl.fold
        (fun _ e i ->
          b.(i) <- e;
          i + 1)
        t.log_mem 0
      |> ignore;
      Array.fast_sort compare_entry b;
      b
    in
    let fan_size =
      match t.index with
      | None -> Tbl.length t.log_mem
      | Some index ->
          (Int64.to_int (FS.offset index.io) / entry_size)
          + Tbl.length t.log_mem
    in
    let fan_out = Fan.v ~hash_size:K.hash_size ~entry_size fan_size in
    let merge =
      FS.v ~readonly:false ~fresh:true ~generation
        ~fan_size:(Int64.of_int (Fan.exported_size fan_out))
        merge_path
    in
    ( match t.index with
    | None ->
        let io =
          FS.v ~fresh:true ~readonly:false ~generation:0L ~fan_size:0L
            (index_path t.root)
        in
        append_remaining_log fan_out log 0 merge;
        t.index <- Some { io; fan_out }
    | Some index ->
        let index = { index with fan_out } in
        merge_with log index merge;
        t.index <- Some index );
    match t.index with
    | None -> assert false
    | Some index ->
        Fan.finalize index.fan_out;
        FS.set_fanout merge (Fan.export index.fan_out);
        FS.rename ~src:merge ~dst:index.io;
        FS.clear t.log;
        Tbl.clear t.log_mem;
        FS.set_generation t.log generation;
        t.generation <- generation

  let force_merge t key value =
    Log.debug (fun l -> l "forced merge %S\n" t.root);
    merge ~witness:{ key; key_hash = K.hash key; value } t

  let replace t key value =
    Log.debug (fun l -> l "add %a %a" K.pp key V.pp value);
    if t.config.readonly then raise RO_not_allowed;
    let entry = { key; key_hash = K.hash key; value } in
    append_entry t.log entry;
    Tbl.replace t.log_mem key entry;
    if Int64.compare (FS.offset t.log) (Int64.of_int t.config.log_size) > 0
    then merge ~witness:entry t

  (* XXX: Perform a merge beforehands to ensure duplicates are not hit twice. *)
  let iter f t =
    Tbl.iter (fun _ e -> f e.key e.value) t.log_mem;
    may (fun index -> iter_io (fun e -> f e.key e.value) index.io) t.index

  let flush t = FS.sync t.log

  let close t =
    t.counter <- t.counter - 1;
    if t.counter = 0 then (
      Log.debug (fun l -> l "close %S" t.root);
      if not t.config.readonly then flush t;
      FS.close t.log;
      may (fun i -> FS.close i.io) t.index;
      t.index <- None;
      Tbl.reset t.log_mem )
end
