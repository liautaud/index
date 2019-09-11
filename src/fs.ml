module type Raw = sig
  type 'a t

  val v : string -> bool * [ `Read | `Write ] t

  val v_rdonly : string -> bool * [> `Read ] t

  val name : [ `Read | `Write ] t -> string

  val read : [> `Read ] t -> off:int64 -> bytes -> int

  val write : [> `Write ] t -> off:int64 -> bytes -> unit

  val rename : src:[> `Write ] t -> dst:[> `Write ] t -> unit

  val close : [ `Read | `Write ] t -> unit

  val is_valid : [> `Read ] t -> bool
end

module Make (Raw : Raw) = struct
  let current_version = "00000001"

  let current_version_bytes = Bytes.of_string "00000001"

  let () = assert (Bytes.length current_version_bytes = 8)

  let ( ++ ) = Int64.add

  let ( -- ) = Int64.sub

  external set_64 : Bytes.t -> int -> int64 -> unit = "%caml_string_set64u"

  external get_64 : string -> int -> int64 = "%caml_string_get64"

  external swap64 : int64 -> int64 = "%bswap_int64"

  let encode_int64 i =
    let set_uint64 s off v =
      if not Sys.big_endian then set_64 s off (swap64 v) else set_64 s off v
    in
    let b = Bytes.create 8 in
    set_uint64 b 0 i;
    b

  let decode_int64 buf =
    let get_uint64 s off =
      if not Sys.big_endian then swap64 (get_64 s off) else get_64 s off
    in
    get_uint64 buf 0

  module Offset = struct
    let set t n =
      let buf = encode_int64 n in
      Raw.write t ~off:0L buf

    let get t =
      let buf = Bytes.create 8 in
      let n = Raw.read t ~off:0L buf in
      assert (n = 8);
      decode_int64 (Bytes.unsafe_to_string buf)
  end

  module Version = struct
    let get t =
      let buf = Bytes.create 8 in
      let n = Raw.read t ~off:8L buf in
      assert (n = 8);
      Bytes.unsafe_to_string buf

    let set t = Raw.write t ~off:8L current_version_bytes
  end

  module Generation = struct
    let get t =
      let buf = Bytes.create 8 in
      let n = Raw.read t ~off:16L buf in
      assert (n = 8);
      decode_int64 (Bytes.unsafe_to_string buf)

    let set t gen =
      let buf = encode_int64 gen in
      Raw.write t ~off:16L buf
  end

  module Fan = struct
    let set t buf =
      let size = encode_int64 (Int64.of_int (String.length buf)) in
      Raw.write t ~off:24L size;
      if buf <> "" then
        Raw.write t ~off:(24L ++ 8L) (Bytes.of_string buf)

    let get_size t =
      let size_buf = Bytes.create 8 in
      let n = Raw.read t ~off:24L size_buf in
      assert (n = 8);
      decode_int64 (Bytes.unsafe_to_string size_buf)

    let set_size t size =
      let buf = encode_int64 size in
      Raw.write t ~off:24L buf

    let get t =
      let size = Int64.to_int (get_size t) in
      let buf = Bytes.create size in
      let n = Raw.read t ~off:(24L ++ 8L) buf in
      assert (n = size);
      Bytes.unsafe_to_string buf
  end

  type t = {
    mutable header : int64;
    mutable raw : [`Read | `Write] Raw.t;
    mutable offset : int64;
    mutable flushed : int64;
    mutable fan_size : int64;
    version : string;
    buf : Buffer.t;
  }

  let name t = Raw.name t.raw

  let sync t =
    let buf = Buffer.to_bytes t.buf in
    let offset = t.offset in
    Buffer.clear t.buf;
    if Bytes.length buf = 0 then ()
    else (
      Raw.write t.raw ~off:t.flushed buf;
      Offset.set t.raw offset;

      (* concurrent append might happen so here t.offset might differ
         from offset *)
      if
        not (t.flushed ++ Int64.of_int (Bytes.length buf) = t.header ++ offset)
      then
        Fmt.failwith "sync error: %s flushed=%Ld buf=%Ld offset+header=%Ld\n%!"
          (Raw.name t.raw) t.flushed
          (Int64.of_int (Bytes.length buf))
          (offset ++ t.header);
      t.flushed <- offset ++ t.header )

  let rename ~src ~dst =
    sync src;
    Raw.rename ~src:src.raw ~dst:dst.raw;
    dst.header <- src.header;
    dst.fan_size <- src.fan_size;
    dst.offset <- src.offset;
    dst.flushed <- src.flushed;
    dst.raw <- src.raw

  let close t = Raw.close t.raw

  let auto_flush_limit = 1_000_000L

  let append t buf =
    Buffer.add_string t.buf buf;
    let len = Int64.of_int (String.length buf) in
    t.offset <- t.offset ++ len;
    if t.offset -- t.flushed > auto_flush_limit then sync t

  let read t ~off buf = Raw.read t.raw ~off:(t.header ++ off) buf

  let offset t = t.offset

  let force_offset t = t.offset <- Offset.get t.raw

  let version t = t.version

  let get_generation t = Generation.get t.raw

  let set_generation t = Generation.set t.raw

  let get_fanout t = Fan.get t.raw

  let set_fanout t buf =
    assert (Int64.equal (Int64.of_int (String.length buf)) t.fan_size);
    Fan.set t.raw buf

  let clear t =
    t.offset <- 0L;
    t.flushed <- t.header;
    Generation.set t.raw 0L;
    Offset.set t.raw 0L;
    Buffer.clear t.buf

  let buffers = Hashtbl.create 256

  let buffer file =
    try
      let buf = Hashtbl.find buffers file in
      Buffer.clear buf;
      buf
    with Not_found ->
      let buf = Buffer.create (4 * 1024) in
      Hashtbl.add buffers file buf;
      buf

  let v ~fresh ~readonly  ~generation ~fan_size name =
    let v ~fan_size ~offset ~version raw =
      let header = 8L ++ 8L ++ 8L ++ 8L ++ fan_size in
      {
        version;
        header;
        offset;
        raw;
        fan_size;
        buf = buffer name;
        flushed = header ++ offset;
      }
    in
    let created, raw =
      if not readonly then
        Raw.v name
      else
        Raw.v_rdonly name
    in
    if created || fresh then (
      Offset.set raw 0L;
      Fan.set_size raw fan_size;
      Version.set raw;
      Generation.set raw generation;
      v ~fan_size ~offset:0L ~version:current_version raw )
    else
      let offset = Offset.get raw in
      let version = Version.get raw in
      let fan_size = Fan.get_size raw in
      v ~fan_size ~offset ~version raw
end
