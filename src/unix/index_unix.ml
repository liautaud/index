exception RO_not_allowed

module IO = struct
  let ( ++ ) = Int64.add

  let ( -- ) = Int64.sub

  module Raw = struct
    type t = { fd : Unix.file_descr; mutable cursor : int64 }

    let v fd = { fd; cursor = 0L }

    external pread : Unix.file_descr -> int64 -> bytes -> int -> int -> int
      = "caml_pread"

    external pwrite : Unix.file_descr -> int64 -> bytes -> int -> int -> int
      = "caml_pwrite"

    let really_write fd off buf =
      let rec aux fd_off buf_off len =
        let w = pwrite fd fd_off buf buf_off len in
        if w = 0 then ()
        else
          (aux [@tailcall]) (fd_off ++ Int64.of_int w) (buf_off + w) (len - w)
      in
      (aux [@tailcall]) off 0 (Bytes.length buf)

    let really_read fd off len buf =
      let rec aux fd_off buf_off len =
        let r = pread fd fd_off buf buf_off len in
        if r = 0 then buf_off (* end of file *)
        else if r = len then buf_off + r
        else
          (aux [@tailcall]) (fd_off ++ Int64.of_int r) (buf_off + r) (len - r)
      in
      (aux [@tailcall]) off 0 len

    let unsafe_write t ~off buf =
      let buf = Bytes.unsafe_of_string buf in
      really_write t.fd off buf;
      t.cursor <- off ++ Int64.of_int (Bytes.length buf)

    let unsafe_read t ~off ~len buf =
      let n = really_read t.fd off len buf in
      t.cursor <- off ++ Int64.of_int n
  end

  type t = { name : string; mutable raw : Raw.t; readonly : bool }

  let name t = t.name

  let close t = Unix.close t.raw.fd

  let rename ~src ~dst = Unix.rename src.name dst.name

  let read t ~off buf = Raw.unsafe_read t.raw ~off ~len:(Bytes.length buf) buf

  let protect_unix_exn = function
    | Unix.Unix_error _ as e -> failwith (Printexc.to_string e)
    | e -> raise e

  let ignore_enoent = function
    | Unix.Unix_error (Unix.ENOENT, _, _) -> ()
    | e -> raise e

  let protect f x = try f x with e -> protect_unix_exn e

  let safe f x = try f x with e -> ignore_enoent e

  let mkdir dirname =
    let rec aux dir k =
      if Sys.file_exists dir && Sys.is_directory dir then k ()
      else (
        if Sys.file_exists dir then safe Unix.unlink dir;
        (aux [@tailcall]) (Filename.dirname dir) @@ fun () ->
        protect (Unix.mkdir dir) 0o755;
        k () )
    in
    (aux [@tailcall]) dirname (fun () -> ())

  let v ~readonly name =
    let v ~offset ~version raw =
      {
        version;
        file;
        offset;
        raw;
        readonly;
        buf = buffer file;
        flushed = header ++ offset;
      }
    in
    let mode = Unix.(if readonly then O_RDONLY else O_RDWR) in
    mkdir (Filename.dirname file);
    match Sys.file_exists file with
    | false ->
        let x = Unix.openfile file Unix.[ O_CREAT; mode ] 0o644 in
        let raw = Raw.v x in
        Raw.Offset.set raw 0L;
        Raw.Version.set raw;
        Raw.Generation.set raw generation;
        v ~offset:0L ~version:current_version raw
    | true ->
        let x = Unix.openfile file Unix.[ O_EXCL; mode ] 0o644 in
        let raw = Raw.v x in
        if readonly && fresh then
          Fmt.failwith "IO.v: cannot reset a readonly file"
        else if fresh then (
          Raw.Offset.set raw 0L;
          Raw.Version.set raw;
          Raw.Generation.set raw generation;
          v ~offset:0L ~version:current_version raw )
        else
          let offset = Raw.Offset.get raw in
          let version = Raw.Version.get raw in
          v ~offset ~version raw

  let is_valid t =
    try
      let _ = Unix.fstat t.raw.fd in
      true
    with Unix.Unix_error (Unix.EBADF, _, _) -> false
end
