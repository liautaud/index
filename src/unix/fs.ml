type 'a t = {
  mutable name : string;
  mutable fd : Unix.file_descr;
  readonly : bool;
}

external pread : Unix.file_descr -> int64 -> bytes -> int -> int -> int
  = "caml_pread"

external pwrite : Unix.file_descr -> int64 -> bytes -> int -> int -> int
  = "caml_pwrite"

let name t = t.name

let really_write fd off buf =
  let rec aux fd_off buf_off len =
    let w = pwrite fd fd_off buf buf_off len in
    if w = 0 then ()
    else (aux [@tailcall]) Int64.(add fd_off (of_int w)) (buf_off + w) (len - w)
  in
  (aux [@tailcall]) off 0 (Bytes.length buf)

let really_read fd off len buf =
  let rec aux fd_off buf_off len =
    let r = pread fd fd_off buf buf_off len in
    if r = 0 then buf_off (* end of file *)
    else if r = len then buf_off + r
    else (aux [@tailcall]) Int64.(add off (of_int r)) (buf_off + r) (len - r)
  in
  (aux [@tailcall]) off 0 len

let write t ~off buf = really_write t.fd off buf

let close t = Unix.close t.fd

let rename ~src ~dst =
  Unix.close dst.fd;
  Unix.rename src.name dst.name;
  dst.fd <- src.fd

let read t ~off buf =
  let len = Bytes.length buf in
  really_read t.fd off len buf

let protect f x =
  try f x with
  | Unix.Unix_error _ as e -> failwith (Printexc.to_string e)
  | e -> raise e

let safe f x =
  try f x with Unix.Unix_error (Unix.ENOENT, _, _) -> () | e -> raise e

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
  let v fd = { name; fd; readonly } in
  mkdir (Filename.dirname name);
  let readonly_flag = Unix.(if readonly then O_RDONLY else O_RDWR) in
  let created, modes =
    match Sys.file_exists name with
    | false -> (false, Unix.[ O_CREAT; readonly_flag ])
    | true -> (true, Unix.[ O_EXCL; readonly_flag ])
  in
  let fd = Unix.openfile name modes 0o644 in
  (created, v fd)

let v_rdonly = v ~readonly:true

let v = v ~readonly:false

let is_valid t =
  try
    let _ = Unix.fstat t.fd in
    true
  with Unix.Unix_error (Unix.EBADF, _, _) -> false
