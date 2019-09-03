module type S = sig
  type 'a t

  val v : string -> [`Read | `Write] t

  val v_rdonly : string -> [`Read] t

  val read : [> `Read] t -> off:int64 -> bytes -> int

  val write : [> `Write] t -> off:int64 -> bytes -> unit

  val rename : [> `Write] t -> string -> unit

  val close : [`Read | `Write] t -> unit

  val is_valid : [> `Read] t -> bool
end
