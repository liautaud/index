type 'a t

val v : string -> bool * [ `Read | `Write ] t

val v_rdonly : string -> bool * [> `Read ] t

val name : [ `Read | `Write ] t -> string

val read : [> `Read ] t -> off:int64 -> bytes -> int

val write : [> `Write ] t -> off:int64 -> bytes -> unit

val rename : src:[> `Write ] t -> dst:[> `Write ] t -> unit

val close : [ `Read | `Write ] t -> unit

val is_valid : [> `Read ] t -> bool
