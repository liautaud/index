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

module Make (Raw : Raw) : sig
  type t

  val v :
    fresh:bool ->
    readonly:bool ->
    generation:int64 ->
    fan_size:int64 ->
    string ->
    t

  val name : t -> string

  val offset : t -> int64

  val force_offset : t -> unit

  val read : t -> off:int64 -> bytes -> int

  val clear : t -> unit

  val sync : t -> unit

  val version : t -> string

  val set_generation : t -> int64 -> unit

  val get_generation : t -> int64

  val set_fanout : t -> string -> unit

  val get_fanout : t -> string

  val rename : src:t -> dst:t -> unit

  val append : t -> string -> unit

  val close : t -> unit
end