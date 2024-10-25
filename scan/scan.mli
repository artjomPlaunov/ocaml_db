type t

(* Scan methods *)
val before_first : t -> unit
val next : t -> bool
val get_int32 : t -> string -> int32
val get_string : t -> string -> string
val get_val : t -> string -> Constant.t
val has_field : t -> string -> bool
val close : t -> unit