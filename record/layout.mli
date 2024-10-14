type t

val make : Schema.t -> t
val create : t -> (string, int) Hashtbl.t -> int -> t
val get_schema : t -> Schema.t
val get_offset : t -> string -> int
