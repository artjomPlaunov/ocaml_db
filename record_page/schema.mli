type field_info
type t = { mutable fields : string list; info : (string, field_info) Hashtbl.t }

val make : unit -> t
val add_field : t -> string -> Type.t -> int -> unit
val add_int_field : t -> string -> unit
val add_string_field : t -> string -> int -> unit
val fields : t -> string list
val has_field : t -> string -> bool
val get_type : t -> string -> Type.t
val get_length : t -> string -> int
val add : t -> string -> t -> unit
val add_all : t -> t -> unit
val length_in_bytes : t -> string -> int
