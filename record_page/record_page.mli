type t

val make : Transaction.t -> File.Block_id.t -> Layout.t -> t
val get_int32 : t -> int -> string -> Int32.t
val get_string : t -> int -> string -> string
val set_int32 : t ->  int -> string -> Int32.t -> unit
val set_string : t -> int -> string -> string -> unit
val delete : t -> int -> unit