(** Record_Page *)

module Type : sig
  type t = Integer | Varchar

  val to_int : t -> int
  val of_int : int -> t
end

module Schema : sig 
    type field_info = { ty : Type.t; length : int }
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
    val to_string : t -> string
end


module Layout : sig
    type t = {
    schema : Schema.t;
    offsets : (string, int) Hashtbl.t;
    slot_size : int;
    }

    val make : Schema.t -> t
    val create : Schema.t -> (string, int) Hashtbl.t -> int -> t
    val get_schema : t -> Schema.t
    val get_offset : t -> string -> int
    val get_slot_size : t -> int
    val to_string : t -> string
end

type t

val make : Transaction.t -> File.Block_id.t -> Layout.t -> t
val block : t -> File.Block_id.t
val get_int32 : t -> int -> string -> Int32.t
val get_string : t -> int -> string -> string
val set_int32 : t -> int -> string -> Int32.t -> unit
val set_string : t -> int -> string -> string -> unit
val delete : t -> int -> unit
val format : t -> unit
val insert_after : t -> int -> int
val next_after : t -> int -> int