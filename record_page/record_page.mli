(** Record page management module.
    This module handles the storage and retrieval of records within database pages. *)

(** {1 Type Module} *)

module Type : sig
  (** Represents the supported data types in the database *)
  type t = Integer | Varchar

  val to_int : t -> int
  (** Convert a type to its integer representation *)

  val of_int : int -> t
  (** Convert an integer to its type representation.
      @raise Failure if the integer doesn't correspond to a valid type *)
end

(** {1 Schema Module} *)

module Schema : sig
  (** Information about a field including its type and length *)
  type field_info = { ty : Type.t; length : int }

  (** Represents the schema of a table with field names and their information *)
  type t = { 
    mutable fields : string list; 
    info : (string, field_info) Hashtbl.t 
  }

  val make : unit -> t
  (** Create a new empty schema *)

  val add_field : t -> string -> Type.t -> int -> unit
  (** [add_field schema name type length] adds a new field to the schema *)

  val add_int_field : t -> string -> unit
  (** [add_int_field schema name] adds an integer field to the schema *)

  val add_string_field : t -> string -> int -> unit
  (** [add_string_field schema name length] adds a varchar field with given length *)

  val fields : t -> string list
  (** Get list of field names in the schema *)

  val has_field : t -> string -> bool
  (** Check if a field exists in the schema *)

  val get_type : t -> string -> Type.t
  (** Get the type of a field *)

  val get_length : t -> string -> int
  (** Get the length of a field *)

  val add : t -> string -> t -> unit
  (** [add schema field_name other_schema] copies a field from another schema *)

  val add_all : t -> t -> unit
  (** Copy all fields from another schema *)

  val length_in_bytes : t -> string -> int
  (** Get the storage length in bytes for a field *)

  val to_string : t -> string
  (** String representation of the schema *)
end

(** {1 Layout Module} *)

module Layout : sig
  (** Represents the physical layout of records in a page *)
  type t = {
    schema : Schema.t;
    offsets : (string, int) Hashtbl.t;
    slot_size : int;
  }

  val make : Schema.t -> t
  (** Create a new layout from a schema *)

  val create : Schema.t -> (string, int) Hashtbl.t -> int -> t
  (** Create a layout with predefined offsets and slot size *)

  val get_schema : t -> Schema.t
  (** Get the schema associated with this layout *)

  val get_offset : t -> string -> int
  (** Get the offset of a field within a record *)

  val get_slot_size : t -> int
  (** Get the total size of a record slot *)

  val to_string : t -> string
  (** String representation of the layout *)
end

(** {1 Record Page Operations} *)

(** Represents a page of records *)
type t

val make : Transaction.t -> File.Block_id.t -> Layout.t -> t
(** Create a new record page *)

val block : t -> File.Block_id.t
(** Get the block ID of this record page *)

val get_int32 : t -> int -> string -> Int32.t
(** [get_int32 page slot field] gets an integer value from a record *)

val get_string : t -> int -> string -> string
(** [get_string page slot field] gets a string value from a record *)

val set_int32 : t -> int -> string -> Int32.t -> unit
(** [set_int32 page slot field value] sets an integer value in a record *)

val set_string : t -> int -> string -> string -> unit
(** [set_string page slot field value] sets a string value in a record *)

val delete : t -> int -> unit
(** [delete page slot] marks a record as deleted *)

val format : t -> unit
(** Initialize a new page *)

val insert_after : t -> int -> int
(** [insert_after page slot] finds space for and inserts a new record *)

val next_after : t -> int -> int
(** [next_after page slot] finds the next non-deleted record after given slot *)