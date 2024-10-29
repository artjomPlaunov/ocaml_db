type t

(* Scan methods *)
val get_rid : scan:t -> Record_id.t
val move_to_rid : scan:t -> rid:Record_id.t -> unit
val delete : scan:t -> unit
val insert : scan:t -> unit
val set_string : scan:t -> field_name:string -> value:string -> unit
val set_int32 : scan:t -> field_name:string -> value:Int32.t -> unit
val set_val : scan:t -> field_name:string -> value:Constant.t -> unit
val before_first : scan:t -> unit
val next : scan:t -> bool
val get_int32 : scan:t -> field_name:string -> int32
val get_string : scan:t -> field_name:string -> string
val get_val : scan:t -> field_name:string -> Constant.t
val has_field : scan:t -> field_name:string -> bool
val close : scan:t -> unit
