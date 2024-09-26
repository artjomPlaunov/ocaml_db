type t
  
(* initialize page of blocksize. *)
val make : int -> t

val get_int32 : t -> int -> int32

val set_int32 : t -> int -> int32 -> unit

