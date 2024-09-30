open File

type t

val contents : t -> Page.t
val block : t -> Block_id.t
val is_pinned : t -> bool
val set_modified : t -> int -> int -> unit
val modifying_tx : t -> int
