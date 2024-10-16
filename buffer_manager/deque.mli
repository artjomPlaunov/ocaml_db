type 'a t

val init : unit -> 'a t
val push_left : 'a t -> 'a -> unit
val push_right : 'a t -> 'a -> unit
val pop_left_exn : 'a t -> 'a
val pop_right_exn : 'a t -> 'a
val peek_right_exn : 'a t -> 'a
val length : 'a t -> int
