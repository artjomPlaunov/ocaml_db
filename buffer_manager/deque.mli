type 'a t

val init : unit -> 'a t
val push_left : list:'a t -> value:'a -> unit
val push_right : list:'a t -> value:'a -> unit
val pop_left_exn : list:'a t -> 'a
val pop_right_exn : list:'a t -> 'a
val peek_right_exn : list:'a t -> 'a
val length : list:'a t -> int
