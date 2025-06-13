open File_manager

type t

val make :
  file_manager:File_manager.t ->
  log_manager:Log_manager.t ->
  buffer_manager:Buffer_manager.t ->
  t

val commit : t -> unit
val rollback : t -> unit
val recover : t -> unit
val size : tx:t -> filename:string -> int
val append : tx:t -> filename:string -> Block_id.t
val block_size : tx:t -> int
val get_next_tx_num : unit -> int
val pin : tx:t -> block:Block_id.t -> unit
val unpin : tx:t -> block:Block_id.t -> unit
val get_int32 : tx:t -> block:Block_id.t -> offset:int -> Int32.t

val set_int :
  tx:t ->
  block:Block_id.t ->
  offset:int ->
  value:Int32.t ->
  to_log:bool ->
  unit

val set_string :
  tx:t ->
  block:Block_id.t ->
  offset:int ->
  value:string ->
  to_log:bool ->
  unit

val get_string : tx:t -> block:Block_id.t -> offset:int -> string
val size : tx:t -> filename:string -> int
