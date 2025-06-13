open File_manager

type t

exception BufferAbortException

val make :
  file_manager:File_manager.t ->
  log_manager:Log_manager.t ->
  num_buffers:int ->
  unit ->
  t

val available : t -> int
val flush_all : t -> int -> unit
val unpin : t -> Db_buffer.t -> unit
val pin : t -> Block_id.t -> Db_buffer.t
