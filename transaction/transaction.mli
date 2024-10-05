open File

type t

val make :
  file_manager:File_manager.t ->
  log_manager:Log_manager.t ->
  buffer_manager:Buffer_manager.t ->
  t

val commit : t -> unit
val rollback : t -> unit
val recover : t -> unit
