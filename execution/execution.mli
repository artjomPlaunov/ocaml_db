(** Execution Engine *)

type t

(** e executes select operation op; Store results in output. *)
val select : e:t -> output:Buffer.t -> op:Ast.Select.t -> unit

(** e executes insert operation op. *)
val insert : e:t -> op:Ast.Insert.t -> unit

(** e executes delete operation op. *)
val delete : e:t -> op:Ast.Delete.t -> unit

(** e executes update operation op. *)
val update : e:t -> op:Ast.Update.t -> unit

(** e executes create table operation op. *)
val create_table : e:t -> op:Ast.Create_table.t -> unit

val execute : e:t -> query:Ast.Query.t -> ?output:Buffer.t -> unit -> unit

