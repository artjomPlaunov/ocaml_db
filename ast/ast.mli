(** SQL Query AST *)

type field = string
type table = string
module Predicate = Scans__Predicate
module Expression = Scans__Expression

(** {1 Select Module} *)
module Select : sig 
    type t
    val make : field list -> table list -> Predicate.t option -> t
    val fields : t -> table list
    val tables : t -> table list
    val predicate : t -> Predicate.t option
    val to_string : t -> string
end

(** {1 Create_table Module}*)
module Create_table : sig 
    type t
    val make : string -> Record_page.Schema.t -> t
    val table_name : t -> string
    val new_schema : t -> Record_page.Schema.t
    val to_string : t -> string
end 

(** {1 Insert Module}*)
module Insert : sig 
    type t
    val make : string -> string list -> Constant.t list -> t
    val table_name : t -> string 
    val fields : t -> string list 
    val values : t -> Constant.t list
    val to_string : t -> string
end 

(** {1 Delete Module}*)
module Delete : sig
  type t
  val make : string -> Predicate.t -> t
  val table_name : t -> string
  val predicate : t -> Predicate.t
  val to_string : t -> string
end

(** {1 Update Module}*)
module Update : sig 
    type t
    val make : string -> string -> Expression.t -> Predicate.t -> t
    val table_name : t -> string
    val field_name : t -> string
    val new_value : t -> Expression.t
    val predicate : t -> Predicate.t
    val to_string : t -> string
end

(** {1 Create_index Module} *)
module Create_index : sig 
    type t
    val make : string -> string -> string -> t
    val index_name : t -> string 
    val table_name : t -> string
    val field_name : t -> string 
    val to_string : t -> string
end


(** {1 Query Module} *)
module Query : sig 
    type t = 
  | Select of Select.t   
  | CreateTable of Create_table.t
  | Insert of Insert.t
  | Delete of Delete.t
  | Update of Update.t
  | CreateIndex of Create_index.t
end 



