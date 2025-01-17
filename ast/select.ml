module Predicate = Scans__Predicate
type field = string
type table = string


type t = {
  fields : field list;
  tables : table list;
  predicate : Predicate.t option;
}

let make fields tables predicate = { fields; tables; predicate }
let fields data = data.fields
let tables data = data.tables
let predicate data = data.predicate

let to_string data =
  let fields_str = String.concat ", " data.fields in
  let tables_str = String.concat ", " data.tables in
  let pred_str =
    match data.predicate with
    | Some pred -> " WHERE " ^ Predicate.to_string pred
    | None -> ""
  in
  "SELECT " ^ fields_str ^ " FROM " ^ tables_str ^ pred_str