type field_info = { ty : Type.t; length : int }
type t = { mutable fields : string list; info : (string, field_info) Hashtbl.t }

let make () = { fields = []; info = Hashtbl.create 10 }

let add_field schema field_name ty length =
  schema.fields <- List.rev (field_name :: List.rev schema.fields);
  Hashtbl.add schema.info field_name { ty; length }

let add_int_field schema field_name = add_field schema field_name Integer 0

let add_string_field schema field_name length =
  add_field schema field_name Varchar length

let fields schema = schema.fields
let has_field schema field_name = List.mem field_name schema.fields

let get_type schema field_name =
  let info = Hashtbl.find schema.info field_name in
  info.ty

let get_length schema field_name =
  let info = Hashtbl.find schema.info field_name in
  info.length

let add schema field_name other_schema =
  let ty = get_type other_schema field_name in
  let length = get_length other_schema field_name in
  add_field schema field_name ty length

let add_all schema other_schema =
  let rec f other_fields =
    match other_fields with
    | [] -> ()
    | x :: xs ->
        add schema x other_schema;
        f xs
  in
  f other_schema.fields

let length_in_bytes schema field_name =
  let ty = get_type schema field_name in
  match ty with
  | Integer -> 4
  | Varchar -> File_manager.Page.max_len (get_length schema field_name)

let to_string schema =
  let field_strings = List.map (fun field_name ->
    let field_info = Hashtbl.find schema.info field_name in
    let type_str = match field_info.ty with
      | Integer -> "INT"
      | Varchar -> Printf.sprintf "VARCHAR(%d)" field_info.length
    in
    Printf.sprintf "%s: %s" field_name type_str
  ) schema.fields in
  String.concat ", " field_strings
