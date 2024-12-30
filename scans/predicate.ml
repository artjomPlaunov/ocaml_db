module Schema = Record_page.Schema

type t = Term.t list

let init () = []
let make term = [ term ]
let conjoin_with t pred = List.concat [ t; pred ]

let is_satisfied t scan =
  List.fold_left
    (fun acc term ->
      if not (Term.is_satisfied term scan) then acc || false else true)
    false t

let select_sub_pred t schema =
  let acc = init () in
  let pred =
    List.fold_left
      (fun acc term ->
        if Term.applies_to term schema then List.append acc [ term ] else acc)
      acc t
  in
  if List.length pred == 0 then None else Some pred

let join_sub_pred t schema1 schema2 =
  let new_schema = Schema.make () in
  Schema.add_all new_schema schema1;
  Schema.add_all new_schema schema2;
  let acc = init () in
  let pred =
    List.fold_left
      (fun acc term ->
        if
          (not (Term.applies_to term schema1))
          && (not (Term.applies_to term schema2))
          && Term.applies_to term new_schema
        then List.append acc [ term ]
        else acc)
      acc t
  in
  if List.length pred = 0 then None else Some pred

let equates_with_constant t field_name =
  List.find_map (fun term -> Term.equates_with_constant term field_name) t

let equates_with_field t field_name =
  List.find_map (fun term -> Term.equates_with_field term field_name) t

let to_string t =
  List.map (fun term -> Term.to_string term) t |> String.concat ", "
