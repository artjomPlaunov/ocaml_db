module Table_scan = Scans__Table_scan
module Select_scan = Scans__Select_scan
open Parser
open Ast
open Table_manager

type t = { tx : Transaction.t }

let get_table_layout e tbl_name =
  let table_mgr = Table_manager.make ~is_new:false ~tx:e.tx in
  Table_manager.get_layout ~table_mgr ~tbl_name ~tx:e.tx

let select ~e ~output ~op =
  match Select.tables op with
  | [] -> failwith "No tables specified in SELECT"
  | table :: _ ->
      let layout = get_table_layout e table in
      let tbl_scan = Table_scan.make ~tx:e.tx ~tbl_name:table ~layout in

      (* Apply WHERE clause if present *)
      let filtered_scan =
        match Select.predicate op with
        | Some pred -> new Select_scan.t tbl_scan pred
        | None -> tbl_scan
      in

      (* Output the results of the selection in a table format *)
      let fields = Select.fields op in
      let column_widths = List.map (fun _ -> 15) fields in
      let separator =
        String.concat "+"
          (List.map (fun width -> String.make width '-') column_widths)
      in

      Buffer.add_string output (Printf.sprintf "Table: %s\n" table);
      Buffer.add_string output (Printf.sprintf "%s\n" separator);
      Buffer.add_string output
        (Printf.sprintf "| %s |\n"
           (String.concat " | "
              (List.map2 (Printf.sprintf "%-*s") column_widths fields)));
      Buffer.add_string output (Printf.sprintf "%s\n" separator);

      filtered_scan#before_first;
      while filtered_scan#next do
        let row =
          List.map
            (fun field ->
              let value = filtered_scan#get_val ~field_name:field in
              Constant.to_string value)
            fields
        in
        Buffer.add_string output
          (Printf.sprintf "| %s |\n"
             (String.concat " | "
                (List.map2 (Printf.sprintf "%-*s") column_widths row)))
      done;
      Buffer.add_string output (Printf.sprintf "%s\n" separator);
      filtered_scan#close

let insert ~e ~op = 
  let layout = get_table_layout e (Insert.table_name op) in
  let tbl_scan =
    Table_scan.make ~tx:e.tx ~tbl_name:(Insert.table_name op) ~layout
  in
  tbl_scan#insert;
  List.iter2
    (fun field value ->
      match value with
      | Constant.Integer i -> tbl_scan#set_int32 ~field_name:field ~value:i
      | Constant.String s -> tbl_scan#set_string ~field_name:field ~value:s)
    (Insert.fields op) (Insert.values op);
    tbl_scan#close

let delete ~e ~op = 
  let layout = get_table_layout e (Delete.table_name op) in
  let tbl_scan =
    Table_scan.make ~tx:e.tx ~tbl_name:(Delete.table_name op) ~layout
  in
  let select = new Select_scan.t tbl_scan (Delete.predicate op) in
  select#before_first;
  while select#next do
    select#delete
  done;
  select#close

let update ~e ~op = 
  let layout = get_table_layout e (Update.table_name op) in
  let tbl_scan =
  Table_scan.make ~tx:e.tx ~tbl_name:(Update.table_name op) ~layout
  in
  let select = new Select_scan.t tbl_scan (Update.predicate op) in
  select#before_first;
  while select#next do
  let expr = Update.new_value op in
  match expr with
  | Scans__Expression.Const c ->
  select#set_val ~field_name:(Update.field_name op) ~value:c
  | Scans__Expression.FieldName _ ->
  failwith "Field references not yet supported in UPDATE SET"
  done;
  select#close

let create_table ~e ~op = 
  let table_mgr = Table_manager.make ~is_new:false ~tx:e.tx in
  Table_manager.create_table ~table_mgr
    ~tbl_name:(Create_table.table_name op)
    ~schema:(Create_table.new_schema op)
    ~tx:e.tx

let execute ~e ~query ?(output = Buffer.create 256) () =
  match query with
  | Query.Select op ->
      select ~e ~output ~op
  | Query.Insert op ->
      insert ~e ~op
  | Query.Delete op ->
      delete ~e ~op
  | Query.Update op ->
      update ~e ~op
  | Query.CreateIndex _ ->
      failwith "Index creation not yet implemented"
  | Query.CreateTable op ->
      create_table ~e ~op 




