open Parser
open Table_manager

type interpreter_env = {
  tx: Transaction.t;
  buffer_manager: Buffer_manager.t;
}

let make_env ~tx ~buffer_manager = {
  tx;
  buffer_manager;
}

class sql_interpreter env = object(self)
  val tx = env.tx
  val buffer_manager = env.buffer_manager

  method private get_table_layout tbl_name =
    let table_mgr = Table_manager.make ~is_new:false ~tx in
    Table_manager.get_layout ~table_mgr ~tbl_name ~tx

  method private execute_select ~output data =
    match Select_data.tables data with
    | [] -> failwith "No tables specified in SELECT"
    | table :: _ ->
        let layout = self#get_table_layout table in
        let tbl_scan = Table_scan.make ~tx ~tbl_name:table ~layout in

        (* Apply WHERE clause if present *)
        let filtered_scan = match Select_data.predicate data with
          | Some pred -> new Select_scan.t tbl_scan pred
          | None -> tbl_scan
        in

        (* Output the results of the selection in a table format *)
        let fields = Select_data.fields data in
        let column_widths = List.map (fun _ -> 15) fields in
        let separator = String.concat "+" (List.map (fun width -> String.make width '-') column_widths) in

        Buffer.add_string output (Printf.sprintf "Table: %s\n" table);
        Buffer.add_string output (Printf.sprintf "%s\n" separator);
        Buffer.add_string output (Printf.sprintf "| %s |\n" 
          (String.concat " | " (List.map2 (Printf.sprintf "%-*s") column_widths fields)));
        Buffer.add_string output (Printf.sprintf "%s\n" separator);

        filtered_scan#before_first;
        while filtered_scan#next do
          let row = List.map (fun field ->
            let value = filtered_scan#get_val ~field_name:field in
            Constant.to_string value
          ) fields in
          Buffer.add_string output (Printf.sprintf "| %s |\n" 
            (String.concat " | " (List.map2 (Printf.sprintf "%-*s") column_widths row)))
        done;
        Buffer.add_string output (Printf.sprintf "%s\n" separator);
        filtered_scan#close

  method private execute_insert data =
    let layout = self#get_table_layout (Insert_data.table_name data) in
    let tbl_scan = Table_scan.make ~tx ~tbl_name:(Insert_data.table_name data) ~layout in
    tbl_scan#insert;
    List.iter2 (fun field value ->
      match value with
      | Constant.Integer i -> tbl_scan#set_int32 ~field_name:field ~value:i
      | Constant.String s -> tbl_scan#set_string ~field_name:field ~value:s
    ) (Insert_data.fields data) (Insert_data.values data);
    tbl_scan#close

  method private execute_delete data =
    let layout = self#get_table_layout (Delete_data.table_name data) in
    let tbl_scan = Table_scan.make ~tx ~tbl_name:(Delete_data.table_name data) ~layout in
    let select = new Select_scan.t tbl_scan (Delete_data.predicate data) in
    select#before_first;
    while select#next do
      select#delete
    done;
    select#close

  method private execute_modify data =
    let layout = self#get_table_layout (Modify_data.table_name data) in
    let tbl_scan = Table_scan.make ~tx ~tbl_name:(Modify_data.table_name data) ~layout in
    let select = new Select_scan.t tbl_scan (Modify_data.predicate data) in
    select#before_first;
    while select#next do
      let expr = Modify_data.new_value data in
      match expr with
      | Predicate__Expression.Const c -> select#set_val ~field_name:(Modify_data.field_name data) ~value:c
      | Predicate__Expression.FieldName _ -> failwith "Field references not yet supported in UPDATE SET"
    done;
    select#close

  method private execute_create_table data =
    let table_mgr = Table_manager.make ~is_new:false ~tx in
    Table_manager.create_table 
      ~table_mgr 
      ~tbl_name:(Create_table_data.table_name data)
      ~schema:(Create_table_data.new_schema data) 
      ~tx;
    Transaction.commit tx

  method execute ?(output=Buffer.create 256) query =
    match query with
    | Query_data.Select data -> 
        self#execute_select ~output data;
        output
    | Query_data.Insert data -> 
        self#execute_insert data;
        output
    | Query_data.Delete data -> 
        self#execute_delete data;
        output
    | Query_data.Modify data -> 
        self#execute_modify data;
        output
    | Query_data.CreateView _ -> 
        failwith "View creation not yet implemented"
    | Query_data.CreateIndex _ -> 
        failwith "Index creation not yet implemented"
    | Query_data.CreateTable data -> 
        self#execute_create_table data;
        output
end