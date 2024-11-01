module To_test = struct
  open File
  module Schema = Record_page__Schema
  module Layout = Record_page__Layout

  let tbl_mgr_create_and_verify () =
    let db_name = "tmp_tablemanager_test_create" in
    let file_manager = File_manager.make ~db_dirname:db_name ~block_size:1024 in
    let log_manager = Log_manager.make ~file_manager ~log_file:(db_name ^ "_logs") in
    let buffer_manager = Buffer_manager.make ~file_manager ~log_manager ~num_buffers:8 () in
    let tx = Transaction.make ~file_manager ~log_manager ~buffer_manager in
    let table_mgr = Table_manager.make ~is_new:true ~tx in

    (* Create a new table *)
    let schema = Schema.make () in
    Schema.add_int_field schema "ID";
    Schema.add_string_field schema "Name" 20;
    Table_manager.create_table ~table_mgr ~tbl_name:"TestTable" ~schema ~tx;

    (* Retrieve and verify the layout *)
    let layout = Table_manager.get_layout ~table_mgr ~tbl_name:"TestTable" ~tx in
    let slot_size = layout.slot_size in
    let schema_retrieved = layout.schema in

    let output = Buffer.create 256 in
    Buffer.add_string output (Printf.sprintf "TestTable has slot size %d\n" slot_size);
    Buffer.add_string output "TestTable has the following fields\n";
    List.iter
      (fun field_name ->
        let typ = Schema.get_type schema_retrieved field_name in
        let typ_repr =
          match typ with
          | Record_page__Type.Integer -> "int"
          | Record_page__Type.Varchar ->
              let len = Schema.get_length schema_retrieved field_name in
              Printf.sprintf "VarChar(%d)" len
        in
        Buffer.add_string output (Printf.sprintf "%s : %s\n" field_name typ_repr))
      schema_retrieved.fields;
    Transaction.commit tx;
    Buffer.contents output

  let tbl_mgr_create_multiple_tables () =
    let db_name = "tmp_tablemanager_test_multiple" in
    let file_manager = File_manager.make ~db_dirname:db_name ~block_size:1024 in
    let log_manager = Log_manager.make ~file_manager ~log_file:(db_name ^ "_logs") in
    let buffer_manager = Buffer_manager.make ~file_manager ~log_manager ~num_buffers:8 () in
    let tx = Transaction.make ~file_manager ~log_manager ~buffer_manager in
    let table_mgr = Table_manager.make ~is_new:true ~tx in

    (* Create multiple tables *)
    let create_table name =
      let schema = Schema.make () in
      Schema.add_int_field schema "ID";
      Schema.add_string_field schema "Description" 50;
      Table_manager.create_table ~table_mgr ~tbl_name:name ~schema ~tx
    in
    create_table "Table1";
    create_table "Table2";

    (* Verify layouts for both tables *)
    let verify_table name =
      let layout = Table_manager.get_layout ~table_mgr ~tbl_name:name ~tx in
      let slot_size = layout.slot_size in
      let schema_retrieved = layout.schema in

      let output = Buffer.create 256 in
      Buffer.add_string output (Printf.sprintf "%s has slot size %d\n" name slot_size);
      Buffer.add_string output (Printf.sprintf "%s has the following fields\n" name);
      List.iter
        (fun field_name ->
          let typ = Schema.get_type schema_retrieved field_name in
          let typ_repr =
            match typ with
            | Record_page__Type.Integer -> "int"
            | Record_page__Type.Varchar ->
                let len = Schema.get_length schema_retrieved field_name in
                Printf.sprintf "VarChar(%d)" len
          in
          Buffer.add_string output (Printf.sprintf "%s : %s\n" field_name typ_repr))
        schema_retrieved.fields;
      Buffer.contents output
    in

    let output1 = verify_table "Table1" in
    let output2 = verify_table "Table2" in
    Transaction.commit tx;
    output1 ^ "\n" ^ output2
end

let tbl_mgr_create_and_verify_expected = "TestTable has slot size 32\nTestTable has the following fields\nID : int\nName : VarChar(20)\n"
let tbl_mgr_create_multiple_tables_expected = "Table1 has slot size 62\nTable1 has the following fields\nID : int\nDescription : VarChar(50)\n\nTable2 has slot size 62\nTable2 has the following fields\nID : int\nDescription : VarChar(50)\n"

let test_tbl_mgr_create_and_verify () =
  Alcotest.(check string)
    "table manager create and verify test"
    tbl_mgr_create_and_verify_expected
    (To_test.tbl_mgr_create_and_verify ())

let test_tbl_mgr_create_multiple_tables () =
  Alcotest.(check string)
    "table manager create multiple tables test"
    tbl_mgr_create_multiple_tables_expected
    (To_test.tbl_mgr_create_multiple_tables ())

let all_tests () = [ Alcotest.test_case "create int logs" `Quick test_tbl_mgr_create_and_verify; Alcotest.test_case "create multiple tables" `Quick test_tbl_mgr_create_multiple_tables ]
