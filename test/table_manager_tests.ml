module To_test = struct
  open File
  module Schema = Record_page__Schema
  module Layout = Record_page__Layout

  let tbl_mgr1 () =
    let file_manager =
      File_manager.make ~db_dirname:"tmp_tablemanager_test" ~block_size:1024
    in
    let log_manager = Log_manager.make ~file_manager ~log_file:"tmp_tablemanager_logs" in
    let buffer_manager =
      Buffer_manager.make ~file_manager ~log_manager ~num_buffers:8 ()
    in
    let tx = Transaction.make ~file_manager ~log_manager ~buffer_manager in
    let table_mgr = Table_manager.make ~is_new:true ~tx in
    let schema1 = Schema.make () in
    Schema.add_int_field schema1 "A";
    Schema.add_string_field schema1 "B" 9;
    Table_manager.create_table ~table_mgr ~tbl_name:"MyTable" ~schema:schema1
      ~tx;
    let layout =
      Table_manager.get_layout ~table_mgr ~tbl_name:"tablecatalog" ~tx
    in
    let slot_size = layout.slot_size in
    let schema2 = layout.schema in
    Printf.printf "table has slot size %d\n" slot_size;
    Printf.printf "table has the following fields\n";
    List.iter
      (fun field_name ->
        let typ = Schema.get_type schema2 field_name in
        let typ_repr =
          match typ with
          | Record_page__Type.Integer -> "int"
          | Record_page__Type.Varchar ->
              let len = Schema.get_length schema2 field_name in
              Printf.sprintf "VarChar(%d)" len
        in
        Printf.printf "%s : %s\n" field_name typ_repr)
      schema2.fields;
    Transaction.commit tx;
    "todo"
end

let test_tbl_mgr1 () =
  Alcotest.(check string)
    "same string" "<UPDATE INT 15 fname, 1 255 15>" (To_test.tbl_mgr1 ())

let all_tests () = [ Alcotest.test_case "create int logs" `Quick test_tbl_mgr1 ]
