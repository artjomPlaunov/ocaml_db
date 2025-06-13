module To_test = struct
  open File_manager
  module Schema = Record_page.Schema
  module Layout = Record_page.Layout

  type record_page_test_env = {
    file_manager : File_manager.t;
    log_manager : Log_manager.t;
    buffer_manager : Buffer_manager.t;
  }

  let setup_record_page_env ~test_name ~block_size ~num_buffers =
    let db_name = "tmp_" ^ test_name in
    let file_manager = File_manager.make ~db_dirname:db_name ~block_size in
    let log_manager =
      Log_manager.make ~file_manager ~log_file:(db_name ^ "_logs")
    in
    let buffer_manager =
      Buffer_manager.make ~file_manager ~log_manager ~num_buffers ()
    in
    { file_manager; log_manager; buffer_manager }

  let test_record_page0 () =
    let env =
      setup_record_page_env ~test_name:"recordpage_test0" ~block_size:1024
        ~num_buffers:8
    in
    let tx =
      Transaction.make ~file_manager:env.file_manager
        ~log_manager:env.log_manager ~buffer_manager:env.buffer_manager
    in
    let schema = Schema.make () in
    Schema.add_int_field schema "A";
    Schema.add_string_field schema "B" 9;
    let layout = Layout.make schema in
    let output = ref "" in
    let iter_f field_name =
      let offset = Layout.get_offset layout field_name in
      output := !output ^ Printf.sprintf "%s has offset %d\n" field_name offset
    in
    List.iter iter_f (Schema.fields (Layout.get_schema layout));
    let block = Transaction.append ~tx ~filename:"testfile" in
    Transaction.pin ~tx ~block;
    let rec_page = Record_page.make tx block layout in
    Record_page.format rec_page;
    output := !output ^ "Filling the page with random records\n";
    let slot = ref (Record_page.insert_after rec_page (-1)) in
    while !slot >= 0 do
      Record_page.set_int32 rec_page !slot "A" (Int32.of_int !slot);
      let rec_str = "rec" ^ string_of_int !slot in
      Record_page.set_string rec_page !slot "B" rec_str;
      output :=
        !output
        ^ Printf.sprintf "inserting into slot %d: {%d, %s}\n" !slot !slot
            rec_str;
      slot := Record_page.insert_after rec_page !slot
    done;
    output := !output ^ "Deleting these records with A vals < 25.\n";
    let slot = ref (Record_page.next_after rec_page (-1)) in
    while !slot >= 0 do
      let a = Int32.to_int (Record_page.get_int32 rec_page !slot "A") in
      let b = Record_page.get_string rec_page !slot "B" in
      if a < 25 then (
        output := !output ^ Printf.sprintf "slot %d: {%d, %s}\n" !slot a b;
        Record_page.delete rec_page !slot);
      slot := Record_page.next_after rec_page !slot
    done;
    output := !output ^ "Remaining records:\n";
    let slot = ref (Record_page.next_after rec_page (-1)) in
    while !slot >= 0 do
      let a = Int32.to_int (Record_page.get_int32 rec_page !slot "A") in
      let b = Record_page.get_string rec_page !slot "B" in
      output := !output ^ Printf.sprintf "slot %d: {%d, %s}\n" !slot a b;
      slot := Record_page.next_after rec_page !slot
    done;
    Transaction.unpin ~tx ~block;
    Transaction.commit tx;
    !output
end

let test_record_page0 () =
  Alcotest.(check string)
    "string equality"
    "A has offset 4\n\
     B has offset 8\n\
     Filling the page with random records\n\
     inserting into slot 0: {0, rec0}\n\
     inserting into slot 1: {1, rec1}\n\
     inserting into slot 2: {2, rec2}\n\
     inserting into slot 3: {3, rec3}\n\
     inserting into slot 4: {4, rec4}\n\
     inserting into slot 5: {5, rec5}\n\
     inserting into slot 6: {6, rec6}\n\
     inserting into slot 7: {7, rec7}\n\
     inserting into slot 8: {8, rec8}\n\
     inserting into slot 9: {9, rec9}\n\
     inserting into slot 10: {10, rec10}\n\
     inserting into slot 11: {11, rec11}\n\
     inserting into slot 12: {12, rec12}\n\
     inserting into slot 13: {13, rec13}\n\
     inserting into slot 14: {14, rec14}\n\
     inserting into slot 15: {15, rec15}\n\
     inserting into slot 16: {16, rec16}\n\
     inserting into slot 17: {17, rec17}\n\
     inserting into slot 18: {18, rec18}\n\
     inserting into slot 19: {19, rec19}\n\
     inserting into slot 20: {20, rec20}\n\
     inserting into slot 21: {21, rec21}\n\
     inserting into slot 22: {22, rec22}\n\
     inserting into slot 23: {23, rec23}\n\
     inserting into slot 24: {24, rec24}\n\
     inserting into slot 25: {25, rec25}\n\
     inserting into slot 26: {26, rec26}\n\
     inserting into slot 27: {27, rec27}\n\
     inserting into slot 28: {28, rec28}\n\
     inserting into slot 29: {29, rec29}\n\
     inserting into slot 30: {30, rec30}\n\
     inserting into slot 31: {31, rec31}\n\
     inserting into slot 32: {32, rec32}\n\
     inserting into slot 33: {33, rec33}\n\
     inserting into slot 34: {34, rec34}\n\
     inserting into slot 35: {35, rec35}\n\
     inserting into slot 36: {36, rec36}\n\
     inserting into slot 37: {37, rec37}\n\
     inserting into slot 38: {38, rec38}\n\
     inserting into slot 39: {39, rec39}\n\
     inserting into slot 40: {40, rec40}\n\
     inserting into slot 41: {41, rec41}\n\
     inserting into slot 42: {42, rec42}\n\
     inserting into slot 43: {43, rec43}\n\
     inserting into slot 44: {44, rec44}\n\
     inserting into slot 45: {45, rec45}\n\
     inserting into slot 46: {46, rec46}\n\
     inserting into slot 47: {47, rec47}\n\
     Deleting these records with A vals < 25.\n\
     slot 0: {0, rec0}\n\
     slot 1: {1, rec1}\n\
     slot 2: {2, rec2}\n\
     slot 3: {3, rec3}\n\
     slot 4: {4, rec4}\n\
     slot 5: {5, rec5}\n\
     slot 6: {6, rec6}\n\
     slot 7: {7, rec7}\n\
     slot 8: {8, rec8}\n\
     slot 9: {9, rec9}\n\
     slot 10: {10, rec10}\n\
     slot 11: {11, rec11}\n\
     slot 12: {12, rec12}\n\
     slot 13: {13, rec13}\n\
     slot 14: {14, rec14}\n\
     slot 15: {15, rec15}\n\
     slot 16: {16, rec16}\n\
     slot 17: {17, rec17}\n\
     slot 18: {18, rec18}\n\
     slot 19: {19, rec19}\n\
     slot 20: {20, rec20}\n\
     slot 21: {21, rec21}\n\
     slot 22: {22, rec22}\n\
     slot 23: {23, rec23}\n\
     slot 24: {24, rec24}\n\
     Remaining records:\n\
     slot 25: {25, rec25}\n\
     slot 26: {26, rec26}\n\
     slot 27: {27, rec27}\n\
     slot 28: {28, rec28}\n\
     slot 29: {29, rec29}\n\
     slot 30: {30, rec30}\n\
     slot 31: {31, rec31}\n\
     slot 32: {32, rec32}\n\
     slot 33: {33, rec33}\n\
     slot 34: {34, rec34}\n\
     slot 35: {35, rec35}\n\
     slot 36: {36, rec36}\n\
     slot 37: {37, rec37}\n\
     slot 38: {38, rec38}\n\
     slot 39: {39, rec39}\n\
     slot 40: {40, rec40}\n\
     slot 41: {41, rec41}\n\
     slot 42: {42, rec42}\n\
     slot 43: {43, rec43}\n\
     slot 44: {44, rec44}\n\
     slot 45: {45, rec45}\n\
     slot 46: {46, rec46}\n\
     slot 47: {47, rec47}\n"
    (To_test.test_record_page0 ())

let all_tests () =
  [ Alcotest.test_case "record page test0" `Quick test_record_page0 ]
