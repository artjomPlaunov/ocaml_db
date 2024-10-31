module To_test = struct
  open File
  module Schema = Record_page__Schema
  module Layout = Record_page__Layout

  type record_page_test_env = {
    file_manager: File_manager.t;
    log_manager: Log_manager.t;
    buffer_manager: Buffer_manager.t;
  }

  let setup_record_page_env ~test_name ~block_size ~num_buffers =
    let db_name = "tmp_" ^ test_name in
    let file_manager = File_manager.make ~db_dirname:db_name ~block_size in
    let log_manager = Log_manager.make ~file_manager ~log_file:(db_name ^ "_logs") in
    let buffer_manager = Buffer_manager.make ~file_manager ~log_manager ~num_buffers () in
    { file_manager; log_manager; buffer_manager }

  let test_record_page0 () =
    let env = setup_record_page_env ~test_name:"recordpage_test0" ~block_size:1024 ~num_buffers:8 in
    let tx = Transaction.make ~file_manager:env.file_manager ~log_manager:env.log_manager ~buffer_manager:env.buffer_manager in
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
      output := !output ^ Printf.sprintf "inserting into slot %d: {%d, %s}\n" !slot !slot rec_str;
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
    "string equality" "A has offset 4\nB has offset 8\nFilling the page with random records\ninserting into slot 0: {0, rec0}\ninserting into slot 1: {1, rec1}\ninserting into slot 2: {2, rec2}\ninserting into slot 3: {3, rec3}\ninserting into slot 4: {4, rec4}\ninserting into slot 5: {5, rec5}\ninserting into slot 6: {6, rec6}\ninserting into slot 7: {7, rec7}\ninserting into slot 8: {8, rec8}\ninserting into slot 9: {9, rec9}\ninserting into slot 10: {10, rec10}\ninserting into slot 11: {11, rec11}\ninserting into slot 12: {12, rec12}\ninserting into slot 13: {13, rec13}\ninserting into slot 14: {14, rec14}\ninserting into slot 15: {15, rec15}\ninserting into slot 16: {16, rec16}\ninserting into slot 17: {17, rec17}\ninserting into slot 18: {18, rec18}\ninserting into slot 19: {19, rec19}\ninserting into slot 20: {20, rec20}\ninserting into slot 21: {21, rec21}\ninserting into slot 22: {22, rec22}\ninserting into slot 23: {23, rec23}\ninserting into slot 24: {24, rec24}\ninserting into slot 25: {25, rec25}\ninserting into slot 26: {26, rec26}\ninserting into slot 27: {27, rec27}\ninserting into slot 28: {28, rec28}\ninserting into slot 29: {29, rec29}\ninserting into slot 30: {30, rec30}\ninserting into slot 31: {31, rec31}\ninserting into slot 32: {32, rec32}\ninserting into slot 33: {33, rec33}\ninserting into slot 34: {34, rec34}\ninserting into slot 35: {35, rec35}\ninserting into slot 36: {36, rec36}\ninserting into slot 37: {37, rec37}\ninserting into slot 38: {38, rec38}\ninserting into slot 39: {39, rec39}\ninserting into slot 40: {40, rec40}\ninserting into slot 41: {41, rec41}\ninserting into slot 42: {42, rec42}\ninserting into slot 43: {43, rec43}\ninserting into slot 44: {44, rec44}\ninserting into slot 45: {45, rec45}\ninserting into slot 46: {46, rec46}\ninserting into slot 47: {47, rec47}\nDeleting these records with A vals < 25.\nslot 0: {0, rec0}\nslot 1: {1, rec1}\nslot 2: {2, rec2}\nslot 3: {3, rec3}\nslot 4: {4, rec4}\nslot 5: {5, rec5}\nslot 6: {6, rec6}\nslot 7: {7, rec7}\nslot 8: {8, rec8}\nslot 9: {9, rec9}\nslot 10: {10, rec10}\nslot 11: {11, rec11}\nslot 12: {12, rec12}\nslot 13: {13, rec13}\nslot 14: {14, rec14}\nslot 15: {15, rec15}\nslot 16: {16, rec16}\nslot 17: {17, rec17}\nslot 18: {18, rec18}\nslot 19: {19, rec19}\nslot 20: {20, rec20}\nslot 21: {21, rec21}\nslot 22: {22, rec22}\nslot 23: {23, rec23}\nslot 24: {24, rec24}\nRemaining records:\nslot 25: {25, rec25}\nslot 26: {26, rec26}\nslot 27: {27, rec27}\nslot 28: {28, rec28}\nslot 29: {29, rec29}\nslot 30: {30, rec30}\nslot 31: {31, rec31}\nslot 32: {32, rec32}\nslot 33: {33, rec33}\nslot 34: {34, rec34}\nslot 35: {35, rec35}\nslot 36: {36, rec36}\nslot 37: {37, rec37}\nslot 38: {38, rec38}\nslot 39: {39, rec39}\nslot 40: {40, rec40}\nslot 41: {41, rec41}\nslot 42: {42, rec42}\nslot 43: {43, rec43}\nslot 44: {44, rec44}\nslot 45: {45, rec45}\nslot 46: {46, rec46}\nslot 47: {47, rec47}\n"
    (To_test.test_record_page0 ())

let all_tests () =
  [ Alcotest.test_case "record page test0" `Quick test_record_page0 ]
