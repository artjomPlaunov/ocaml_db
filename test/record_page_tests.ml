module To_test = struct
  open File
  module Schema = Record_page__Schema
  module Layout = Record_page__Layout

  let test_record_page0 () =
    let fm =
      File_manager.make ~db_dirname:"tmp_recordpage_test0" ~block_size:1024
    in
    let lm = Log_manager.make ~file_manager:fm ~log_file:"tmp_recordpage_logs" in
    let bm =
      Buffer_manager.make ~file_manager:fm ~log_manager:lm ~num_buffers:8 ()
    in
    let tx =
      Transaction.make ~file_manager:fm ~log_manager:lm ~buffer_manager:bm
    in
    let schema = Schema.make () in
    Schema.add_int_field schema "A";
    Schema.add_string_field schema "B" 9;
    let layout = Layout.make schema in
    let iter_f field_name =
      let offset = Layout.get_offset layout field_name in
      Printf.printf "%s has offset %d\n" field_name offset
    in
    List.iter iter_f (Schema.fields (Layout.get_schema layout));
    let block = Transaction.append ~tx ~filename:"testfile" in
    Transaction.pin ~tx ~block;
    let rec_page = Record_page.make tx block layout in
    Record_page.format rec_page;
    Printf.printf "Filling the page with random records\n";
    let slot = ref (Record_page.insert_after rec_page (-1)) in
    while !slot >= 0 do
      Record_page.set_int32 rec_page !slot "A" (Int32.of_int !slot);
      let rec_str = "rec" ^ string_of_int !slot in
      Record_page.set_string rec_page !slot "B" rec_str;
      Printf.printf "inserting into slot %d: {%d, %s}\n" !slot !slot rec_str;
      slot := Record_page.insert_after rec_page !slot
    done;
    Printf.printf "Deleting these records with A vals < 25.\n";
    let count = ref 0 in
    let slot = ref (Record_page.next_after rec_page (-1)) in
    while !slot >= 0 do
      let a = Int32.to_int (Record_page.get_int32 rec_page !slot "A") in
      let b = Record_page.get_string rec_page !slot "B" in
      if a < 25 then (
        count := !count + 1;
        Printf.printf "slot %d: {%d, %s}\n" !slot a b;
        Record_page.delete rec_page !slot);
      slot := Record_page.next_after rec_page !slot
    done;
    Printf.printf "Remaining records:\n";
    let slot = ref (Record_page.next_after rec_page (-1)) in
    while !slot >= 0 do
      let a = Int32.to_int (Record_page.get_int32 rec_page !slot "A") in
      let b = Record_page.get_string rec_page !slot "B" in
      Printf.printf "slot %d: {%d, %s}\n" !slot a b;
      slot := Record_page.next_after rec_page !slot
    done;
    Transaction.unpin ~tx ~block;
    Transaction.commit tx;
    ""
end

let test_record_page0 () =
  Alcotest.(check string)
    "string equality" "<START 1><UPDATE INT 1 testfile, 1 80 255><COMMIT 1>"
    (To_test.test_record_page0 ())

let all_tests () =
  [ Alcotest.test_case "record page test0" `Quick test_record_page0 ]
