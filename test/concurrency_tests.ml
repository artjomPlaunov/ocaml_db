module To_test = struct
  open File

  let f_a fm lm bm () =
    let tx =
      Transaction.make ~file_manager:fm ~log_manager:lm ~buffer_manager:bm
    in
    let block = Block_id.make ~filename:"testfile" ~block_num:0 in
    Transaction.pin ~tx ~block;
    Transaction.set_int ~tx ~block ~offset:0 ~value:(Int32.of_int 677)
      ~to_log:true;
    Transaction.commit tx

  let transaction_a fm lm bm () =
    let tx =
      Transaction.make ~file_manager:fm ~log_manager:lm ~buffer_manager:bm
    in
    let block = Block_id.make ~filename:"testfile" ~block_num:0 in
    Transaction.pin ~tx ~block;
    let x = Int32.to_int (Transaction.get_int32 ~tx ~block ~offset:0) in
    let result = Printf.sprintf "transaction A initial val: %d\n" x in
    Thread.delay 3.0;
    Transaction.set_int ~tx ~block ~offset:0
      ~value:(Int32.of_int (x + 1))
      ~to_log:true;
    Transaction.commit tx;
    result

  let transaction_b fm lm bm () =
    let tx =
      Transaction.make ~file_manager:fm ~log_manager:lm ~buffer_manager:bm
    in
    let block = Block_id.make ~filename:"testfile" ~block_num:0 in
    Transaction.pin ~tx ~block;
    let x = Int32.to_int (Transaction.get_int32 ~tx ~block ~offset:0) in
    let result = Printf.sprintf "transaction B initial val: %d\n" x in
    Transaction.set_int ~tx ~block ~offset:0
      ~value:(Int32.of_int (x + 1))
      ~to_log:true;
    Transaction.commit tx;
    result

  let read_tx fm lm bm =
    let tx =
      Transaction.make ~file_manager:fm ~log_manager:lm ~buffer_manager:bm
    in
    let block = Block_id.make ~filename:"testfile" ~block_num:0 in
    Transaction.pin ~tx ~block;
    let x = Transaction.get_int32 ~tx ~block ~offset:0 in
    Transaction.commit tx;
    Int32.to_int x

  let test_concurrency1 () =
    let fm =
      File_manager.make ~db_dirname:"tmp_concurrency_tests" ~block_size:400
    in
    let lm = Log_manager.make ~file_manager:fm ~log_file:"logfile" in
    let bm =
      Buffer_manager.make ~file_manager:fm ~log_manager:lm ~num_buffers:8 ()
    in
    let t1 = Thread.create (f_a fm lm bm) () in
    Thread.join t1;
    let old_val = read_tx fm lm bm in
    let initial_str = Printf.sprintf "Initial value: %d\n" old_val in
    let threadA = Thread.create (transaction_a fm lm bm) () in
    let threadB = Thread.create (transaction_b fm lm bm) () in
    let resultA = Thread.join threadA in
    let resultB = Thread.join threadB in
    let new_val = read_tx fm lm bm in
    let new_str = Printf.sprintf "New value: %d\n" new_val in
    initial_str ^ new_str
end

let test_concurrency1 () =
  Alcotest.(check string)
    "simple concurrency test"
    "Initial value: 677\nNew value: 678\n"
    (To_test.test_concurrency1 ())

let all_tests () =
  [ Alcotest.test_case "test concurrency" `Quick test_concurrency1 ]
