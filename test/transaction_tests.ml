module To_test = struct
  open File

  let get_logs lm =
    let iterator = Log_manager.get_iterator lm in
    let s = "" in
    let rec iterate_records iterator s1 =
      if Log_manager__Log_iterator.has_next iterator then
        let bytes = Log_manager__Log_iterator.next iterator in
        let next_rec = Log_record.make ~bytes in
        let s2 = Printf.sprintf "%s" (Log_record.to_string next_rec) in
        iterate_records iterator s1 ^ s2
      else s1
    in
    iterate_records iterator s

  let test_transaction0 () =
    let env = Test_utils.make_transaction_test_env ~db_name:"transaction_tests0" ~block_size:1024 ~num_buffers:8 in
    let tx = Transaction.make ~file_manager:env.file_manager ~log_manager:env.log_manager ~buffer_manager:env.buffer_manager in
    let block = Block_id.make ~filename:"testfile" ~block_num:1 in
    Transaction.pin ~tx ~block;
    Transaction.set_int ~tx ~block ~offset:80 ~value:(Int32.of_int 255) ~to_log:true;
    Transaction.commit tx;
    Test_utils.get_logs env.log_manager

  let test_transaction1 () =
    let env = Test_utils.make_transaction_test_env ~db_name:"transaction_tests" ~block_size:1024 ~num_buffers:8 in
    let tx1 = Transaction.make ~file_manager:env.file_manager ~log_manager:env.log_manager ~buffer_manager:env.buffer_manager in
    let block = Block_id.make ~filename:"testfile" ~block_num:1 in
    Transaction.pin ~tx:tx1 ~block;
    Transaction.set_int ~tx:tx1 ~block ~offset:80 ~value:(Int32.of_int 1) ~to_log:true;
    Transaction.set_string ~tx:tx1 ~block ~offset:40 ~value:"one" ~to_log:true;
    Transaction.commit tx1;

    let tx2 = Transaction.make ~file_manager:env.file_manager ~log_manager:env.log_manager ~buffer_manager:env.buffer_manager in
    Transaction.pin ~tx:tx2 ~block;
    let ival = Transaction.get_int32 ~tx:tx2 ~block ~offset:80 |> Int32.to_int in
    let sval = Transaction.get_string ~tx:tx2 ~block ~offset:40 in
    let newival = ival + 1 in
    let newsval = sval ^ "!" in
    Transaction.set_int ~tx:tx2 ~block ~offset:80 ~value:(Int32.of_int newival) ~to_log:true;
    Transaction.set_string ~tx:tx2 ~block ~offset:40 ~value:newsval ~to_log:true;
    Transaction.commit tx2;

    let tx3 = Transaction.make ~file_manager:env.file_manager ~log_manager:env.log_manager ~buffer_manager:env.buffer_manager in
    Transaction.pin ~tx:tx3 ~block;
    Transaction.set_int ~tx:tx3 ~block ~offset:80 ~value:(Int32.of_int 9999) ~to_log:true;
    Transaction.rollback tx3;

    let tx4 = Transaction.make ~file_manager:env.file_manager ~log_manager:env.log_manager ~buffer_manager:env.buffer_manager in
    Transaction.pin ~tx:tx4 ~block;
    Test_utils.get_logs env.log_manager
end

let test_transaction0 () =
  Alcotest.(check string)
    "string equality" "<COMMIT 1>\n<UPDATE INT 1 testfile, 1 80 255>\n<START 1>\n"
    (To_test.test_transaction0 ())

let test_transaction1 () =
  Alcotest.(check string)
    "string equality"
    "<START 5>\n<ROLLBACK 4>\n<UPDATE INT 4 testfile, 1 80 9999>\n<START 4>\n<COMMIT 3>\n<UPDATE STRING 3 testfile, 1 40 one!>\n<UPDATE INT 3 testfile, 1 80 2>\n<START 3>\n<COMMIT 2>\n<UPDATE STRING 2 testfile, 1 40 one>\n<UPDATE INT 2 testfile, 1 80 1>\n<START 2>\n"
    (To_test.test_transaction1 ())

let all_tests () =
  [
    Alcotest.test_case "transaction test0" `Quick test_transaction0;
    Alcotest.test_case "transaction test1" `Quick test_transaction1;
  ]
