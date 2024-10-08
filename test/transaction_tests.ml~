module To_test = struct
  open File

  let test_transaction1 () =
    let fm =
      File_manager.make ~db_dirname:"transaction_tests" ~block_size:1024
    in
    let lm = Log_manager.make ~file_manager:fm ~log_file:"transaction_logs" in
    let bm =
      Buffer_manager.make ~file_manager:fm ~log_manager:lm ~num_buffers:8 ()
    in
    let tx1 =
      Transaction.make ~file_manager:fm ~log_manager:lm ~buffer_manager:bm
    in
    let block = Block_id.make ~filename:"testfile" ~block_num:1 in
    Transaction.pin ~tx:tx1 ~block;
    Transaction.set_int ~tx:tx1 ~block ~offset:80 ~value:(Int32.of_int 1)
      ~to_log:false;
    Transaction.set_string ~tx:tx1 ~block ~offset:40 ~value:"one" ~to_log:false;
    Transaction.commit tx1;
    let tx2 =
      Transaction.make ~file_manager:fm ~log_manager:lm ~buffer_manager:bm
    in
    Transaction.pin ~tx:tx2 ~block;
    let ival = Transaction.get_int32 ~tx:tx2 ~block ~offset:80 in
    let ival = Int32.to_int ival in
    let sval = Transaction.get_string ~tx:tx2 ~block ~offset:40 in
    Printf.printf "initial value at location 80 = %d\n" ival;
    Printf.printf "initial value at location 40 = %s\n" sval;
    ""
end

let test_transaction1 () =
  Alcotest.(check string)
    "string equality" "lolz"
    (To_test.test_transaction1 ())

let all_tests () =
  [ Alcotest.test_case "transaction test" `Quick test_transaction1 ]
