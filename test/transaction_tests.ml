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
    let fm =
      File_manager.make ~db_dirname:"transaction_tests0" ~block_size:1024
    in
    let lm = Log_manager.make ~file_manager:fm ~log_file:"transaction_logs" in
    let bm =
      Buffer_manager.make ~file_manager:fm ~log_manager:lm ~num_buffers:8 ()
    in
    let tx =
      Transaction.make ~file_manager:fm ~log_manager:lm ~buffer_manager:bm
    in
    let block = Block_id.make ~filename:"testfile" ~block_num:1 in
    Transaction.pin ~tx ~block;
    Transaction.set_int ~tx ~block ~offset:80 ~value:(Int32.of_int 255)
      ~to_log:true;
    Transaction.commit tx;
    get_logs lm

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
      ~to_log:true;
    Transaction.set_string ~tx:tx1 ~block ~offset:40 ~value:"one" ~to_log:true;
    Transaction.commit tx1;
    let tx2 =
      Transaction.make ~file_manager:fm ~log_manager:lm ~buffer_manager:bm
    in
    Transaction.pin ~tx:tx2 ~block;
    let ival = Transaction.get_int32 ~tx:tx2 ~block ~offset:80 in
    let ival = Int32.to_int ival in
    let sval = Transaction.get_string ~tx:tx2 ~block ~offset:40 in
    (*Printf.printf "initial value at location 80 = %d\n" ival;
      Printf.printf "initial value at location 40 = %s\n" sval;*)
    let newival = ival + 1 in
    let newsval = sval ^ "!" in
    Transaction.set_int ~tx:tx2 ~block ~offset:80 ~value:(Int32.of_int newival)
      ~to_log:true;
    Transaction.set_string ~tx:tx2 ~block ~offset:40 ~value:newsval ~to_log:true;
    Transaction.commit tx2;
    let tx3 =
      Transaction.make ~file_manager:fm ~log_manager:lm ~buffer_manager:bm
    in
    Transaction.pin ~tx:tx3 ~block;
    let ival = Transaction.get_int32 ~tx:tx3 ~block ~offset:80 in
    let ival = Int32.to_int ival in
    let sval = Transaction.get_string ~tx:tx3 ~block ~offset:40 in
    (* Printf.printf "new value at location 80 = %d\n" ival;
       Printf.printf "new value at location 40 = %s\n" sval;*)
    Transaction.set_int ~tx:tx3 ~block ~offset:80 ~value:(Int32.of_int 9999)
      ~to_log:true;
    let ival = Transaction.get_int32 ~tx:tx3 ~block ~offset:80 in
    let ival = Int32.to_int ival in
    (*    Printf.printf "pre-rollback value at location 80 = %d\n" ival;*)
    Transaction.rollback tx3;

    let tx4 =
      Transaction.make ~file_manager:fm ~log_manager:lm ~buffer_manager:bm
    in
    Transaction.pin ~tx:tx4 ~block;
    let ival = Transaction.get_int32 ~tx:tx4 ~block ~offset:80 in
    let ival = Int32.to_int ival in
    (*    Printf.printf "post-rollback at location 80 = %d\n" ival;*)
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
end

let test_transaction0 () =
  Alcotest.(check string)
    "string equality" "<START 1><UPDATE INT 1 testfile, 1 80 255><COMMIT 1>"
    (To_test.test_transaction0 ())

let test_transaction1 () =
  Alcotest.(check string)
    "string equality"
    "<START 2><UPDATE INT 2 testfile, 1 80 1><UPDATE STRING 2 testfile, 1 40 \
     one><COMMIT 2><START 3><UPDATE INT 3 testfile, 1 80 2><UPDATE STRING 3 \
     testfile, 1 40 one!><COMMIT 3><START 4><UPDATE INT 4 testfile, 1 80 \
     9999><ROLLBACK 4><START 5>"
    (To_test.test_transaction1 ())

let all_tests () =
  [
    Alcotest.test_case "transaction test0" `Quick test_transaction0;
    Alcotest.test_case "transaction test1" `Quick test_transaction1;
  ]
