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
  
  let f_a fm lm bm () =
    let tx = Transaction.make ~file_manager:fm ~log_manager:lm ~buffer_manager:bm in
    let block = Block_id.make ~filename:"testfile" ~block_num:0 in
    Transaction.pin ~tx ~block;
    Transaction.set_int ~tx ~block ~offset:0 ~value:(Int32.of_int 1) ~to_log:true;
    Transaction.commit tx

  let transaction_a fm lm bm () =
    let tx = Transaction.make ~file_manager:fm ~log_manager:lm ~buffer_manager:bm in
    let block = Block_id.make ~filename:"testfile" ~block_num:0 in
    Transaction.pin ~tx:tx ~block;
    let x = Int32.to_int (Transaction.get_int32 ~tx ~block ~offset:0) in
    (* Force the other thread to run by letting this thread sleep. Here we fetched 
       the same initial value the other thread will have fetched. *)
    Thread.delay 1.0;
    Transaction.set_int ~tx ~block ~offset:0 ~value:(Int32.of_int (x+1)) ~to_log:true;
    Transaction.commit tx

  let transaction_b fm lm bm () =
    let tx = Transaction.make ~file_manager:fm ~log_manager:lm ~buffer_manager:bm in
    let block = Block_id.make ~filename:"testfile" ~block_num:0 in
    Transaction.pin ~tx:tx ~block;
    let x = Int32.to_int (Transaction.get_int32 ~tx ~block ~offset:0) in
    Transaction.set_int ~tx ~block ~offset:0 ~value:(Int32.of_int (x+1)) ~to_log:true;
    Transaction.commit tx
  
  let read_tx fm lm bm =
    let tx = Transaction.make ~file_manager:fm ~log_manager:lm ~buffer_manager:bm in
    let block = Block_id.make ~filename:"testfile" ~block_num:0 in
    Transaction.pin ~tx ~block;
    let x = Transaction.get_int32 ~tx ~block ~offset:0 in
    Transaction.commit tx;
    Int32.to_int x

  (* Initial test written BEFORE writing the concurrency manager. 
     The point of this test is to play around with threads, and create a simple 
     scenario where two transactions concurrently executing create some inconsistent 
     state in the database. 

     Each transaction tries to increment a counter at the same location of the disk,
     which is initialized to 1. So we would expect the final result to be 3 if the 
     transactions were serial.

     However without a concurrency manager in place, it is possible that threadA 
     reads the initial value, and gets interrupted at which point threadB runs and also
     reads the same initial value of 1. Then both threads finish operation in whatever 
     order, and we get the result of 2 instead of 3. This interruption is forced in the 
     test by making transactionA sleep after fetching the initial value.
  *)
  let test_concurrency1 () =
    let fm = File_manager.make ~db_dirname:"concurrency_tests" ~block_size:400 in
    let lm = Log_manager.make ~file_manager:fm ~log_file:"logfile" in
    let bm = Buffer_manager.make ~file_manager:fm ~log_manager:lm ~num_buffers:8 () in
    let t1 = Thread.create (f_a fm lm bm) () in
    Thread.join t1;
    (* Initial value *)
    let old_val = read_tx fm lm bm in
    let threadA = Thread.create (transaction_a fm lm bm) () in
    let threadB = Thread.create (transaction_b fm lm bm) () in
    Thread.join threadA;
    Thread.join threadB;
    let new_val = read_tx fm lm bm in
    Printf.printf "new val: %d\n" new_val;
    get_logs lm

end

let test_concurrency1 () =
  Alcotest.(check string)
    "simple concurrency test" "lolz"
    (To_test.test_concurrency1 ())

let all_tests () =
  [
    Alcotest.test_case "test concurrency" `Quick
      test_concurrency1;
  ]
