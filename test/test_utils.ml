open File
open Log_manager
open Log_record
open Buffer_manager
open Transaction
open Predicate
open Predicate__Expression
open Predicate__Term
open Constant

(* Returns true if files have no diff.*)

let no_diff_aux file1 file2 =
  let command = Printf.sprintf "diff -q %s %s" file1 file2 in
  let result = Sys.command command in
  result = 0 (* Return true if the files are identical (exit status 0) *)

let no_diff generated_file reference_file =
  let current_dir = Sys.getcwd () in
  (* Combine the current directory with the relative path *)
  let full_path = Filename.concat current_dir generated_file in
  no_diff_aux full_path ("../../../test/" ^ reference_file)

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

(* Common test environment setup *)
type test_env = {
  file_manager: File_manager.t;
  log_manager: Log_manager.t;
  buffer_manager: Buffer_manager.t;
  transaction: Transaction.t;
  output: Buffer.t;
}

let make_test_env ~db_name = 
  let file_manager = File_manager.make ~db_dirname:("tmp_" ^ db_name) ~block_size:400 in
  let log_manager = Log_manager.make ~file_manager ~log_file:("tmp_" ^ db_name ^ "_logs") in
  let buffer_manager = Buffer_manager.make ~file_manager ~log_manager ~num_buffers:8 () in
  let transaction = Transaction.make ~file_manager ~log_manager ~buffer_manager in
  let output = Buffer.create 1024 in
  { file_manager; log_manager; buffer_manager; transaction; output }

(* Common table operations *)
let print_table_contents ~output ~name ~scan ~get_fields =
  Buffer.add_string output (Printf.sprintf "\n%s:\n" name);
  scan#before_first;
  while scan#next do
    Buffer.add_string output (get_fields scan)
  done

let cleanup_test_env scan env =
  scan#close;
  Transaction.commit env.transaction

(* Helper for creating predicates *)
let make_predicate field_name value =
  let lhs = Predicate__Expression.make_field_name field_name in
  let rhs = Predicate__Expression.make_const (Constant.Integer (Int32.of_int value)) in
  let term = Predicate__Term.make lhs rhs in
  Predicate.make term

(* Format record as string *)
let format_record fields =
  let field_strs = List.map (fun (name, value) -> Printf.sprintf "%s=%s" name value) fields in
  "{" ^ String.concat ", " field_strs ^ "}"

let write_test_output ~test_name ~output ~db_name =
  let base_dir = "tmp" in
  let dir = Filename.concat base_dir db_name in
  (try Unix.mkdir base_dir 0o777 with Unix.Unix_error(Unix.EEXIST, _, _) -> ());
  (try Unix.mkdir dir 0o777 with Unix.Unix_error(Unix.EEXIST, _, _) -> ());
  let output_file = Filename.concat dir (test_name ^ "_output.txt") in
  let oc = open_out output_file in
  output_string oc output;
  close_out oc

(* Buffer test environment type *)
type buffer_test_env = {
  file_manager: File_manager.t;
  log_manager: Log_manager.t;
  buffer_manager: Buffer_manager.t;
}

(* Basic setup for buffer/file tests *)
let make_buffer_test_env ~db_name ~num_buffers = 
  let file_manager = File_manager.make ~db_dirname:("tmp_" ^ db_name) ~block_size:512 in
  let log_manager = Log_manager.make ~file_manager ~log_file:("tmp_" ^ db_name ^ "_logs") in
  let buffer_manager = Buffer_manager.make ~file_manager ~log_manager ~num_buffers () in
  { file_manager; log_manager; buffer_manager }

(* Setup for table manager tests *)
let make_table_test_env ~db_name = 
  let env = make_test_env ~db_name in
  let table_mgr = Table_manager.make ~is_new:true ~tx:env.transaction in
  (env, table_mgr)

(* Setup for parser tests *)
let make_parser_test_env ~db_name =
  let env = make_test_env ~db_name in
  let table_mgr = Table_manager.make ~is_new:true ~tx:env.transaction in
  (env, table_mgr)

(* Cleanup functions *)
let cleanup_buffer_test_env env =
  Buffer_manager.flush_all env.buffer_manager 0

let cleanup_table_test_env env table_mgr =
  Transaction.commit env.transaction

(* Log record test environment type *)
type log_record_test_env = {
  file_manager: File_manager.t;
  log_manager: Log_manager.t;
}

(* Setup for log record tests *)
let make_log_record_test_env ~db_name = 
  let file_manager = File_manager.make ~db_dirname:("tmp_" ^ db_name) ~block_size:500 in
  let log_manager = Log_manager.make ~file_manager ~log_file:("tmp_" ^ db_name ^ "_logs") in
  { file_manager; log_manager }

(* Helper to get logs as string *)
let get_logs log_manager =
  let iterator = Log_manager.get_iterator log_manager in
  let rec iterate_records iterator acc =
    if Log_manager__Log_iterator.has_next iterator then
      let bytes = Log_manager__Log_iterator.next iterator in
      let next_rec = Log_record.make ~bytes in
      let log_str = Log_record.to_string next_rec ^ "\n" in
      iterate_records iterator (acc ^ log_str)
    else 
      acc
  in
  iterate_records iterator ""

let make_transaction_test_env ~db_name ~block_size ~num_buffers =
  let file_manager = File_manager.make ~db_dirname:("tmp_" ^ db_name) ~block_size in
  let log_manager = Log_manager.make ~file_manager ~log_file:("tmp_" ^ db_name ^ "_logs") in
  let buffer_manager = Buffer_manager.make ~file_manager ~log_manager ~num_buffers () in
  { file_manager; log_manager; buffer_manager }