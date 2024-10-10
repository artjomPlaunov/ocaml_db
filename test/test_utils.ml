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
