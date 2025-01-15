module Test_utils = struct
  open File
  open Storage_manager

  let setup_test_env dir =
    let file_manager = File_manager.make ~db_dirname:dir ~block_size:40 in
    let storage_manager =
      Storage_manager.make ~file_manager ~storage_file:"test_btree"
    in
    storage_manager
end

module Btree_tests = struct
  open Btree
  open Test_utils

  let expected_output = " "

  let empty_btree_insert_leaf () =
    let storage_manager = setup_test_env "tmp_btree_insert_leaf" in
    let key_type = TVarchar 4 in
    let t = Btree.create storage_manager key_type in
    (* Empty tree.*)
    Btree.print_node t.root;
    let _ = Btree.insert_in_leaf t 1 (Btree.Varchar (String.make 4 'J')) 4 in
    let _ = Btree.insert_in_leaf t 1 (Btree.Varchar (String.make 4 'A')) 3 in
    let _ = Btree.insert_in_leaf t 1 (Btree.Varchar (String.make 4 'V')) 2 in
    let _ = Btree.insert_in_leaf t 1 (Btree.Varchar (String.make 4 'Q')) 1 in
    let _ = Btree.insert_in_leaf t 1 (Btree.Varchar (String.make 4 'S')) 5 in
    let _ = Btree.insert_in_leaf t 1 (Btree.Varchar (String.make 4 'Z')) 6 in
    let _ = Btree.insert_in_leaf t 1 (Btree.Varchar (String.make 4 'C')) 7 in
    let _ = Btree.insert_in_leaf t 1 (Btree.Varchar (String.make 4 'P')) 8 in
    Btree.print_node t.root;
    " "

  let insert_in_parent_root () =
    let dir = "tmp_btree_insert_parent_root" in
    let storage_manager = setup_test_env dir in
    let key_type = TVarchar 4 in

    let t = Btree.create storage_manager key_type in

    (* create another node, so we can insert a new key, pointer pair into the root we created above*)
    let key_ty = t.key in
    let block_size = File.File_manager.get_blocksize t.sm.file_manager in

    let empty = Btree.empty_node t in
    (* let empty_page = Btree.serialize empty block_size in
       let e1 = Storage_manager.append ~storage_manager ~page:empty_page in
       let p1 = File.Block_id.block_num e1 in
       let e2 = Storage_manager.append ~storage_manager ~page:empty_page in
       let p2 = File.Block_id.block_num e2 in
       let e3 = Storage_manager.append ~storage_manager ~page:empty_page in
       let p3 = File.Block_id.block_num e3 in
       let e4 = Storage_manager.append ~storage_manager ~page:empty_page in
       let p4 = File.Block_id.block_num e4 in
       let e5 = Storage_manager.append ~storage_manager ~page:empty_page in
       let p5 = File.Block_id.block_num e5 in


       let _ = Btree.insert_in_leaf t 1 (Btree.Varchar (String.make 4 'C')) 8 in
       let _ = Btree.insert_in_leaf t 1 (Btree.Varchar (String.make 4 'B')) 9 in
       Btree.insert_in_parent t t.root_num (Btree.Varchar (String.make 4 'D')) p1;
       Btree.insert_in_parent t t.root_num (Btree.Varchar (String.make 4 'P')) p2;
       Btree.insert_in_parent t 1 (Btree.Varchar (String.make 4 'F')) p3;
       Btree.insert_in_parent t 1 (Btree.Varchar (String.make 4 'E')) p4;
       Btree.insert_aux t t.root_num (Btree.Varchar (String.make 4 'D')) 7;
       Btree.insert_aux t t.root_num (Btree.Varchar (String.make 4 'G')) 7;
       Btree.insert_aux t t.root_num (Btree.Varchar (String.make 4 'H')) 7; *)
    Btree.insert t (Btree.Varchar (String.make 4 'A')) 9999;

    Btree.insert t (Btree.Varchar (String.make 4 'U')) 9999;
    Btree.insert t (Btree.Varchar (String.make 4 'C')) 9999;
    Btree.insert t (Btree.Varchar (String.make 4 'D')) 9999;

    Btree.insert t (Btree.Varchar (String.make 4 'S')) 9999;
    Btree.insert t (Btree.Varchar (String.make 4 'E')) 9999;

    Btree.insert t (Btree.Varchar (String.make 4 'Z')) 9999;
    Btree.insert t (Btree.Varchar (String.make 4 'L')) 9999;
    Btree.insert t (Btree.Varchar (String.make 4 'F')) 9999;
    Btree.insert t (Btree.Varchar (String.make 4 'G')) 9999;

    Btree.insert t (Btree.Varchar (String.make 4 'I')) 9999;
    Btree.insert t (Btree.Varchar (String.make 4 'J')) 9999;
    Btree.insert t (Btree.Varchar (String.make 4 'K')) 9999;

    Btree.insert t (Btree.Varchar (String.make 4 'M')) 9999;
    Btree.insert t (Btree.Varchar (String.make 4 'W')) 9999;
    Btree.insert t (Btree.Varchar (String.make 4 'X')) 9999;

    Btree.insert t (Btree.Varchar (String.make 4 'Q')) 9999;
    Btree.insert t (Btree.Varchar (String.make 4 'B')) 9999;
    Btree.insert t (Btree.Varchar (String.make 4 'N')) 9999;
    Btree.insert t (Btree.Varchar (String.make 4 'O')) 9999;
    Btree.insert t (Btree.Varchar (String.make 4 'P')) 9999;

    Btree.insert t (Btree.Varchar (String.make 4 'R')) 9999;

    Btree.insert t (Btree.Varchar (String.make 4 'T')) 9999;

    Btree.insert t (Btree.Varchar (String.make 4 'V')) 9999;

    Btree.insert t (Btree.Varchar (String.make 4 'Y')) 9999;
    Btree.insert t (Btree.Varchar (String.make 4 'H')) 9999;

    (*
    Btree.print_tree_aux t t.root_num 0;
    *)

    (* prints out graph in dot language *)
    (* copy paste the graph starting from digraph *)
    (* save in file with the .dot extension *)
    (* to create the visualization run the command: *)
    (* dot -Tpng <file.dot> -o <out.png> *)
    (* ex: dot -Tpng btree.dot -o btree.png *)
    let graphviz_str = Btree.create_graphviz_str t t.root_num in
    Printf.printf "%s" graphviz_str;
    ""

  let insert_varchar2s () =
    let dir = "tmp_btree_insert_varchar2s" in
    let storage_manager = setup_test_env dir in
    let key_type = TVarchar 2 in

    let t = Btree.create storage_manager key_type in

    (* create another node, so we can insert a new key, pointer pair into the root we created above*)
    let key_ty = t.key in
    let block_size = File.File_manager.get_blocksize t.sm.file_manager in

    let empty = Btree.empty_node t in
    Btree.insert t (Btree.Varchar "AA") 9999;
    Btree.insert t (Btree.Varchar "AB") 9999;
    Btree.insert t (Btree.Varchar "AC") 9999;
    Btree.insert t (Btree.Varchar "AD") 9999;
    Btree.insert t (Btree.Varchar "AE") 9999;
    Btree.insert t (Btree.Varchar "AF") 9999;
    Btree.insert t (Btree.Varchar "AG") 9999;
    Btree.insert t (Btree.Varchar "AH") 9999;
    Btree.insert t (Btree.Varchar "AI") 9999;
    Btree.insert t (Btree.Varchar "AJ") 9999;
    Btree.insert t (Btree.Varchar "BA") 9999;
    Btree.insert t (Btree.Varchar "BB") 9999;
    Btree.insert t (Btree.Varchar "BC") 9999;
    Btree.insert t (Btree.Varchar "BD") 9999;
    Btree.insert t (Btree.Varchar "BE") 9999;
    Btree.insert t (Btree.Varchar "BF") 9999;
    Btree.insert t (Btree.Varchar "BG") 9999;
    Btree.insert t (Btree.Varchar "BH") 9999;
    Btree.insert t (Btree.Varchar "BI") 9999;
    Btree.insert t (Btree.Varchar "BJ") 9999;
    Btree.insert t (Btree.Varchar "CA") 9999;
    Btree.insert t (Btree.Varchar "CB") 9999;
    Btree.insert t (Btree.Varchar "CC") 9999;
    Btree.insert t (Btree.Varchar "CD") 9999;
    Btree.insert t (Btree.Varchar "CE") 9999;
    Btree.insert t (Btree.Varchar "CF") 9999;
    Btree.insert t (Btree.Varchar "CG") 9999;
    Btree.insert t (Btree.Varchar "CH") 9999;
    Btree.insert t (Btree.Varchar "CI") 9999;
    Btree.insert t (Btree.Varchar "CJ") 9999;
    Btree.insert t (Btree.Varchar "DA") 9999;
    Btree.insert t (Btree.Varchar "DB") 9999;
    Btree.insert t (Btree.Varchar "DC") 9999;
    Btree.insert t (Btree.Varchar "DD") 9999;
    Btree.insert t (Btree.Varchar "DE") 9999;
    Btree.insert t (Btree.Varchar "DF") 9999;
    Btree.insert t (Btree.Varchar "DG") 9999;
    Btree.insert t (Btree.Varchar "DH") 9999;
    Btree.insert t (Btree.Varchar "DI") 9999;
    Btree.insert t (Btree.Varchar "DJ") 9999;
    Btree.insert t (Btree.Varchar "EA") 9999;
    Btree.insert t (Btree.Varchar "EB") 9999;
    Btree.insert t (Btree.Varchar "EC") 9999;
    Btree.insert t (Btree.Varchar "ED") 9999;
    Btree.insert t (Btree.Varchar "EE") 9999;
    Btree.insert t (Btree.Varchar "EF") 9999;
    Btree.insert t (Btree.Varchar "EG") 9999;
    Btree.insert t (Btree.Varchar "EH") 9999;
    Btree.insert t (Btree.Varchar "EI") 9999;
    Btree.insert t (Btree.Varchar "EJ") 9999;
    Btree.insert t (Btree.Varchar "FA") 9999;
    Btree.insert t (Btree.Varchar "FB") 9999;
    Btree.insert t (Btree.Varchar "FC") 9999;
    Btree.insert t (Btree.Varchar "FD") 9999;
    Btree.insert t (Btree.Varchar "FE") 9999;
    Btree.insert t (Btree.Varchar "FF") 9999;
    Btree.insert t (Btree.Varchar "FG") 9999;
    Btree.insert t (Btree.Varchar "FH") 9999;
    Btree.insert t (Btree.Varchar "FI") 9999;
    Btree.insert t (Btree.Varchar "FJ") 9999;
    Btree.insert t (Btree.Varchar "GA") 9999;
    Btree.insert t (Btree.Varchar "GB") 9999;
    Btree.insert t (Btree.Varchar "GC") 9999;
    Btree.insert t (Btree.Varchar "GD") 9999;
    Btree.insert t (Btree.Varchar "GE") 9999;
    Btree.insert t (Btree.Varchar "GF") 9999;
    Btree.insert t (Btree.Varchar "GG") 9999;
    Btree.insert t (Btree.Varchar "GH") 9999;
    Btree.insert t (Btree.Varchar "GI") 9999;
    Btree.insert t (Btree.Varchar "GJ") 9999;
    Btree.insert t (Btree.Varchar "HA") 9999;
    Btree.insert t (Btree.Varchar "HB") 9999;
    Btree.insert t (Btree.Varchar "HC") 9999;
    Btree.insert t (Btree.Varchar "HD") 9999;
    Btree.insert t (Btree.Varchar "HE") 9999;
    Btree.insert t (Btree.Varchar "HF") 9999;
    Btree.insert t (Btree.Varchar "HG") 9999;
    Btree.insert t (Btree.Varchar "HH") 9999;
    Btree.insert t (Btree.Varchar "HI") 9999;
    Btree.insert t (Btree.Varchar "HJ") 9999;
    Btree.insert t (Btree.Varchar "IA") 9999;
    Btree.insert t (Btree.Varchar "IB") 9999;
    Btree.insert t (Btree.Varchar "IC") 9999;
    Btree.insert t (Btree.Varchar "ID") 9999;
    Btree.insert t (Btree.Varchar "IE") 9999;
    Btree.insert t (Btree.Varchar "IF") 9999;
    Btree.insert t (Btree.Varchar "IG") 9999;
    Btree.insert t (Btree.Varchar "IH") 9999;
    Btree.insert t (Btree.Varchar "II") 9999;
    Btree.insert t (Btree.Varchar "IJ") 9999;
    Btree.insert t (Btree.Varchar "JA") 9999;
    Btree.insert t (Btree.Varchar "JB") 9999;
    Btree.insert t (Btree.Varchar "JC") 9999;
    Btree.insert t (Btree.Varchar "JD") 9999;
    Btree.insert t (Btree.Varchar "JE") 9999;
    Btree.insert t (Btree.Varchar "JF") 9999;
    Btree.insert t (Btree.Varchar "JG") 9999;
    Btree.insert t (Btree.Varchar "JH") 9999;
    Btree.insert t (Btree.Varchar "JI") 9999;
    Btree.insert t (Btree.Varchar "JJ") 9999;
    let graphviz_str = Btree.create_graphviz_str t t.root_num in
    Printf.printf "%s" graphviz_str;
    ""

  let empty_btree_insert_leaf_test () =
    Alcotest.(check string)
      "verify empty btree creation" expected_output
      (empty_btree_insert_leaf ())

  let insert_in_root_parent_test () =
    Alcotest.(check string)
      "test inserting into parent when parent is root node" ""
      (insert_in_parent_root ())

  let insert_varchar2s_test () =
    Alcotest.(check string)
      "verify empty btree creation" expected_output (insert_varchar2s ())
end

let all_tests () =
  [
    Alcotest.test_case "empty btree creation + insert leaf." `Quick
      Btree_tests.insert_in_root_parent_test;
    Alcotest.test_case "insert varchar2s test" `Quick
      Btree_tests.insert_varchar2s_test;
  ]
