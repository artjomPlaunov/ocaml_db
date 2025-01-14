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
    ""

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
    Btree.print_dot t t.root_num;
    " "

  let empty_btree_insert_leaf_test () =
    Alcotest.(check string)
      "verify empty btree creation" expected_output
      (empty_btree_insert_leaf ())

  let insert_in_root_parent_test () =
    Alcotest.(check string)
      "test inserting into parent when parent is root node" ""
      (insert_in_parent_root ())
end

let all_tests () =
  [
    Alcotest.test_case "empty btree creation + insert leaf." `Quick
      Btree_tests.insert_in_root_parent_test;
  ]
