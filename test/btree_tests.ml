module Test_utils = struct
  open File
  open Storage_manager

  let setup_test_env dir =
    let file_manager = File_manager.make ~db_dirname:dir ~block_size:50 in
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
Btree.insert t (Btree.Varchar "AK") 9999;
Btree.insert t (Btree.Varchar "AL") 9999;
Btree.insert t (Btree.Varchar "AM") 9999;
Btree.insert t (Btree.Varchar "AN") 9999;
Btree.insert t (Btree.Varchar "AO") 9999;
Btree.insert t (Btree.Varchar "AP") 9999;
Btree.insert t (Btree.Varchar "AQ") 9999;
Btree.insert t (Btree.Varchar "AR") 9999;
Btree.insert t (Btree.Varchar "AS") 9999;
Btree.insert t (Btree.Varchar "AT") 9999;
Btree.insert t (Btree.Varchar "AU") 9999; 
Btree.insert t (Btree.Varchar "AV") 9999;
Btree.insert t (Btree.Varchar "AW") 9999;
Btree.insert t (Btree.Varchar "AX") 9999;
Btree.insert t (Btree.Varchar "AY") 9999;
Btree.insert t (Btree.Varchar "AZ") 9999;
Btree.insert t (Btree.Varchar "BA") 9999;
(* Btree.insert t (Btree.Varchar "BB") 9999;
Btree.insert t (Btree.Varchar "BC") 9999;
Btree.insert t (Btree.Varchar "BD") 9999;
Btree.insert t (Btree.Varchar "BE") 9999;
Btree.insert t (Btree.Varchar "BF") 9999;
Btree.insert t (Btree.Varchar "BG") 9999;
Btree.insert t (Btree.Varchar "BH") 9999;
Btree.insert t (Btree.Varchar "BI") 9999;
Btree.insert t (Btree.Varchar "BJ") 9999;
Btree.insert t (Btree.Varchar "BK") 9999;
Btree.insert t (Btree.Varchar "BL") 9999;
Btree.insert t (Btree.Varchar "BM") 9999;
Btree.insert t (Btree.Varchar "BN") 9999;
Btree.insert t (Btree.Varchar "BO") 9999;
Btree.insert t (Btree.Varchar "BP") 9999;
Btree.insert t (Btree.Varchar "BQ") 9999;
Btree.insert t (Btree.Varchar "BR") 9999;
Btree.insert t (Btree.Varchar "BS") 9999;
Btree.insert t (Btree.Varchar "BT") 9999;
Btree.insert t (Btree.Varchar "BU") 9999;
Btree.insert t (Btree.Varchar "BV") 9999;
Btree.insert t (Btree.Varchar "BW") 9999;
Btree.insert t (Btree.Varchar "BX") 9999;
Btree.insert t (Btree.Varchar "BY") 9999;
Btree.insert t (Btree.Varchar "BZ") 9999;
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
Btree.insert t (Btree.Varchar "CK") 9999;
Btree.insert t (Btree.Varchar "CL") 9999;
Btree.insert t (Btree.Varchar "CM") 9999;
Btree.insert t (Btree.Varchar "CN") 9999;
Btree.insert t (Btree.Varchar "CO") 9999;
Btree.insert t (Btree.Varchar "CP") 9999;
Btree.insert t (Btree.Varchar "CQ") 9999;
Btree.insert t (Btree.Varchar "CR") 9999;
Btree.insert t (Btree.Varchar "CS") 9999;
Btree.insert t (Btree.Varchar "CT") 9999;
Btree.insert t (Btree.Varchar "CU") 9999;
Btree.insert t (Btree.Varchar "CV") 9999;
Btree.insert t (Btree.Varchar "CW") 9999;
Btree.insert t (Btree.Varchar "CX") 9999;
Btree.insert t (Btree.Varchar "CY") 9999;
Btree.insert t (Btree.Varchar "CZ") 9999;
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
Btree.insert t (Btree.Varchar "DK") 9999;
Btree.insert t (Btree.Varchar "DL") 9999;
Btree.insert t (Btree.Varchar "DM") 9999;
Btree.insert t (Btree.Varchar "DN") 9999;
Btree.insert t (Btree.Varchar "DO") 9999;
Btree.insert t (Btree.Varchar "DP") 9999;
Btree.insert t (Btree.Varchar "DQ") 9999;
Btree.insert t (Btree.Varchar "DR") 9999;
Btree.insert t (Btree.Varchar "DS") 9999;
Btree.insert t (Btree.Varchar "DT") 9999;
Btree.insert t (Btree.Varchar "DU") 9999;
Btree.insert t (Btree.Varchar "DV") 9999;
Btree.insert t (Btree.Varchar "DW") 9999;
Btree.insert t (Btree.Varchar "DX") 9999;
Btree.insert t (Btree.Varchar "DY") 9999;
Btree.insert t (Btree.Varchar "DZ") 9999;
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
Btree.insert t (Btree.Varchar "EK") 9999;
Btree.insert t (Btree.Varchar "EL") 9999;
Btree.insert t (Btree.Varchar "EM") 9999;
Btree.insert t (Btree.Varchar "EN") 9999;
Btree.insert t (Btree.Varchar "EO") 9999;
Btree.insert t (Btree.Varchar "EP") 9999;
Btree.insert t (Btree.Varchar "EQ") 9999;
Btree.insert t (Btree.Varchar "ER") 9999;
Btree.insert t (Btree.Varchar "ES") 9999;
Btree.insert t (Btree.Varchar "ET") 9999;
Btree.insert t (Btree.Varchar "EU") 9999;
Btree.insert t (Btree.Varchar "EV") 9999;
Btree.insert t (Btree.Varchar "EW") 9999;
Btree.insert t (Btree.Varchar "EX") 9999;
Btree.insert t (Btree.Varchar "EY") 9999;
Btree.insert t (Btree.Varchar "EZ") 9999;
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
Btree.insert t (Btree.Varchar "FK") 9999;
Btree.insert t (Btree.Varchar "FL") 9999;
Btree.insert t (Btree.Varchar "FM") 9999;
Btree.insert t (Btree.Varchar "FN") 9999;
Btree.insert t (Btree.Varchar "FO") 9999;
Btree.insert t (Btree.Varchar "FP") 9999;
Btree.insert t (Btree.Varchar "FQ") 9999;
Btree.insert t (Btree.Varchar "FR") 9999;
Btree.insert t (Btree.Varchar "FS") 9999;
Btree.insert t (Btree.Varchar "FT") 9999;
Btree.insert t (Btree.Varchar "FU") 9999;
Btree.insert t (Btree.Varchar "FV") 9999;
Btree.insert t (Btree.Varchar "FW") 9999;
Btree.insert t (Btree.Varchar "FX") 9999;
Btree.insert t (Btree.Varchar "FY") 9999;
Btree.insert t (Btree.Varchar "FZ") 9999;
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
Btree.insert t (Btree.Varchar "GK") 9999;
Btree.insert t (Btree.Varchar "GL") 9999;
Btree.insert t (Btree.Varchar "GM") 9999;
Btree.insert t (Btree.Varchar "GN") 9999;
Btree.insert t (Btree.Varchar "GO") 9999;
Btree.insert t (Btree.Varchar "GP") 9999;
Btree.insert t (Btree.Varchar "GQ") 9999;
Btree.insert t (Btree.Varchar "GR") 9999;
Btree.insert t (Btree.Varchar "GS") 9999;
Btree.insert t (Btree.Varchar "GT") 9999;
Btree.insert t (Btree.Varchar "GU") 9999;
Btree.insert t (Btree.Varchar "GV") 9999;
Btree.insert t (Btree.Varchar "GW") 9999;
Btree.insert t (Btree.Varchar "GX") 9999;
Btree.insert t (Btree.Varchar "GY") 9999;
Btree.insert t (Btree.Varchar "GZ") 9999;
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
Btree.insert t (Btree.Varchar "HK") 9999;
Btree.insert t (Btree.Varchar "HL") 9999;
Btree.insert t (Btree.Varchar "HM") 9999;
Btree.insert t (Btree.Varchar "HN") 9999;
Btree.insert t (Btree.Varchar "HO") 9999;
Btree.insert t (Btree.Varchar "HP") 9999;
Btree.insert t (Btree.Varchar "HQ") 9999;
Btree.insert t (Btree.Varchar "HR") 9999;
Btree.insert t (Btree.Varchar "HS") 9999;
Btree.insert t (Btree.Varchar "HT") 9999;
Btree.insert t (Btree.Varchar "HU") 9999;
Btree.insert t (Btree.Varchar "HV") 9999;
Btree.insert t (Btree.Varchar "HW") 9999;
Btree.insert t (Btree.Varchar "HX") 9999;
Btree.insert t (Btree.Varchar "HY") 9999;
Btree.insert t (Btree.Varchar "HZ") 9999;
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
Btree.insert t (Btree.Varchar "IK") 9999;
Btree.insert t (Btree.Varchar "IL") 9999;
Btree.insert t (Btree.Varchar "IM") 9999;
Btree.insert t (Btree.Varchar "IN") 9999;
Btree.insert t (Btree.Varchar "IO") 9999;
Btree.insert t (Btree.Varchar "IP") 9999;
Btree.insert t (Btree.Varchar "IQ") 9999;
Btree.insert t (Btree.Varchar "IR") 9999;
Btree.insert t (Btree.Varchar "IS") 9999;
Btree.insert t (Btree.Varchar "IT") 9999;
Btree.insert t (Btree.Varchar "IU") 9999;
Btree.insert t (Btree.Varchar "IV") 9999;
Btree.insert t (Btree.Varchar "IW") 9999;
Btree.insert t (Btree.Varchar "IX") 9999;
Btree.insert t (Btree.Varchar "IY") 9999;
Btree.insert t (Btree.Varchar "IZ") 9999;
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
Btree.insert t (Btree.Varchar "JK") 9999;
Btree.insert t (Btree.Varchar "JL") 9999;
Btree.insert t (Btree.Varchar "JM") 9999;
Btree.insert t (Btree.Varchar "JN") 9999;
Btree.insert t (Btree.Varchar "JO") 9999;
Btree.insert t (Btree.Varchar "JP") 9999;
Btree.insert t (Btree.Varchar "JQ") 9999;
Btree.insert t (Btree.Varchar "JR") 9999;
Btree.insert t (Btree.Varchar "JS") 9999;
Btree.insert t (Btree.Varchar "JT") 9999;
Btree.insert t (Btree.Varchar "JU") 9999;
Btree.insert t (Btree.Varchar "JV") 9999;
Btree.insert t (Btree.Varchar "JW") 9999;
Btree.insert t (Btree.Varchar "JX") 9999;
Btree.insert t (Btree.Varchar "JY") 9999;
Btree.insert t (Btree.Varchar "JZ") 9999;
Btree.insert t (Btree.Varchar "KA") 9999;
Btree.insert t (Btree.Varchar "KB") 9999;
Btree.insert t (Btree.Varchar "KC") 9999;
Btree.insert t (Btree.Varchar "KD") 9999;
Btree.insert t (Btree.Varchar "KE") 9999;
Btree.insert t (Btree.Varchar "KF") 9999;
Btree.insert t (Btree.Varchar "KG") 9999;
Btree.insert t (Btree.Varchar "KH") 9999;
Btree.insert t (Btree.Varchar "KI") 9999;
Btree.insert t (Btree.Varchar "KJ") 9999;
Btree.insert t (Btree.Varchar "KK") 9999;
Btree.insert t (Btree.Varchar "KL") 9999;
Btree.insert t (Btree.Varchar "KM") 9999;
Btree.insert t (Btree.Varchar "KN") 9999;
Btree.insert t (Btree.Varchar "KO") 9999;
Btree.insert t (Btree.Varchar "KP") 9999;
Btree.insert t (Btree.Varchar "KQ") 9999;
Btree.insert t (Btree.Varchar "KR") 9999;
Btree.insert t (Btree.Varchar "KS") 9999;
Btree.insert t (Btree.Varchar "KT") 9999;
Btree.insert t (Btree.Varchar "KU") 9999;
Btree.insert t (Btree.Varchar "KV") 9999;
Btree.insert t (Btree.Varchar "KW") 9999;
Btree.insert t (Btree.Varchar "KX") 9999;
Btree.insert t (Btree.Varchar "KY") 9999;
Btree.insert t (Btree.Varchar "KZ") 9999;
Btree.insert t (Btree.Varchar "LA") 9999;
Btree.insert t (Btree.Varchar "LB") 9999;
Btree.insert t (Btree.Varchar "LC") 9999;
Btree.insert t (Btree.Varchar "LD") 9999;
Btree.insert t (Btree.Varchar "LE") 9999;
Btree.insert t (Btree.Varchar "LF") 9999;
Btree.insert t (Btree.Varchar "LG") 9999;
Btree.insert t (Btree.Varchar "LH") 9999;
Btree.insert t (Btree.Varchar "LI") 9999;
Btree.insert t (Btree.Varchar "LJ") 9999;
Btree.insert t (Btree.Varchar "LK") 9999;
Btree.insert t (Btree.Varchar "LL") 9999;
Btree.insert t (Btree.Varchar "LM") 9999;
Btree.insert t (Btree.Varchar "LN") 9999;
Btree.insert t (Btree.Varchar "LO") 9999;
Btree.insert t (Btree.Varchar "LP") 9999;
Btree.insert t (Btree.Varchar "LQ") 9999;
Btree.insert t (Btree.Varchar "LR") 9999;
Btree.insert t (Btree.Varchar "LS") 9999;
Btree.insert t (Btree.Varchar "LT") 9999;
Btree.insert t (Btree.Varchar "LU") 9999;
Btree.insert t (Btree.Varchar "LV") 9999;
Btree.insert t (Btree.Varchar "LW") 9999;
Btree.insert t (Btree.Varchar "LX") 9999;
Btree.insert t (Btree.Varchar "LY") 9999;
Btree.insert t (Btree.Varchar "LZ") 9999;
Btree.insert t (Btree.Varchar "MA") 9999;
Btree.insert t (Btree.Varchar "MB") 9999;
Btree.insert t (Btree.Varchar "MC") 9999;
Btree.insert t (Btree.Varchar "MD") 9999;
Btree.insert t (Btree.Varchar "ME") 9999;
Btree.insert t (Btree.Varchar "MF") 9999;
Btree.insert t (Btree.Varchar "MG") 9999;
Btree.insert t (Btree.Varchar "MH") 9999;
Btree.insert t (Btree.Varchar "MI") 9999;
Btree.insert t (Btree.Varchar "MJ") 9999;
Btree.insert t (Btree.Varchar "MK") 9999;
Btree.insert t (Btree.Varchar "ML") 9999;
Btree.insert t (Btree.Varchar "MM") 9999;
Btree.insert t (Btree.Varchar "MN") 9999;
Btree.insert t (Btree.Varchar "MO") 9999;
Btree.insert t (Btree.Varchar "MP") 9999;
Btree.insert t (Btree.Varchar "MQ") 9999;
Btree.insert t (Btree.Varchar "MR") 9999;
Btree.insert t (Btree.Varchar "MS") 9999;
Btree.insert t (Btree.Varchar "MT") 9999;
Btree.insert t (Btree.Varchar "MU") 9999;
Btree.insert t (Btree.Varchar "MV") 9999;
Btree.insert t (Btree.Varchar "MW") 9999;
Btree.insert t (Btree.Varchar "MX") 9999;
Btree.insert t (Btree.Varchar "MY") 9999;
Btree.insert t (Btree.Varchar "MZ") 9999;
Btree.insert t (Btree.Varchar "NA") 9999;
Btree.insert t (Btree.Varchar "NB") 9999;
Btree.insert t (Btree.Varchar "NC") 9999;
Btree.insert t (Btree.Varchar "ND") 9999;
Btree.insert t (Btree.Varchar "NE") 9999;
Btree.insert t (Btree.Varchar "NF") 9999;
Btree.insert t (Btree.Varchar "NG") 9999;
Btree.insert t (Btree.Varchar "NH") 9999;
Btree.insert t (Btree.Varchar "NI") 9999;
Btree.insert t (Btree.Varchar "NJ") 9999;
Btree.insert t (Btree.Varchar "NK") 9999;
Btree.insert t (Btree.Varchar "NL") 9999;
Btree.insert t (Btree.Varchar "NM") 9999;
Btree.insert t (Btree.Varchar "NN") 9999;
Btree.insert t (Btree.Varchar "NO") 9999;
Btree.insert t (Btree.Varchar "NP") 9999;
Btree.insert t (Btree.Varchar "NQ") 9999;
Btree.insert t (Btree.Varchar "NR") 9999;
Btree.insert t (Btree.Varchar "NS") 9999;
Btree.insert t (Btree.Varchar "NT") 9999;
Btree.insert t (Btree.Varchar "NU") 9999;
Btree.insert t (Btree.Varchar "NV") 9999;
Btree.insert t (Btree.Varchar "NW") 9999;
Btree.insert t (Btree.Varchar "NX") 9999;
Btree.insert t (Btree.Varchar "NY") 9999;
Btree.insert t (Btree.Varchar "NZ") 9999;
Btree.insert t (Btree.Varchar "OA") 9999;
Btree.insert t (Btree.Varchar "OB") 9999;
Btree.insert t (Btree.Varchar "OC") 9999;
Btree.insert t (Btree.Varchar "OD") 9999;
Btree.insert t (Btree.Varchar "OE") 9999;
Btree.insert t (Btree.Varchar "OF") 9999;
Btree.insert t (Btree.Varchar "OG") 9999;
Btree.insert t (Btree.Varchar "OH") 9999;
Btree.insert t (Btree.Varchar "OI") 9999;
Btree.insert t (Btree.Varchar "OJ") 9999;
Btree.insert t (Btree.Varchar "OK") 9999;
Btree.insert t (Btree.Varchar "OL") 9999;
Btree.insert t (Btree.Varchar "OM") 9999;
Btree.insert t (Btree.Varchar "ON") 9999;
Btree.insert t (Btree.Varchar "OO") 9999;
Btree.insert t (Btree.Varchar "OP") 9999;
Btree.insert t (Btree.Varchar "OQ") 9999;
Btree.insert t (Btree.Varchar "OR") 9999;
Btree.insert t (Btree.Varchar "OS") 9999;
Btree.insert t (Btree.Varchar "OT") 9999;
Btree.insert t (Btree.Varchar "OU") 9999;
Btree.insert t (Btree.Varchar "OV") 9999;
Btree.insert t (Btree.Varchar "OW") 9999;
Btree.insert t (Btree.Varchar "OX") 9999;
Btree.insert t (Btree.Varchar "OY") 9999;
Btree.insert t (Btree.Varchar "OZ") 9999;
Btree.insert t (Btree.Varchar "PA") 9999;
Btree.insert t (Btree.Varchar "PB") 9999;
Btree.insert t (Btree.Varchar "PC") 9999;
Btree.insert t (Btree.Varchar "PD") 9999;
Btree.insert t (Btree.Varchar "PE") 9999;
Btree.insert t (Btree.Varchar "PF") 9999;
Btree.insert t (Btree.Varchar "PG") 9999;
Btree.insert t (Btree.Varchar "PH") 9999;
Btree.insert t (Btree.Varchar "PI") 9999;
Btree.insert t (Btree.Varchar "PJ") 9999;
Btree.insert t (Btree.Varchar "PK") 9999;
Btree.insert t (Btree.Varchar "PL") 9999;
Btree.insert t (Btree.Varchar "PM") 9999;
Btree.insert t (Btree.Varchar "PN") 9999;
Btree.insert t (Btree.Varchar "PO") 9999;
Btree.insert t (Btree.Varchar "PP") 9999;
Btree.insert t (Btree.Varchar "PQ") 9999;
Btree.insert t (Btree.Varchar "PR") 9999;
Btree.insert t (Btree.Varchar "PS") 9999;
Btree.insert t (Btree.Varchar "PT") 9999;
Btree.insert t (Btree.Varchar "PU") 9999;
Btree.insert t (Btree.Varchar "PV") 9999;
Btree.insert t (Btree.Varchar "PW") 9999;
Btree.insert t (Btree.Varchar "PX") 9999;
Btree.insert t (Btree.Varchar "PY") 9999;
Btree.insert t (Btree.Varchar "PZ") 9999;
Btree.insert t (Btree.Varchar "QA") 9999;
Btree.insert t (Btree.Varchar "QB") 9999;
Btree.insert t (Btree.Varchar "QC") 9999;
Btree.insert t (Btree.Varchar "QD") 9999;
Btree.insert t (Btree.Varchar "QE") 9999;
Btree.insert t (Btree.Varchar "QF") 9999;
Btree.insert t (Btree.Varchar "QG") 9999;
Btree.insert t (Btree.Varchar "QH") 9999;
Btree.insert t (Btree.Varchar "QI") 9999;
Btree.insert t (Btree.Varchar "QJ") 9999;
Btree.insert t (Btree.Varchar "QK") 9999;
Btree.insert t (Btree.Varchar "QL") 9999;
Btree.insert t (Btree.Varchar "QM") 9999;
Btree.insert t (Btree.Varchar "QN") 9999;
Btree.insert t (Btree.Varchar "QO") 9999;
Btree.insert t (Btree.Varchar "QP") 9999;
Btree.insert t (Btree.Varchar "QQ") 9999;
Btree.insert t (Btree.Varchar "QR") 9999;
Btree.insert t (Btree.Varchar "QS") 9999;
Btree.insert t (Btree.Varchar "QT") 9999;
Btree.insert t (Btree.Varchar "QU") 9999;
Btree.insert t (Btree.Varchar "QV") 9999;
Btree.insert t (Btree.Varchar "QW") 9999;
Btree.insert t (Btree.Varchar "QX") 9999;
Btree.insert t (Btree.Varchar "QY") 9999;
Btree.insert t (Btree.Varchar "QZ") 9999;
Btree.insert t (Btree.Varchar "RA") 9999;
Btree.insert t (Btree.Varchar "RB") 9999;
Btree.insert t (Btree.Varchar "RC") 9999;
Btree.insert t (Btree.Varchar "RD") 9999;
Btree.insert t (Btree.Varchar "RE") 9999;
Btree.insert t (Btree.Varchar "RF") 9999;
Btree.insert t (Btree.Varchar "RG") 9999;
Btree.insert t (Btree.Varchar "RH") 9999;
Btree.insert t (Btree.Varchar "RI") 9999;
Btree.insert t (Btree.Varchar "RJ") 9999;
Btree.insert t (Btree.Varchar "RK") 9999;
Btree.insert t (Btree.Varchar "RL") 9999;
Btree.insert t (Btree.Varchar "RM") 9999;
Btree.insert t (Btree.Varchar "RN") 9999;
Btree.insert t (Btree.Varchar "RO") 9999;
Btree.insert t (Btree.Varchar "RP") 9999;
Btree.insert t (Btree.Varchar "RQ") 9999;
Btree.insert t (Btree.Varchar "RR") 9999;
Btree.insert t (Btree.Varchar "RS") 9999;
Btree.insert t (Btree.Varchar "RT") 9999;
Btree.insert t (Btree.Varchar "RU") 9999;
Btree.insert t (Btree.Varchar "RV") 9999;
Btree.insert t (Btree.Varchar "RW") 9999;
Btree.insert t (Btree.Varchar "RX") 9999;
Btree.insert t (Btree.Varchar "RY") 9999;
Btree.insert t (Btree.Varchar "RZ") 9999;
Btree.insert t (Btree.Varchar "SA") 9999;
Btree.insert t (Btree.Varchar "SB") 9999;
Btree.insert t (Btree.Varchar "SC") 9999;
Btree.insert t (Btree.Varchar "SD") 9999;
Btree.insert t (Btree.Varchar "SE") 9999;
Btree.insert t (Btree.Varchar "SF") 9999;
Btree.insert t (Btree.Varchar "SG") 9999;
Btree.insert t (Btree.Varchar "SH") 9999;
Btree.insert t (Btree.Varchar "SI") 9999;
Btree.insert t (Btree.Varchar "SJ") 9999;
Btree.insert t (Btree.Varchar "SK") 9999;
Btree.insert t (Btree.Varchar "SL") 9999;
Btree.insert t (Btree.Varchar "SM") 9999;
Btree.insert t (Btree.Varchar "SN") 9999;
Btree.insert t (Btree.Varchar "SO") 9999;
Btree.insert t (Btree.Varchar "SP") 9999;
Btree.insert t (Btree.Varchar "SQ") 9999;
Btree.insert t (Btree.Varchar "SR") 9999;
Btree.insert t (Btree.Varchar "SS") 9999;
Btree.insert t (Btree.Varchar "ST") 9999;
Btree.insert t (Btree.Varchar "SU") 9999;
Btree.insert t (Btree.Varchar "SV") 9999;
Btree.insert t (Btree.Varchar "SW") 9999;
Btree.insert t (Btree.Varchar "SX") 9999;
Btree.insert t (Btree.Varchar "SY") 9999;
Btree.insert t (Btree.Varchar "SZ") 9999;
Btree.insert t (Btree.Varchar "TA") 9999;
Btree.insert t (Btree.Varchar "TB") 9999;
Btree.insert t (Btree.Varchar "TC") 9999;
Btree.insert t (Btree.Varchar "TD") 9999;
Btree.insert t (Btree.Varchar "TE") 9999;
Btree.insert t (Btree.Varchar "TF") 9999;
Btree.insert t (Btree.Varchar "TG") 9999;
Btree.insert t (Btree.Varchar "TH") 9999;
Btree.insert t (Btree.Varchar "TI") 9999;
Btree.insert t (Btree.Varchar "TJ") 9999;
Btree.insert t (Btree.Varchar "TK") 9999;
Btree.insert t (Btree.Varchar "TL") 9999;
Btree.insert t (Btree.Varchar "TM") 9999;
Btree.insert t (Btree.Varchar "TN") 9999;
Btree.insert t (Btree.Varchar "TO") 9999;
Btree.insert t (Btree.Varchar "TP") 9999;
Btree.insert t (Btree.Varchar "TQ") 9999;
Btree.insert t (Btree.Varchar "TR") 9999;
Btree.insert t (Btree.Varchar "TS") 9999;
Btree.insert t (Btree.Varchar "TT") 9999;
Btree.insert t (Btree.Varchar "TU") 9999;
Btree.insert t (Btree.Varchar "TV") 9999;
Btree.insert t (Btree.Varchar "TW") 9999;
Btree.insert t (Btree.Varchar "TX") 9999;
Btree.insert t (Btree.Varchar "TY") 9999;
Btree.insert t (Btree.Varchar "TZ") 9999;
Btree.insert t (Btree.Varchar "UA") 9999;
Btree.insert t (Btree.Varchar "UB") 9999;
Btree.insert t (Btree.Varchar "UC") 9999;
Btree.insert t (Btree.Varchar "UD") 9999;
Btree.insert t (Btree.Varchar "UE") 9999;
Btree.insert t (Btree.Varchar "UF") 9999;
Btree.insert t (Btree.Varchar "UG") 9999;
Btree.insert t (Btree.Varchar "UH") 9999;
Btree.insert t (Btree.Varchar "UI") 9999;
Btree.insert t (Btree.Varchar "UJ") 9999;
Btree.insert t (Btree.Varchar "UK") 9999;
Btree.insert t (Btree.Varchar "UL") 9999;
Btree.insert t (Btree.Varchar "UM") 9999;
Btree.insert t (Btree.Varchar "UN") 9999;
Btree.insert t (Btree.Varchar "UO") 9999;
Btree.insert t (Btree.Varchar "UP") 9999;
Btree.insert t (Btree.Varchar "UQ") 9999;
Btree.insert t (Btree.Varchar "UR") 9999;
Btree.insert t (Btree.Varchar "US") 9999;
Btree.insert t (Btree.Varchar "UT") 9999;
Btree.insert t (Btree.Varchar "UU") 9999;
Btree.insert t (Btree.Varchar "UV") 9999;
Btree.insert t (Btree.Varchar "UW") 9999;
Btree.insert t (Btree.Varchar "UX") 9999;
Btree.insert t (Btree.Varchar "UY") 9999;
Btree.insert t (Btree.Varchar "UZ") 9999;
Btree.insert t (Btree.Varchar "VA") 9999;
Btree.insert t (Btree.Varchar "VB") 9999;
Btree.insert t (Btree.Varchar "VC") 9999;
Btree.insert t (Btree.Varchar "VD") 9999;
Btree.insert t (Btree.Varchar "VE") 9999;
Btree.insert t (Btree.Varchar "VF") 9999;
Btree.insert t (Btree.Varchar "VG") 9999;
Btree.insert t (Btree.Varchar "VH") 9999;
Btree.insert t (Btree.Varchar "VI") 9999;
Btree.insert t (Btree.Varchar "VJ") 9999;
Btree.insert t (Btree.Varchar "VK") 9999;
Btree.insert t (Btree.Varchar "VL") 9999;
Btree.insert t (Btree.Varchar "VM") 9999;
Btree.insert t (Btree.Varchar "VN") 9999;
Btree.insert t (Btree.Varchar "VO") 9999;
Btree.insert t (Btree.Varchar "VP") 9999;
Btree.insert t (Btree.Varchar "VQ") 9999;
Btree.insert t (Btree.Varchar "VR") 9999;
Btree.insert t (Btree.Varchar "VS") 9999;
Btree.insert t (Btree.Varchar "VT") 9999;
Btree.insert t (Btree.Varchar "VU") 9999;
Btree.insert t (Btree.Varchar "VV") 9999;
Btree.insert t (Btree.Varchar "VW") 9999;
Btree.insert t (Btree.Varchar "VX") 9999;
Btree.insert t (Btree.Varchar "VY") 9999;
Btree.insert t (Btree.Varchar "VZ") 9999;
Btree.insert t (Btree.Varchar "WA") 9999;
Btree.insert t (Btree.Varchar "WB") 9999;
Btree.insert t (Btree.Varchar "WC") 9999;
Btree.insert t (Btree.Varchar "WD") 9999;
Btree.insert t (Btree.Varchar "WE") 9999;
Btree.insert t (Btree.Varchar "WF") 9999;
Btree.insert t (Btree.Varchar "WG") 9999;
Btree.insert t (Btree.Varchar "WH") 9999;
Btree.insert t (Btree.Varchar "WI") 9999;
Btree.insert t (Btree.Varchar "WJ") 9999;
Btree.insert t (Btree.Varchar "WK") 9999;
Btree.insert t (Btree.Varchar "WL") 9999;
Btree.insert t (Btree.Varchar "WM") 9999;
Btree.insert t (Btree.Varchar "WN") 9999;
Btree.insert t (Btree.Varchar "WO") 9999;
Btree.insert t (Btree.Varchar "WP") 9999;
Btree.insert t (Btree.Varchar "WQ") 9999;
Btree.insert t (Btree.Varchar "WR") 9999;
Btree.insert t (Btree.Varchar "WS") 9999;
Btree.insert t (Btree.Varchar "WT") 9999;
Btree.insert t (Btree.Varchar "WU") 9999;
Btree.insert t (Btree.Varchar "WV") 9999;
Btree.insert t (Btree.Varchar "WW") 9999;
Btree.insert t (Btree.Varchar "WX") 9999;
Btree.insert t (Btree.Varchar "WY") 9999;
Btree.insert t (Btree.Varchar "WZ") 9999;
Btree.insert t (Btree.Varchar "XA") 9999;
Btree.insert t (Btree.Varchar "XB") 9999;
Btree.insert t (Btree.Varchar "XC") 9999;
Btree.insert t (Btree.Varchar "XD") 9999;
Btree.insert t (Btree.Varchar "XE") 9999;
Btree.insert t (Btree.Varchar "XF") 9999;
Btree.insert t (Btree.Varchar "XG") 9999;
Btree.insert t (Btree.Varchar "XH") 9999;
Btree.insert t (Btree.Varchar "XI") 9999;
Btree.insert t (Btree.Varchar "XJ") 9999;
Btree.insert t (Btree.Varchar "XK") 9999;
Btree.insert t (Btree.Varchar "XL") 9999;
Btree.insert t (Btree.Varchar "XM") 9999;
Btree.insert t (Btree.Varchar "XN") 9999;
Btree.insert t (Btree.Varchar "XO") 9999;
Btree.insert t (Btree.Varchar "XP") 9999;
Btree.insert t (Btree.Varchar "XQ") 9999;
Btree.insert t (Btree.Varchar "XR") 9999;
Btree.insert t (Btree.Varchar "XS") 9999;
Btree.insert t (Btree.Varchar "XT") 9999;
Btree.insert t (Btree.Varchar "XU") 9999;
Btree.insert t (Btree.Varchar "XV") 9999;
Btree.insert t (Btree.Varchar "XW") 9999;
Btree.insert t (Btree.Varchar "XX") 9999;
Btree.insert t (Btree.Varchar "XY") 9999;
Btree.insert t (Btree.Varchar "XZ") 9999;
Btree.insert t (Btree.Varchar "YA") 9999;
Btree.insert t (Btree.Varchar "YB") 9999;
Btree.insert t (Btree.Varchar "YC") 9999;
Btree.insert t (Btree.Varchar "YD") 9999;
Btree.insert t (Btree.Varchar "YE") 9999;
Btree.insert t (Btree.Varchar "YF") 9999;
Btree.insert t (Btree.Varchar "YG") 9999;
Btree.insert t (Btree.Varchar "YH") 9999;
Btree.insert t (Btree.Varchar "YI") 9999;
Btree.insert t (Btree.Varchar "YJ") 9999;
Btree.insert t (Btree.Varchar "YK") 9999;
Btree.insert t (Btree.Varchar "YL") 9999;
Btree.insert t (Btree.Varchar "YM") 9999;
Btree.insert t (Btree.Varchar "YN") 9999;
Btree.insert t (Btree.Varchar "YO") 9999;
Btree.insert t (Btree.Varchar "YP") 9999;
Btree.insert t (Btree.Varchar "YQ") 9999;
Btree.insert t (Btree.Varchar "YR") 9999;
Btree.insert t (Btree.Varchar "YS") 9999;
Btree.insert t (Btree.Varchar "YT") 9999;
Btree.insert t (Btree.Varchar "YU") 9999;
Btree.insert t (Btree.Varchar "YV") 9999;
Btree.insert t (Btree.Varchar "YW") 9999;
Btree.insert t (Btree.Varchar "YX") 9999;
Btree.insert t (Btree.Varchar "YY") 9999;
Btree.insert t (Btree.Varchar "YZ") 9999;
Btree.insert t (Btree.Varchar "ZA") 9999;
Btree.insert t (Btree.Varchar "ZB") 9999;
Btree.insert t (Btree.Varchar "ZC") 9999;
Btree.insert t (Btree.Varchar "ZD") 9999;
Btree.insert t (Btree.Varchar "ZE") 9999;
Btree.insert t (Btree.Varchar "ZF") 9999;
Btree.insert t (Btree.Varchar "ZG") 9999;
Btree.insert t (Btree.Varchar "ZH") 9999;
Btree.insert t (Btree.Varchar "ZI") 9999;
Btree.insert t (Btree.Varchar "ZJ") 9999;
Btree.insert t (Btree.Varchar "ZK") 9999;
Btree.insert t (Btree.Varchar "ZL") 9999;
Btree.insert t (Btree.Varchar "ZM") 9999;
Btree.insert t (Btree.Varchar "ZN") 9999;
Btree.insert t (Btree.Varchar "ZO") 9999;
Btree.insert t (Btree.Varchar "ZP") 9999;
Btree.insert t (Btree.Varchar "ZQ") 9999;
Btree.insert t (Btree.Varchar "ZR") 9999;
Btree.insert t (Btree.Varchar "ZS") 9999;
Btree.insert t (Btree.Varchar "ZT") 9999;
Btree.insert t (Btree.Varchar "ZU") 9999;
Btree.insert t (Btree.Varchar "ZV") 9999;
Btree.insert t (Btree.Varchar "ZW") 9999;
Btree.insert t (Btree.Varchar "ZX") 9999;
Btree.insert t (Btree.Varchar "ZY") 9999;
Btree.insert t (Btree.Varchar "ZZ") 9999; *)

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
