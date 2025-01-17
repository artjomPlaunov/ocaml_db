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
    let key_type = KeyType.TVarchar 4 in
    let t = Btree.create storage_manager key_type in
    (* Empty tree.*)
    let _ = Btree.insert_in_leaf t 1 (KeyType.Varchar (String.make 4 'J')) 4 in
    let _ = Btree.insert_in_leaf t 1 (KeyType.Varchar (String.make 4 'A')) 3 in
    let _ = Btree.insert_in_leaf t 1 (KeyType.Varchar (String.make 4 'V')) 2 in
    let _ = Btree.insert_in_leaf t 1 (KeyType.Varchar (String.make 4 'Q')) 1 in
    let _ = Btree.insert_in_leaf t 1 (KeyType.Varchar (String.make 4 'S')) 5 in
    let _ = Btree.insert_in_leaf t 1 (KeyType.Varchar (String.make 4 'Z')) 6 in
    let _ = Btree.insert_in_leaf t 1 (KeyType.Varchar (String.make 4 'C')) 7 in
    let _ = Btree.insert_in_leaf t 1 (KeyType.Varchar (String.make 4 'P')) 8 in
    " "

  let insert_in_parent_root () =
    let dir = "tmp_btree_insert_parent_root" in
    let storage_manager = setup_test_env dir in
    let key_type = KeyType.TVarchar 4 in

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
    Btree.insert t (KeyType.Varchar (String.make 4 'A')) 9999;

    Btree.insert t (KeyType.Varchar (String.make 4 'U')) 9999;
    Btree.insert t (KeyType.Varchar (String.make 4 'C')) 9999;
    Btree.insert t (KeyType.Varchar (String.make 4 'D')) 9999;

    Btree.insert t (KeyType.Varchar (String.make 4 'S')) 9999;
    Btree.insert t (KeyType.Varchar (String.make 4 'E')) 9999;

    Btree.insert t (KeyType.Varchar (String.make 4 'Z')) 9999;
    Btree.insert t (KeyType.Varchar (String.make 4 'L')) 9999;
    Btree.insert t (KeyType.Varchar (String.make 4 'F')) 9999;
    Btree.insert t (KeyType.Varchar (String.make 4 'G')) 9999;

    Btree.insert t (KeyType.Varchar (String.make 4 'I')) 9999;
    Btree.insert t (KeyType.Varchar (String.make 4 'J')) 9999;
    Btree.insert t (KeyType.Varchar (String.make 4 'K')) 9999;

    Btree.insert t (KeyType.Varchar (String.make 4 'M')) 9999;
    Btree.insert t (KeyType.Varchar (String.make 4 'W')) 9999;
    Btree.insert t (KeyType.Varchar (String.make 4 'X')) 9999;

    Btree.insert t (KeyType.Varchar (String.make 4 'Q')) 9999;
    Btree.insert t (KeyType.Varchar (String.make 4 'B')) 9999;
    Btree.insert t (KeyType.Varchar (String.make 4 'N')) 9999;
    Btree.insert t (KeyType.Varchar (String.make 4 'O')) 9999;
    Btree.insert t (KeyType.Varchar (String.make 4 'P')) 9999;

    Btree.insert t (KeyType.Varchar (String.make 4 'R')) 9999;

    Btree.insert t (KeyType.Varchar (String.make 4 'T')) 9999;

    Btree.insert t (KeyType.Varchar (String.make 4 'V')) 9999;

    Btree.insert t (KeyType.Varchar (String.make 4 'Y')) 9999;
    Btree.insert t (KeyType.Varchar (String.make 4 'H')) 9999;

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
    " "

  let insert_varchar2s () =
    let dir = "tmp_btree_insert_varchar2s" in
    let storage_manager = setup_test_env dir in
    let key_type = KeyType.TVarchar 2 in

    let t = Btree.create storage_manager key_type in

    (* create another node, so we can insert a new key, pointer pair into the root we created above*)
    let key_ty = t.key in
    let block_size = File.File_manager.get_blocksize t.sm.file_manager in

    let empty = Btree.empty_node t in
    Btree.insert t (KeyType.Varchar "AA") 9999;
    Btree.insert t (KeyType.Varchar "AB") 9999;
    Btree.insert t (KeyType.Varchar "AC") 9999;
    Btree.insert t (KeyType.Varchar "AD") 9999;
    Btree.insert t (KeyType.Varchar "AE") 9999;
    Btree.insert t (KeyType.Varchar "AF") 9999;
    Btree.insert t (KeyType.Varchar "AG") 9999;
    Btree.insert t (KeyType.Varchar "AH") 9999;
    Btree.insert t (KeyType.Varchar "AI") 9999;
    Btree.insert t (KeyType.Varchar "AJ") 9999;
    Btree.insert t (KeyType.Varchar "AK") 9999;
    Btree.insert t (KeyType.Varchar "AL") 9999;
    Btree.insert t (KeyType.Varchar "AM") 9999;
    Btree.insert t (KeyType.Varchar "AN") 9999;
    Btree.insert t (KeyType.Varchar "AO") 9999;
    Btree.insert t (KeyType.Varchar "AP") 9999;
    Btree.insert t (KeyType.Varchar "AQ") 9999;
    Btree.insert t (KeyType.Varchar "AR") 9999;
    Btree.insert t (KeyType.Varchar "AS") 9999;
    Btree.insert t (KeyType.Varchar "AT") 9999;
    Btree.insert t (KeyType.Varchar "AU") 9999;
    Btree.insert t (KeyType.Varchar "AV") 9999;
    Btree.insert t (KeyType.Varchar "AW") 9999;
    Btree.insert t (KeyType.Varchar "AX") 9999;
    Btree.insert t (KeyType.Varchar "AY") 9999;
    Btree.insert t (KeyType.Varchar "AZ") 9999;
    Btree.insert t (KeyType.Varchar "BA") 9999;
    Btree.insert t (KeyType.Varchar "BB") 9999;
    Btree.insert t (KeyType.Varchar "BC") 9999;
    Btree.insert t (KeyType.Varchar "BD") 9999;
    Btree.insert t (KeyType.Varchar "BE") 9999;
    Btree.insert t (KeyType.Varchar "BF") 9999;
    Btree.insert t (KeyType.Varchar "BG") 9999;
    Btree.insert t (KeyType.Varchar "BH") 9999;
    Btree.insert t (KeyType.Varchar "BI") 9999;
    Btree.insert t (KeyType.Varchar "BJ") 9999;
    Btree.insert t (KeyType.Varchar "BK") 9999;
    Btree.insert t (KeyType.Varchar "BL") 9999;
    Btree.insert t (KeyType.Varchar "BM") 9999;
    Btree.insert t (KeyType.Varchar "BN") 9999;
    Btree.insert t (KeyType.Varchar "BO") 9999;
    Btree.insert t (KeyType.Varchar "BP") 9999;
    Btree.insert t (KeyType.Varchar "BQ") 9999;
    Btree.insert t (KeyType.Varchar "BR") 9999;
    Btree.insert t (KeyType.Varchar "BS") 9999;
    Btree.insert t (KeyType.Varchar "BT") 9999;
    Btree.insert t (KeyType.Varchar "BU") 9999;
    Btree.insert t (KeyType.Varchar "BV") 9999;
    Btree.insert t (KeyType.Varchar "BW") 9999;
    Btree.insert t (KeyType.Varchar "BX") 9999;
    Btree.insert t (KeyType.Varchar "BY") 9999;
    Btree.insert t (KeyType.Varchar "BZ") 9999;
    Btree.insert t (KeyType.Varchar "CA") 9999;
    Btree.insert t (KeyType.Varchar "CB") 9999;
    Btree.insert t (KeyType.Varchar "CC") 9999;
    Btree.insert t (KeyType.Varchar "CD") 9999;
    Btree.insert t (KeyType.Varchar "CE") 9999;
    Btree.insert t (KeyType.Varchar "CF") 9999;
    Btree.insert t (KeyType.Varchar "CG") 9999;
    Btree.insert t (KeyType.Varchar "CH") 9999;
    Btree.insert t (KeyType.Varchar "CI") 9999;
    Btree.insert t (KeyType.Varchar "CJ") 9999;
    Btree.insert t (KeyType.Varchar "CK") 9999;
    Btree.insert t (KeyType.Varchar "CL") 9999;
    Btree.insert t (KeyType.Varchar "CM") 9999;
    Btree.insert t (KeyType.Varchar "CN") 9999;
    Btree.insert t (KeyType.Varchar "CO") 9999;
    Btree.insert t (KeyType.Varchar "CP") 9999;
    Btree.insert t (KeyType.Varchar "CQ") 9999;
    Btree.insert t (KeyType.Varchar "CR") 9999;
    Btree.insert t (KeyType.Varchar "CS") 9999;
    Btree.insert t (KeyType.Varchar "CT") 9999;
    Btree.insert t (KeyType.Varchar "CU") 9999;
    Btree.insert t (KeyType.Varchar "CV") 9999;
    Btree.insert t (KeyType.Varchar "CW") 9999;
    Btree.insert t (KeyType.Varchar "CX") 9999;
    Btree.insert t (KeyType.Varchar "CY") 9999;
    Btree.insert t (KeyType.Varchar "CZ") 9999;
    Btree.insert t (KeyType.Varchar "DA") 9999;
    Btree.insert t (KeyType.Varchar "DB") 9999;
    Btree.insert t (KeyType.Varchar "DC") 9999;
    Btree.insert t (KeyType.Varchar "DD") 9999;
    Btree.insert t (KeyType.Varchar "DE") 9999;
    Btree.insert t (KeyType.Varchar "DF") 9999;
    Btree.insert t (KeyType.Varchar "DG") 9999;
    Btree.insert t (KeyType.Varchar "DH") 9999;
    Btree.insert t (KeyType.Varchar "DI") 9999;
    Btree.insert t (KeyType.Varchar "DJ") 9999;
    Btree.insert t (KeyType.Varchar "DK") 9999;
    Btree.insert t (KeyType.Varchar "DL") 9999;
    Btree.insert t (KeyType.Varchar "DM") 9999;
    Btree.insert t (KeyType.Varchar "DN") 9999;
    Btree.insert t (KeyType.Varchar "DO") 9999;
    Btree.insert t (KeyType.Varchar "DP") 9999;
    Btree.insert t (KeyType.Varchar "DQ") 9999;
    Btree.insert t (KeyType.Varchar "DR") 9999;
    Btree.insert t (KeyType.Varchar "DS") 9999;
    Btree.insert t (KeyType.Varchar "DT") 9999;
    Btree.insert t (KeyType.Varchar "DU") 9999;
    Btree.insert t (KeyType.Varchar "DV") 9999;
    Btree.insert t (KeyType.Varchar "DW") 9999;
    Btree.insert t (KeyType.Varchar "DX") 9999;
    Btree.insert t (KeyType.Varchar "DY") 9999;
    Btree.insert t (KeyType.Varchar "DZ") 9999;
    Btree.insert t (KeyType.Varchar "EA") 9999;
    Btree.insert t (KeyType.Varchar "EB") 9999;
    Btree.insert t (KeyType.Varchar "EC") 9999;
    Btree.insert t (KeyType.Varchar "ED") 9999;
    Btree.insert t (KeyType.Varchar "EE") 9999;
    Btree.insert t (KeyType.Varchar "EF") 9999;
    Btree.insert t (KeyType.Varchar "EG") 9999;
    Btree.insert t (KeyType.Varchar "EH") 9999;
    Btree.insert t (KeyType.Varchar "EI") 9999;
    Btree.insert t (KeyType.Varchar "EJ") 9999;
    Btree.insert t (KeyType.Varchar "EK") 9999;
    Btree.insert t (KeyType.Varchar "EL") 9999;
    Btree.insert t (KeyType.Varchar "EM") 9999;
    Btree.insert t (KeyType.Varchar "EN") 9999;
    Btree.insert t (KeyType.Varchar "EO") 9999;
    Btree.insert t (KeyType.Varchar "EP") 9999;
    Btree.insert t (KeyType.Varchar "EQ") 9999;
    Btree.insert t (KeyType.Varchar "ER") 9999;
    Btree.insert t (KeyType.Varchar "ES") 9999;
    Btree.insert t (KeyType.Varchar "ET") 9999;
    Btree.insert t (KeyType.Varchar "EU") 9999;
    Btree.insert t (KeyType.Varchar "EV") 9999;
    Btree.insert t (KeyType.Varchar "EW") 9999;
    Btree.insert t (KeyType.Varchar "EX") 9999;
    Btree.insert t (KeyType.Varchar "EY") 9999;
    Btree.insert t (KeyType.Varchar "EZ") 9999;
    Btree.insert t (KeyType.Varchar "FA") 9999;
    Btree.insert t (KeyType.Varchar "FB") 9999;
    Btree.insert t (KeyType.Varchar "FC") 9999;
    Btree.insert t (KeyType.Varchar "FD") 9999;
    Btree.insert t (KeyType.Varchar "FE") 9999;
    Btree.insert t (KeyType.Varchar "FF") 9999;
    Btree.insert t (KeyType.Varchar "FG") 9999;
    Btree.insert t (KeyType.Varchar "FH") 9999;
    Btree.insert t (KeyType.Varchar "FI") 9999;
    Btree.insert t (KeyType.Varchar "FJ") 9999;
    Btree.insert t (KeyType.Varchar "FK") 9999;
    Btree.insert t (KeyType.Varchar "FL") 9999;
    Btree.insert t (KeyType.Varchar "FM") 9999;
    Btree.insert t (KeyType.Varchar "FN") 9999;
    Btree.insert t (KeyType.Varchar "FO") 9999;
    Btree.insert t (KeyType.Varchar "FP") 9999;
    Btree.insert t (KeyType.Varchar "FQ") 9999;
    Btree.insert t (KeyType.Varchar "FR") 9999;
    Btree.insert t (KeyType.Varchar "FS") 9999;
    Btree.insert t (KeyType.Varchar "FT") 9999;
    Btree.insert t (KeyType.Varchar "FU") 9999;
    Btree.insert t (KeyType.Varchar "FV") 9999;
    Btree.insert t (KeyType.Varchar "FW") 9999;
    Btree.insert t (KeyType.Varchar "FX") 9999;
    Btree.insert t (KeyType.Varchar "FY") 9999;
    Btree.insert t (KeyType.Varchar "FZ") 9999;
    Btree.insert t (KeyType.Varchar "GA") 9999;
    Btree.insert t (KeyType.Varchar "GB") 9999;
    Btree.insert t (KeyType.Varchar "GC") 9999;
    Btree.insert t (KeyType.Varchar "GD") 9999;
    Btree.insert t (KeyType.Varchar "GE") 9999;
    Btree.insert t (KeyType.Varchar "GF") 9999;
    Btree.insert t (KeyType.Varchar "GG") 9999;
    Btree.insert t (KeyType.Varchar "GH") 9999;
    Btree.insert t (KeyType.Varchar "GI") 9999;
    Btree.insert t (KeyType.Varchar "GJ") 9999;
    Btree.insert t (KeyType.Varchar "GK") 9999;
    Btree.insert t (KeyType.Varchar "GL") 9999;
    Btree.insert t (KeyType.Varchar "GM") 9999;
    Btree.insert t (KeyType.Varchar "GN") 9999;
    Btree.insert t (KeyType.Varchar "GO") 9999;
    Btree.insert t (KeyType.Varchar "GP") 9999;
    Btree.insert t (KeyType.Varchar "GQ") 9999;
    Btree.insert t (KeyType.Varchar "GR") 9999;
    Btree.insert t (KeyType.Varchar "GS") 9999;
    Btree.insert t (KeyType.Varchar "GT") 9999;
    Btree.insert t (KeyType.Varchar "GU") 9999;
    Btree.insert t (KeyType.Varchar "GV") 9999;
    Btree.insert t (KeyType.Varchar "GW") 9999;
    Btree.insert t (KeyType.Varchar "GX") 9999;
    Btree.insert t (KeyType.Varchar "GY") 9999;
    Btree.insert t (KeyType.Varchar "GZ") 9999;
    Btree.insert t (KeyType.Varchar "HA") 9999;
    Btree.insert t (KeyType.Varchar "HB") 9999;
    Btree.insert t (KeyType.Varchar "HC") 9999;
    Btree.insert t (KeyType.Varchar "HD") 9999;
    Btree.insert t (KeyType.Varchar "HE") 9999;
    Btree.insert t (KeyType.Varchar "HF") 9999;
    Btree.insert t (KeyType.Varchar "HG") 9999;
    Btree.insert t (KeyType.Varchar "HH") 9999;
    Btree.insert t (KeyType.Varchar "HI") 9999;
    Btree.insert t (KeyType.Varchar "HJ") 9999;
    Btree.insert t (KeyType.Varchar "HK") 9999;
    Btree.insert t (KeyType.Varchar "HL") 9999;
    Btree.insert t (KeyType.Varchar "HM") 9999;
    Btree.insert t (KeyType.Varchar "HN") 9999;
    Btree.insert t (KeyType.Varchar "HO") 9999;
    Btree.insert t (KeyType.Varchar "HP") 9999;
    Btree.insert t (KeyType.Varchar "HQ") 9999;
    Btree.insert t (KeyType.Varchar "HR") 9999;
    Btree.insert t (KeyType.Varchar "HS") 9999;
    Btree.insert t (KeyType.Varchar "HT") 9999;
    Btree.insert t (KeyType.Varchar "HU") 9999;
    Btree.insert t (KeyType.Varchar "HV") 9999;
    Btree.insert t (KeyType.Varchar "HW") 9999;
    Btree.insert t (KeyType.Varchar "HX") 9999;
    Btree.insert t (KeyType.Varchar "HY") 9999;
    Btree.insert t (KeyType.Varchar "HZ") 9999;
    Btree.insert t (KeyType.Varchar "IA") 9999;
    Btree.insert t (KeyType.Varchar "IB") 9999;
    Btree.insert t (KeyType.Varchar "IC") 9999;
    Btree.insert t (KeyType.Varchar "ID") 9999;
    Btree.insert t (KeyType.Varchar "IE") 9999;
    Btree.insert t (KeyType.Varchar "IF") 9999;
    Btree.insert t (KeyType.Varchar "IG") 9999;
    Btree.insert t (KeyType.Varchar "IH") 9999;
    Btree.insert t (KeyType.Varchar "II") 9999;
    Btree.insert t (KeyType.Varchar "IJ") 9999;
    Btree.insert t (KeyType.Varchar "IK") 9999;
    Btree.insert t (KeyType.Varchar "IL") 9999;
    Btree.insert t (KeyType.Varchar "IM") 9999;
    Btree.insert t (KeyType.Varchar "IN") 9999;
    Btree.insert t (KeyType.Varchar "IO") 9999;
    Btree.insert t (KeyType.Varchar "IP") 9999;
    Btree.insert t (KeyType.Varchar "IQ") 9999;
    Btree.insert t (KeyType.Varchar "IR") 9999;
    Btree.insert t (KeyType.Varchar "IS") 9999;
    Btree.insert t (KeyType.Varchar "IT") 9999;
    Btree.insert t (KeyType.Varchar "IU") 9999;
    Btree.insert t (KeyType.Varchar "IV") 9999;
    Btree.insert t (KeyType.Varchar "IW") 9999;
    Btree.insert t (KeyType.Varchar "IX") 9999;
    Btree.insert t (KeyType.Varchar "IY") 9999;
    Btree.insert t (KeyType.Varchar "IZ") 9999;
    Btree.insert t (KeyType.Varchar "JA") 9999;
    Btree.insert t (KeyType.Varchar "JB") 9999;
    Btree.insert t (KeyType.Varchar "JC") 9999;
    Btree.insert t (KeyType.Varchar "JD") 9999;
    Btree.insert t (KeyType.Varchar "JE") 9999;
    Btree.insert t (KeyType.Varchar "JF") 9999;
    Btree.insert t (KeyType.Varchar "JG") 9999;
    Btree.insert t (KeyType.Varchar "JH") 9999;
    Btree.insert t (KeyType.Varchar "JI") 9999;
    Btree.insert t (KeyType.Varchar "JJ") 9999;
    Btree.insert t (KeyType.Varchar "JK") 9999;
    Btree.insert t (KeyType.Varchar "JL") 9999;
    Btree.insert t (KeyType.Varchar "JM") 9999;
    Btree.insert t (KeyType.Varchar "JN") 9999;
    Btree.insert t (KeyType.Varchar "JO") 9999;
    Btree.insert t (KeyType.Varchar "JP") 9999;
    Btree.insert t (KeyType.Varchar "JQ") 9999;
    Btree.insert t (KeyType.Varchar "JR") 9999;
    Btree.insert t (KeyType.Varchar "JS") 9999;
    Btree.insert t (KeyType.Varchar "JT") 9999;
    Btree.insert t (KeyType.Varchar "JU") 9999;
    Btree.insert t (KeyType.Varchar "JV") 9999;
    Btree.insert t (KeyType.Varchar "JW") 9999;
    Btree.insert t (KeyType.Varchar "JX") 9999;
    Btree.insert t (KeyType.Varchar "JY") 9999;
    Btree.insert t (KeyType.Varchar "JZ") 9999;
    Btree.insert t (KeyType.Varchar "KA") 9999;
    Btree.insert t (KeyType.Varchar "KB") 9999;
    Btree.insert t (KeyType.Varchar "KC") 9999;
    Btree.insert t (KeyType.Varchar "KD") 9999;
    Btree.insert t (KeyType.Varchar "KE") 9999;
    Btree.insert t (KeyType.Varchar "KF") 9999;
    Btree.insert t (KeyType.Varchar "KG") 9999;
    Btree.insert t (KeyType.Varchar "KH") 9999;
    Btree.insert t (KeyType.Varchar "KI") 9999;
    Btree.insert t (KeyType.Varchar "KJ") 9999;
    Btree.insert t (KeyType.Varchar "KK") 9999;
    Btree.insert t (KeyType.Varchar "KL") 9999;
    Btree.insert t (KeyType.Varchar "KM") 9999;
    Btree.insert t (KeyType.Varchar "KN") 9999;
    Btree.insert t (KeyType.Varchar "KO") 9999;
    Btree.insert t (KeyType.Varchar "KP") 9999;
    Btree.insert t (KeyType.Varchar "KQ") 9999;
    Btree.insert t (KeyType.Varchar "KR") 9999;
    Btree.insert t (KeyType.Varchar "KS") 9999;
    Btree.insert t (KeyType.Varchar "KT") 9999;
    Btree.insert t (KeyType.Varchar "KU") 9999;
    Btree.insert t (KeyType.Varchar "KV") 9999;
    Btree.insert t (KeyType.Varchar "KW") 9999;
    Btree.insert t (KeyType.Varchar "KX") 9999;
    Btree.insert t (KeyType.Varchar "KY") 9999;
    Btree.insert t (KeyType.Varchar "KZ") 9999;
    Btree.insert t (KeyType.Varchar "LA") 9999;
    Btree.insert t (KeyType.Varchar "LB") 9999;
    Btree.insert t (KeyType.Varchar "LC") 9999;
    Btree.insert t (KeyType.Varchar "LD") 9999;
    Btree.insert t (KeyType.Varchar "LE") 9999;
    Btree.insert t (KeyType.Varchar "LF") 9999;
    Btree.insert t (KeyType.Varchar "LG") 9999;
    Btree.insert t (KeyType.Varchar "LH") 9999;
    Btree.insert t (KeyType.Varchar "LI") 9999;
    Btree.insert t (KeyType.Varchar "LJ") 9999;
    Btree.insert t (KeyType.Varchar "LK") 9999;
    Btree.insert t (KeyType.Varchar "LL") 9999;
    Btree.insert t (KeyType.Varchar "LM") 9999;
    Btree.insert t (KeyType.Varchar "LN") 9999;
    Btree.insert t (KeyType.Varchar "LO") 9999;
    Btree.insert t (KeyType.Varchar "LP") 9999;
    Btree.insert t (KeyType.Varchar "LQ") 9999;
    Btree.insert t (KeyType.Varchar "LR") 9999;
    Btree.insert t (KeyType.Varchar "LS") 9999;
    Btree.insert t (KeyType.Varchar "LT") 9999;
    Btree.insert t (KeyType.Varchar "LU") 9999;
    Btree.insert t (KeyType.Varchar "LV") 9999;
    Btree.insert t (KeyType.Varchar "LW") 9999;
    Btree.insert t (KeyType.Varchar "LX") 9999;
    Btree.insert t (KeyType.Varchar "LY") 9999;
    Btree.insert t (KeyType.Varchar "LZ") 9999;
    Btree.insert t (KeyType.Varchar "MA") 9999;
    Btree.insert t (KeyType.Varchar "MB") 9999;
    Btree.insert t (KeyType.Varchar "MC") 9999;
    Btree.insert t (KeyType.Varchar "MD") 9999;
    Btree.insert t (KeyType.Varchar "ME") 9999;
    Btree.insert t (KeyType.Varchar "MF") 9999;
    Btree.insert t (KeyType.Varchar "MG") 9999;
    Btree.insert t (KeyType.Varchar "MH") 9999;
    Btree.insert t (KeyType.Varchar "MI") 9999;
    Btree.insert t (KeyType.Varchar "MJ") 9999;
    Btree.insert t (KeyType.Varchar "MK") 9999;
    Btree.insert t (KeyType.Varchar "ML") 9999;
    Btree.insert t (KeyType.Varchar "MM") 9999;
    Btree.insert t (KeyType.Varchar "MN") 9999;
    Btree.insert t (KeyType.Varchar "MO") 9999;
    Btree.insert t (KeyType.Varchar "MP") 9999;
    Btree.insert t (KeyType.Varchar "MQ") 9999;
    Btree.insert t (KeyType.Varchar "MR") 9999;
    Btree.insert t (KeyType.Varchar "MS") 9999;
    Btree.insert t (KeyType.Varchar "MT") 9999;
    Btree.insert t (KeyType.Varchar "MU") 9999;
    Btree.insert t (KeyType.Varchar "MV") 9999;
    Btree.insert t (KeyType.Varchar "MW") 9999;
    Btree.insert t (KeyType.Varchar "MX") 9999;
    Btree.insert t (KeyType.Varchar "MY") 9999;
    Btree.insert t (KeyType.Varchar "MZ") 9999;
    Btree.insert t (KeyType.Varchar "NA") 9999;
    Btree.insert t (KeyType.Varchar "NB") 9999;
    Btree.insert t (KeyType.Varchar "NC") 9999;
    Btree.insert t (KeyType.Varchar "ND") 9999;
    Btree.insert t (KeyType.Varchar "NE") 9999;
    Btree.insert t (KeyType.Varchar "NF") 9999;
    Btree.insert t (KeyType.Varchar "NG") 9999;
    Btree.insert t (KeyType.Varchar "NH") 9999;
    Btree.insert t (KeyType.Varchar "NI") 9999;
    Btree.insert t (KeyType.Varchar "NJ") 9999;
    Btree.insert t (KeyType.Varchar "NK") 9999;
    Btree.insert t (KeyType.Varchar "NL") 9999;
    Btree.insert t (KeyType.Varchar "NM") 9999;
    Btree.insert t (KeyType.Varchar "NN") 9999;
    Btree.insert t (KeyType.Varchar "NO") 9999;
    Btree.insert t (KeyType.Varchar "NP") 9999;
    Btree.insert t (KeyType.Varchar "NQ") 9999;
    Btree.insert t (KeyType.Varchar "NR") 9999;
    Btree.insert t (KeyType.Varchar "NS") 9999;
    Btree.insert t (KeyType.Varchar "NT") 9999;
    Btree.insert t (KeyType.Varchar "NU") 9999;
    Btree.insert t (KeyType.Varchar "NV") 9999;
    Btree.insert t (KeyType.Varchar "NW") 9999;
    Btree.insert t (KeyType.Varchar "NX") 9999;
    Btree.insert t (KeyType.Varchar "NY") 9999;
    Btree.insert t (KeyType.Varchar "NZ") 9999;
    Btree.insert t (KeyType.Varchar "OA") 9999;
    Btree.insert t (KeyType.Varchar "OB") 9999;
    Btree.insert t (KeyType.Varchar "OC") 9999;
    Btree.insert t (KeyType.Varchar "OD") 9999;
    Btree.insert t (KeyType.Varchar "OE") 9999;
    Btree.insert t (KeyType.Varchar "OF") 9999;
    Btree.insert t (KeyType.Varchar "OG") 9999;
    Btree.insert t (KeyType.Varchar "OH") 9999;
    Btree.insert t (KeyType.Varchar "OI") 9999;
    Btree.insert t (KeyType.Varchar "OJ") 9999;
    Btree.insert t (KeyType.Varchar "OK") 9999;
    Btree.insert t (KeyType.Varchar "OL") 9999;
    Btree.insert t (KeyType.Varchar "OM") 9999;
    Btree.insert t (KeyType.Varchar "ON") 9999;
    Btree.insert t (KeyType.Varchar "OO") 9999;
    Btree.insert t (KeyType.Varchar "OP") 9999;
    Btree.insert t (KeyType.Varchar "OQ") 9999;
    Btree.insert t (KeyType.Varchar "OR") 9999;
    Btree.insert t (KeyType.Varchar "OS") 9999;
    Btree.insert t (KeyType.Varchar "OT") 9999;
    Btree.insert t (KeyType.Varchar "OU") 9999;
    Btree.insert t (KeyType.Varchar "OV") 9999;
    Btree.insert t (KeyType.Varchar "OW") 9999;
    Btree.insert t (KeyType.Varchar "OX") 9999;
    Btree.insert t (KeyType.Varchar "OY") 9999;
    Btree.insert t (KeyType.Varchar "OZ") 9999;
    Btree.insert t (KeyType.Varchar "PA") 9999;
    Btree.insert t (KeyType.Varchar "PB") 9999;
    Btree.insert t (KeyType.Varchar "PC") 9999;
    Btree.insert t (KeyType.Varchar "PD") 9999;
    Btree.insert t (KeyType.Varchar "PE") 9999;
    Btree.insert t (KeyType.Varchar "PF") 9999;
    Btree.insert t (KeyType.Varchar "PG") 9999;
    Btree.insert t (KeyType.Varchar "PH") 9999;
    Btree.insert t (KeyType.Varchar "PI") 9999;
    Btree.insert t (KeyType.Varchar "PJ") 9999;
    Btree.insert t (KeyType.Varchar "PK") 9999;
    Btree.insert t (KeyType.Varchar "PL") 9999;
    Btree.insert t (KeyType.Varchar "PM") 9999;
    Btree.insert t (KeyType.Varchar "PN") 9999;
    Btree.insert t (KeyType.Varchar "PO") 9999;
    Btree.insert t (KeyType.Varchar "PP") 9999;
    Btree.insert t (KeyType.Varchar "PQ") 9999;
    Btree.insert t (KeyType.Varchar "PR") 9999;
    Btree.insert t (KeyType.Varchar "PS") 9999;
    Btree.insert t (KeyType.Varchar "PT") 9999;
    Btree.insert t (KeyType.Varchar "PU") 9999;
    Btree.insert t (KeyType.Varchar "PV") 9999;
    Btree.insert t (KeyType.Varchar "PW") 9999;
    Btree.insert t (KeyType.Varchar "PX") 9999;
    Btree.insert t (KeyType.Varchar "PY") 9999;
    Btree.insert t (KeyType.Varchar "PZ") 9999;
    Btree.insert t (KeyType.Varchar "QA") 9999;
    Btree.insert t (KeyType.Varchar "QB") 9999;
    Btree.insert t (KeyType.Varchar "QC") 9999;
    Btree.insert t (KeyType.Varchar "QD") 9999;
    Btree.insert t (KeyType.Varchar "QE") 9999;
    Btree.insert t (KeyType.Varchar "QF") 9999;
    Btree.insert t (KeyType.Varchar "QG") 9999;
    Btree.insert t (KeyType.Varchar "QH") 9999;
    Btree.insert t (KeyType.Varchar "QI") 9999;
    Btree.insert t (KeyType.Varchar "QJ") 9999;
    Btree.insert t (KeyType.Varchar "QK") 9999;
    Btree.insert t (KeyType.Varchar "QL") 9999;
    Btree.insert t (KeyType.Varchar "QM") 9999;
    Btree.insert t (KeyType.Varchar "QN") 9999;
    Btree.insert t (KeyType.Varchar "QO") 9999;
    Btree.insert t (KeyType.Varchar "QP") 9999;
    Btree.insert t (KeyType.Varchar "QQ") 9999;
    Btree.insert t (KeyType.Varchar "QR") 9999;
    Btree.insert t (KeyType.Varchar "QS") 9999;
    Btree.insert t (KeyType.Varchar "QT") 9999;
    Btree.insert t (KeyType.Varchar "QU") 9999;
    Btree.insert t (KeyType.Varchar "QV") 9999;
    Btree.insert t (KeyType.Varchar "QW") 9999;
    Btree.insert t (KeyType.Varchar "QX") 9999;
    Btree.insert t (KeyType.Varchar "QY") 9999;
    Btree.insert t (KeyType.Varchar "QZ") 9999;
    Btree.insert t (KeyType.Varchar "RA") 9999;
    Btree.insert t (KeyType.Varchar "RB") 9999;
    Btree.insert t (KeyType.Varchar "RC") 9999;
    Btree.insert t (KeyType.Varchar "RD") 9999;
    Btree.insert t (KeyType.Varchar "RE") 9999;
    Btree.insert t (KeyType.Varchar "RF") 9999;
    Btree.insert t (KeyType.Varchar "RG") 9999;
    Btree.insert t (KeyType.Varchar "RH") 9999;
    Btree.insert t (KeyType.Varchar "RI") 9999;
    Btree.insert t (KeyType.Varchar "RJ") 9999;
    Btree.insert t (KeyType.Varchar "RK") 9999;
    Btree.insert t (KeyType.Varchar "RL") 9999;
    Btree.insert t (KeyType.Varchar "RM") 9999;
    Btree.insert t (KeyType.Varchar "RN") 9999;
    Btree.insert t (KeyType.Varchar "RO") 9999;
    Btree.insert t (KeyType.Varchar "RP") 9999;
    Btree.insert t (KeyType.Varchar "RQ") 9999;
    Btree.insert t (KeyType.Varchar "RR") 9999;
    Btree.insert t (KeyType.Varchar "RS") 9999;
    Btree.insert t (KeyType.Varchar "RT") 9999;
    Btree.insert t (KeyType.Varchar "RU") 9999;
    Btree.insert t (KeyType.Varchar "RV") 9999;
    Btree.insert t (KeyType.Varchar "RW") 9999;
    Btree.insert t (KeyType.Varchar "RX") 9999;
    Btree.insert t (KeyType.Varchar "RY") 9999;
    Btree.insert t (KeyType.Varchar "RZ") 9999;
    Btree.insert t (KeyType.Varchar "SA") 9999;
    Btree.insert t (KeyType.Varchar "SB") 9999;
    Btree.insert t (KeyType.Varchar "SC") 9999;
    Btree.insert t (KeyType.Varchar "SD") 9999;
    Btree.insert t (KeyType.Varchar "SE") 9999;
    Btree.insert t (KeyType.Varchar "SF") 9999;
    Btree.insert t (KeyType.Varchar "SG") 9999;
    Btree.insert t (KeyType.Varchar "SH") 9999;
    Btree.insert t (KeyType.Varchar "SI") 9999;
    Btree.insert t (KeyType.Varchar "SJ") 9999;
    Btree.insert t (KeyType.Varchar "SK") 9999;
    Btree.insert t (KeyType.Varchar "SL") 9999;
    Btree.insert t (KeyType.Varchar "SM") 9999;
    Btree.insert t (KeyType.Varchar "SN") 9999;
    Btree.insert t (KeyType.Varchar "SO") 9999;
    Btree.insert t (KeyType.Varchar "SP") 9999;
    Btree.insert t (KeyType.Varchar "SQ") 9999;
    Btree.insert t (KeyType.Varchar "SR") 9999;
    Btree.insert t (KeyType.Varchar "SS") 9999;
    Btree.insert t (KeyType.Varchar "ST") 9999;
    Btree.insert t (KeyType.Varchar "SU") 9999;
    Btree.insert t (KeyType.Varchar "SV") 9999;
    Btree.insert t (KeyType.Varchar "SW") 9999;
    Btree.insert t (KeyType.Varchar "SX") 9999;
    Btree.insert t (KeyType.Varchar "SY") 9999;
    Btree.insert t (KeyType.Varchar "SZ") 9999;
    Btree.insert t (KeyType.Varchar "TA") 9999;
    Btree.insert t (KeyType.Varchar "TB") 9999;
    Btree.insert t (KeyType.Varchar "TC") 9999;
    Btree.insert t (KeyType.Varchar "TD") 9999;
    Btree.insert t (KeyType.Varchar "TE") 9999;
    Btree.insert t (KeyType.Varchar "TF") 9999;
    Btree.insert t (KeyType.Varchar "TG") 9999;
    Btree.insert t (KeyType.Varchar "TH") 9999;
    Btree.insert t (KeyType.Varchar "TI") 9999;
    Btree.insert t (KeyType.Varchar "TJ") 9999;
    Btree.insert t (KeyType.Varchar "TK") 9999;
    Btree.insert t (KeyType.Varchar "TL") 9999;
    Btree.insert t (KeyType.Varchar "TM") 9999;
    Btree.insert t (KeyType.Varchar "TN") 9999;
    Btree.insert t (KeyType.Varchar "TO") 9999;
    Btree.insert t (KeyType.Varchar "TP") 9999;
    Btree.insert t (KeyType.Varchar "TQ") 9999;
    Btree.insert t (KeyType.Varchar "TR") 9999;
    Btree.insert t (KeyType.Varchar "TS") 9999;
    Btree.insert t (KeyType.Varchar "TT") 9999;
    Btree.insert t (KeyType.Varchar "TU") 9999;
    Btree.insert t (KeyType.Varchar "TV") 9999;
    Btree.insert t (KeyType.Varchar "TW") 9999;
    Btree.insert t (KeyType.Varchar "TX") 9999;
    Btree.insert t (KeyType.Varchar "TY") 9999;
    Btree.insert t (KeyType.Varchar "TZ") 9999;
    Btree.insert t (KeyType.Varchar "UA") 9999;
    Btree.insert t (KeyType.Varchar "UB") 9999;
    Btree.insert t (KeyType.Varchar "UC") 9999;
    Btree.insert t (KeyType.Varchar "UD") 9999;
    Btree.insert t (KeyType.Varchar "UE") 9999;
    Btree.insert t (KeyType.Varchar "UF") 9999;
    Btree.insert t (KeyType.Varchar "UG") 9999;
    Btree.insert t (KeyType.Varchar "UH") 9999;
    Btree.insert t (KeyType.Varchar "UI") 9999;
    Btree.insert t (KeyType.Varchar "UJ") 9999;
    Btree.insert t (KeyType.Varchar "UK") 9999;
    Btree.insert t (KeyType.Varchar "UL") 9999;
    Btree.insert t (KeyType.Varchar "UM") 9999;
    Btree.insert t (KeyType.Varchar "UN") 9999;
    Btree.insert t (KeyType.Varchar "UO") 9999;
    Btree.insert t (KeyType.Varchar "UP") 9999;
    Btree.insert t (KeyType.Varchar "UQ") 9999;
    Btree.insert t (KeyType.Varchar "UR") 9999;
    Btree.insert t (KeyType.Varchar "US") 9999;
    Btree.insert t (KeyType.Varchar "UT") 9999;
    Btree.insert t (KeyType.Varchar "UU") 9999;
    Btree.insert t (KeyType.Varchar "UV") 9999;
    Btree.insert t (KeyType.Varchar "UW") 9999;
    Btree.insert t (KeyType.Varchar "UX") 9999;
    Btree.insert t (KeyType.Varchar "UY") 9999;
    Btree.insert t (KeyType.Varchar "UZ") 9999;
    Btree.insert t (KeyType.Varchar "VA") 9999;
    Btree.insert t (KeyType.Varchar "VB") 9999;
    Btree.insert t (KeyType.Varchar "VC") 9999;
    Btree.insert t (KeyType.Varchar "VD") 9999;
    Btree.insert t (KeyType.Varchar "VE") 9999;
    Btree.insert t (KeyType.Varchar "VF") 9999;
    Btree.insert t (KeyType.Varchar "VG") 9999;
    Btree.insert t (KeyType.Varchar "VH") 9999;
    Btree.insert t (KeyType.Varchar "VI") 9999;
    Btree.insert t (KeyType.Varchar "VJ") 9999;
    Btree.insert t (KeyType.Varchar "VK") 9999;
    Btree.insert t (KeyType.Varchar "VL") 9999;
    Btree.insert t (KeyType.Varchar "VM") 9999;
    Btree.insert t (KeyType.Varchar "VN") 9999;
    Btree.insert t (KeyType.Varchar "VO") 9999;
    Btree.insert t (KeyType.Varchar "VP") 9999;
    Btree.insert t (KeyType.Varchar "VQ") 9999;
    Btree.insert t (KeyType.Varchar "VR") 9999;
    Btree.insert t (KeyType.Varchar "VS") 9999;
    Btree.insert t (KeyType.Varchar "VT") 9999;
    Btree.insert t (KeyType.Varchar "VU") 9999;
    Btree.insert t (KeyType.Varchar "VV") 9999;
    Btree.insert t (KeyType.Varchar "VW") 9999;
    Btree.insert t (KeyType.Varchar "VX") 9999;
    Btree.insert t (KeyType.Varchar "VY") 9999;
    Btree.insert t (KeyType.Varchar "VZ") 9999;
    Btree.insert t (KeyType.Varchar "WA") 9999;
    Btree.insert t (KeyType.Varchar "WB") 9999;
    Btree.insert t (KeyType.Varchar "WC") 9999;
    Btree.insert t (KeyType.Varchar "WD") 9999;
    Btree.insert t (KeyType.Varchar "WE") 9999;
    Btree.insert t (KeyType.Varchar "WF") 9999;
    Btree.insert t (KeyType.Varchar "WG") 9999;
    Btree.insert t (KeyType.Varchar "WH") 9999;
    Btree.insert t (KeyType.Varchar "WI") 9999;
    Btree.insert t (KeyType.Varchar "WJ") 9999;
    Btree.insert t (KeyType.Varchar "WK") 9999;
    Btree.insert t (KeyType.Varchar "WL") 9999;
    Btree.insert t (KeyType.Varchar "WM") 9999;
    Btree.insert t (KeyType.Varchar "WN") 9999;
    Btree.insert t (KeyType.Varchar "WO") 9999;
    Btree.insert t (KeyType.Varchar "WP") 9999;
    Btree.insert t (KeyType.Varchar "WQ") 9999;
    Btree.insert t (KeyType.Varchar "WR") 9999;
    Btree.insert t (KeyType.Varchar "WS") 9999;
    Btree.insert t (KeyType.Varchar "WT") 9999;
    Btree.insert t (KeyType.Varchar "WU") 9999;
    Btree.insert t (KeyType.Varchar "WV") 9999;
    Btree.insert t (KeyType.Varchar "WW") 9999;
    Btree.insert t (KeyType.Varchar "WX") 9999;
    Btree.insert t (KeyType.Varchar "WY") 9999;
    Btree.insert t (KeyType.Varchar "WZ") 9999;
    Btree.insert t (KeyType.Varchar "XA") 9999;
    Btree.insert t (KeyType.Varchar "XB") 9999;
    Btree.insert t (KeyType.Varchar "XC") 9999;
    Btree.insert t (KeyType.Varchar "XD") 9999;
    Btree.insert t (KeyType.Varchar "XE") 9999;
    Btree.insert t (KeyType.Varchar "XF") 9999;
    Btree.insert t (KeyType.Varchar "XG") 9999;
    Btree.insert t (KeyType.Varchar "XH") 9999;
    Btree.insert t (KeyType.Varchar "XI") 9999;
    Btree.insert t (KeyType.Varchar "XJ") 9999;
    Btree.insert t (KeyType.Varchar "XK") 9999;
    Btree.insert t (KeyType.Varchar "XL") 9999;
    Btree.insert t (KeyType.Varchar "XM") 9999;
    Btree.insert t (KeyType.Varchar "XN") 9999;
    Btree.insert t (KeyType.Varchar "XO") 9999;
    Btree.insert t (KeyType.Varchar "XP") 9999;
    Btree.insert t (KeyType.Varchar "XQ") 9999;
    Btree.insert t (KeyType.Varchar "XR") 9999;
    Btree.insert t (KeyType.Varchar "XS") 9999;
    Btree.insert t (KeyType.Varchar "XT") 9999;
    Btree.insert t (KeyType.Varchar "XU") 9999;
    Btree.insert t (KeyType.Varchar "XV") 9999;
    Btree.insert t (KeyType.Varchar "XW") 9999;
    Btree.insert t (KeyType.Varchar "XX") 9999;
    Btree.insert t (KeyType.Varchar "XY") 9999;
    Btree.insert t (KeyType.Varchar "XZ") 9999;
    Btree.insert t (KeyType.Varchar "YA") 9999;
    Btree.insert t (KeyType.Varchar "YB") 9999;
    Btree.insert t (KeyType.Varchar "YC") 9999;
    Btree.insert t (KeyType.Varchar "YD") 9999;
    Btree.insert t (KeyType.Varchar "YE") 9999;
    Btree.insert t (KeyType.Varchar "YF") 9999;
    Btree.insert t (KeyType.Varchar "YG") 9999;
    Btree.insert t (KeyType.Varchar "YH") 9999;
    Btree.insert t (KeyType.Varchar "YI") 9999;
    Btree.insert t (KeyType.Varchar "YJ") 9999;
    Btree.insert t (KeyType.Varchar "YK") 9999;
    Btree.insert t (KeyType.Varchar "YL") 9999;
    Btree.insert t (KeyType.Varchar "YM") 9999;
    Btree.insert t (KeyType.Varchar "YN") 9999;
    Btree.insert t (KeyType.Varchar "YO") 9999;
    Btree.insert t (KeyType.Varchar "YP") 9999;
    Btree.insert t (KeyType.Varchar "YQ") 9999;
    Btree.insert t (KeyType.Varchar "YR") 9999;
    Btree.insert t (KeyType.Varchar "YS") 9999;
    Btree.insert t (KeyType.Varchar "YT") 9999;
    Btree.insert t (KeyType.Varchar "YU") 9999;
    Btree.insert t (KeyType.Varchar "YV") 9999;
    Btree.insert t (KeyType.Varchar "YW") 9999;
    Btree.insert t (KeyType.Varchar "YX") 9999;
    Btree.insert t (KeyType.Varchar "YY") 9999;
    Btree.insert t (KeyType.Varchar "YZ") 9999;
    Btree.insert t (KeyType.Varchar "ZA") 9999;
    Btree.insert t (KeyType.Varchar "ZB") 9999;
    Btree.insert t (KeyType.Varchar "ZC") 9999;
    Btree.insert t (KeyType.Varchar "ZD") 9999;
    Btree.insert t (KeyType.Varchar "ZE") 9999;
    Btree.insert t (KeyType.Varchar "ZF") 9999;
    Btree.insert t (KeyType.Varchar "ZG") 9999;
    Btree.insert t (KeyType.Varchar "ZH") 9999;
    Btree.insert t (KeyType.Varchar "ZI") 9999;
    Btree.insert t (KeyType.Varchar "ZJ") 9999;
    Btree.insert t (KeyType.Varchar "ZK") 9999;
    Btree.insert t (KeyType.Varchar "ZL") 9999;
    Btree.insert t (KeyType.Varchar "ZM") 9999;
    Btree.insert t (KeyType.Varchar "ZN") 9999;
    Btree.insert t (KeyType.Varchar "ZO") 9999;
    Btree.insert t (KeyType.Varchar "ZP") 9999;
    Btree.insert t (KeyType.Varchar "ZQ") 9999;
    Btree.insert t (KeyType.Varchar "ZR") 9999;
    Btree.insert t (KeyType.Varchar "ZS") 9999;
    Btree.insert t (KeyType.Varchar "ZT") 9999;
    Btree.insert t (KeyType.Varchar "ZU") 9999;
    Btree.insert t (KeyType.Varchar "ZV") 9999;
    Btree.insert t (KeyType.Varchar "ZW") 9999;
    Btree.insert t (KeyType.Varchar "ZX") 9999;
    Btree.insert t (KeyType.Varchar "ZY") 9999;
    Btree.insert t (KeyType.Varchar "ZZ") 9999;

    let graphviz_str = Btree.create_graphviz_str t t.root_num in
    Printf.printf "%s" graphviz_str;
    " "

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
