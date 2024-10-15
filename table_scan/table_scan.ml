module Layout = Record_page__Layout
module Schema = Record_page__Schema

type t = {
  tx: Transaction.t;
  layout: Layout.t;
  mutable rec_page: Record_page.t;
  file_name: string;
  mutable cur_slot: int;
}

let make ~tx ~tbl_name ~layout =
  let file_name = tbl_name ^ ".tbl" in 
  if Transaction.size ~tx ~filename:file_name = 0
  then
    let block = Transaction.append ~tx ~filename:file_name in
    let rec_page = Record_page.make tx block layout in
    Record_page.format rec_page;
    {tx; layout; rec_page; file_name; cur_slot=(-1);}
  else
    let block = File.Block_id.make ~filename:file_name ~block_num:0 in
    let rec_page = Record_page.make tx block layout in
    {tx; layout; rec_page; file_name; cur_slot=(-1);}

let close ~tbl_scan =
  let block = Record_page.block tbl_scan.rec_page in
  Transaction.unpin ~tx:tbl_scan.tx ~block

let move_to_block ~tbl_scan ~block_num =
  close ~tbl_scan;
  let block = File.Block_id.make ~filename:tbl_scan.file_name ~block_num in
  tbl_scan.rec_page <- Record_page.make tbl_scan.tx block tbl_scan.layout;
  tbl_scan.cur_slot <- (-1)

let move_to_new_block ~tbl_scan =
  close ~tbl_scan;
  let block = Transaction.append ~tx:tbl_scan.tx ~filename:tbl_scan.file_name
  in
  tbl_scan.rec_page <- Record_page.make tbl_scan.tx block tbl_scan.layout;
  Record_page.format tbl_scan.rec_page;
  tbl_scan.cur_slot <- (-1)

let get_block_num ~tbl_scan =
  let block = Record_page.block (tbl_scan.rec_page) in
  File.Block_id.block_num block

let at_last_block ~tbl_scan =
  let block_num = get_block_num ~tbl_scan in 
  let size = Transaction.size ~tx:tbl_scan.tx ~filename:tbl_scan.file_name in
  block_num = size

let get_rid ~tbl_scan =
  let block_num = get_block_num ~tbl_scan in
  Record_id.make ~block_num ~slot:tbl_scan.cur_slot 
        
let move_to_rid ~tbl_scan ~rid =
  close ~tbl_scan;
  let block = File.Block_id.make ~filename:tbl_scan.file_name
      ~block_num:(Record_id.get_block_num ~rid) in
  tbl_scan.rec_page <- Record_page.make tbl_scan.tx block tbl_scan.layout;
  tbl_scan.cur_slot <- Record_id.get_slot ~rid
