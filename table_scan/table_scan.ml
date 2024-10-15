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

let delete ~tbl_scan =
  Record_page.delete tbl_scan.rec_page tbl_scan.cur_slot

let insert ~tbl_scan =
  tbl_scan.cur_slot <-
    (Record_page.insert_after tbl_scan.rec_page tbl_scan.cur_slot);
  while (tbl_scan.cur_slot < 0) do
    if at_last_block ~tbl_scan
    then
      move_to_new_block ~tbl_scan
    else (
      move_to_block ~tbl_scan ~block_num:((get_block_num ~tbl_scan)+1)
    );
    tbl_scan.cur_slot <-
      Record_page.insert_after tbl_scan.rec_page tbl_scan.cur_slot
  done

let set_string ~tbl_scan ~field_name ~value =
  Record_page.set_string tbl_scan.rec_page tbl_scan.cur_slot field_name value

let set_int32 ~tbl_scan ~field_name ~value =
  Record_page.set_int32 tbl_scan.rec_page tbl_scan.cur_slot field_name value

let set_val ~tbl_scan ~field_name ~value =
  match value with
  | Constant.Integer v -> set_int32 ~tbl_scan ~field_name ~value:v
  | Constant.String v -> set_string ~tbl_scan ~field_name ~value:v

let get_int32 ~tbl_scan ~field_name =
  Record_page.get_int32 tbl_scan.rec_page tbl_scan.cur_slot field_name

let get_string ~tbl_scan ~field_name =
  Record_page.get_string tbl_scan.rec_page tbl_scan.cur_slot field_name

let get_val ~tbl_scan ~field_name =
  let schema = Layout.get_schema tbl_scan.layout in
  match Schema.get_type schema field_name with
  | Integer -> Constant.Integer (get_int32 ~tbl_scan ~field_name)
  | Varchar -> Constant.String (get_string ~tbl_scan ~field_name)

let next ~tbl_scan =
  tbl_scan.cur_slot <-
    Record_page.next_after tbl_scan.rec_page tbl_scan.cur_slot;
  let rec f () =
    if tbl_scan.cur_slot < 0
    then (
      if at_last_block ~tbl_scan
      then
        false
      else
        let block_num = get_block_num ~tbl_scan in 
        move_to_block ~tbl_scan ~block_num:(block_num+1);
        tbl_scan.cur_slot <-
          Record_page.next_after tbl_scan.rec_page tbl_scan.cur_slot;
        f ()
    )
    else
      true
  in
  f ()
  
let before_first ~tbl_scan =
  move_to_block ~tbl_scan ~block_num:0
