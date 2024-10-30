module Layout = Record_page__Layout
module Schema = Record_page__Schema

type t = {
  tx : Transaction.t;
  layout : Layout.t;
  mutable rec_page : Record_page.t;
  file_name : string;
  mutable cur_slot : int;
}

let make ~tx ~tbl_name ~layout =
  let file_name = tbl_name ^ ".tbl" in
  if Transaction.size ~tx ~filename:file_name = 0 then (
    let block = Transaction.append ~tx ~filename:file_name in
    let rec_page = Record_page.make tx block layout in
    Record_page.format rec_page;
    { tx; layout; rec_page; file_name; cur_slot = -1 })
  else
    let block = File.Block_id.make ~filename:file_name ~block_num:0 in
    let rec_page = Record_page.make tx block layout in
    { tx; layout; rec_page; file_name; cur_slot = -1 }

let close ~scan =
  let block = Record_page.block scan.rec_page in
  Transaction.unpin ~tx:scan.tx ~block

let move_to_block ~scan ~block_num =
  close ~scan;
  let block = File.Block_id.make ~filename:scan.file_name ~block_num in
  scan.rec_page <- Record_page.make scan.tx block scan.layout;
  scan.cur_slot <- -1

let move_to_new_block ~scan =
  close ~scan;
  let block = Transaction.append ~tx:scan.tx ~filename:scan.file_name in
  scan.rec_page <- Record_page.make scan.tx block scan.layout;
  Record_page.format scan.rec_page;
  scan.cur_slot <- -1

let get_block_num ~scan =
  let block = Record_page.block scan.rec_page in
  File.Block_id.block_num block

let at_last_block ~scan =
  let block_num = get_block_num ~scan in
  let size = Transaction.size ~tx:scan.tx ~filename:scan.file_name in
  block_num = size

let get_rid ~scan =
  let block_num = get_block_num ~scan in
  Record_id.make ~block_num ~slot:scan.cur_slot

let move_to_rid ~scan ~rid =
  close ~scan;
  let block =
    File.Block_id.make ~filename:scan.file_name
      ~block_num:(Record_id.get_block_num ~rid)
  in
  scan.rec_page <- Record_page.make scan.tx block scan.layout;
  scan.cur_slot <- Record_id.get_slot ~rid

let delete ~scan = Record_page.delete scan.rec_page scan.cur_slot

let insert ~scan =
  scan.cur_slot <-
    Record_page.insert_after scan.rec_page scan.cur_slot;
  while scan.cur_slot < 0 do
    if at_last_block ~scan then move_to_new_block ~scan
    else move_to_block ~scan ~block_num:(get_block_num ~scan + 1);
    scan.cur_slot <-
      Record_page.insert_after scan.rec_page scan.cur_slot
  done

let set_string ~scan ~field_name ~value =
  Record_page.set_string scan.rec_page scan.cur_slot field_name value

let set_int32 ~scan ~field_name ~value =
  Record_page.set_int32 scan.rec_page scan.cur_slot field_name value

let set_val ~scan ~field_name ~value =
  match value with
  | Constant.Integer v -> set_int32 ~scan ~field_name ~value:v
  | Constant.String v -> set_string ~scan ~field_name ~value:v

let get_int32 ~scan ~field_name =
  Record_page.get_int32 scan.rec_page scan.cur_slot field_name

let get_string ~scan ~field_name =
  Record_page.get_string scan.rec_page scan.cur_slot field_name

let get_val ~scan ~field_name =
  let schema = Layout.get_schema scan.layout in
  match Schema.get_type schema field_name with
  | Integer -> Constant.Integer (get_int32 ~scan ~field_name)
  | Varchar -> Constant.String (get_string ~scan ~field_name)

let next ~scan =
  scan.cur_slot <-
    Record_page.next_after scan.rec_page scan.cur_slot;
  let rec f () =
    if scan.cur_slot < 0 then (
      if at_last_block ~scan then false
      else
        let block_num = get_block_num ~scan in
        move_to_block ~scan ~block_num:(block_num + 1);
        scan.cur_slot <-
          Record_page.next_after scan.rec_page scan.cur_slot;
        f ())
    else true
  in
  f ()

let before_first ~scan = move_to_block ~scan ~block_num:0

let has_field ~scan ~field_name = 
  Schema.has_field (Layout.get_schema scan.layout) field_name