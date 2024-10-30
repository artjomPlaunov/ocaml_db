module Layout = Record_page__Layout
module Schema = Record_page__Schema

class t tx tbl_name layout = object (self)
  inherit Scan.t

  val mutable rec_page = 
    if Transaction.size ~tx ~filename:(tbl_name ^ ".tbl") = 0 then (
      let block = Transaction.append ~tx ~filename:(tbl_name ^ ".tbl") in
      let rec_page = Record_page.make tx block layout in
      Record_page.format rec_page;
      rec_page
    ) else (
      let block = File.Block_id.make ~filename:(tbl_name ^ ".tbl") ~block_num:0 in
      Record_page.make tx block layout
    )

  val mutable cur_slot = -1
  val file_name = tbl_name ^ ".tbl"
  val tx = tx
  val layout = layout

  method private move_to_block block_num =
    self#close;
    let block = File.Block_id.make ~filename:file_name ~block_num in
    rec_page <- Record_page.make tx block layout;
    cur_slot <- -1

  method private move_to_new_block =
    self#close;
    let block = Transaction.append ~tx ~filename:file_name in
    rec_page <- Record_page.make tx block layout;
    Record_page.format rec_page;
    cur_slot <- -1

  method private get_block_num =
    let block = Record_page.block rec_page in
    File.Block_id.block_num block

  method private at_last_block =
    let block_num = self#get_block_num in
    let size = Transaction.size ~tx ~filename:file_name in
    block_num = size - 1

  method get_rid = 
    let block_num = self#get_block_num in
    Record_id.make ~block_num ~slot:cur_slot

  method move_to_rid ~rid =
    self#close;
    let block = File.Block_id.make ~filename:file_name 
      ~block_num:(Record_id.get_block_num ~rid) in
    rec_page <- Record_page.make tx block layout;
    cur_slot <- Record_id.get_slot ~rid

  method delete = Record_page.delete rec_page cur_slot

  method insert =
    cur_slot <- Record_page.insert_after rec_page cur_slot;
    while cur_slot < 0 do
      if self#at_last_block then self#move_to_new_block
      else self#move_to_block (self#get_block_num + 1);
      cur_slot <- Record_page.insert_after rec_page cur_slot
    done

  method set_string ~field_name ~value =
    Record_page.set_string rec_page cur_slot field_name value

  method set_int32 ~field_name ~value =
    Record_page.set_int32 rec_page cur_slot field_name value

  method set_val ~field_name ~value =
    match value with
    | Constant.Integer v -> self#set_int32 ~field_name ~value:v
    | Constant.String v -> self#set_string ~field_name ~value:v

  method get_int32 ~field_name =
    Record_page.get_int32 rec_page cur_slot field_name

  method get_string ~field_name =
    Record_page.get_string rec_page cur_slot field_name

  method get_val ~field_name =
    let schema = Layout.get_schema layout in
    match Schema.get_type schema field_name with
    | Integer -> Constant.Integer (self#get_int32 ~field_name)
    | Varchar -> Constant.String (self#get_string ~field_name)

  method next =
    cur_slot <- Record_page.next_after rec_page cur_slot;
    let rec try_next () =
      if cur_slot < 0 then
        if self#at_last_block then false
        else (
          let block_num = self#get_block_num in
          self#move_to_block (block_num + 1);
          cur_slot <- Record_page.next_after rec_page cur_slot;
          try_next ()
        )
      else true
    in
    try_next ()

  method before_first = self#move_to_block 0

  method has_field ~field_name =
    let schema = Layout.get_schema layout in
    Schema.has_field schema field_name

  method close =
    let block = Record_page.block rec_page in
    Transaction.unpin ~tx ~block
end

let make ~tx ~tbl_name ~layout = new t tx tbl_name layout
