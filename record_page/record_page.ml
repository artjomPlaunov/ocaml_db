type t = {
  empty : int;
  used : int;
  tx : Transaction.t;
  block : File.Block_id.t;
  layout : Layout.t;
}

let offset layout slot = slot * Layout.get_slot_size layout

let make tx block layout =
  Transaction.pin ~tx ~block;
  { empty = 0; used = 1; tx; block; layout }

let get_int32 rec_page slot field_name =
  let field_offset = Layout.get_offset rec_page.layout field_name in
  let field_pos = (offset rec_page.layout slot) + field_offset in
  Transaction.get_int32 ~tx:rec_page.tx ~block:rec_page.block ~offset:field_pos

let get_string rec_page slot field_name =
  let field_offset = Layout.get_offset rec_page.layout field_name in
  let field_pos = (offset rec_page.layout slot) + field_offset in
  Transaction.get_string ~tx:rec_page.tx ~block:rec_page.block ~offset:field_pos

let set_int32 rec_page slot field_name value =
  let field_offset = Layout.get_offset rec_page.layout field_name in
  let field_pos = (offset rec_page.layout slot) + field_offset in
  Transaction.set_int ~tx:rec_page.tx ~block:rec_page.block
    ~offset:field_pos ~value ~to_log:true

let set_string rec_page slot field_name value =
  let field_offset = Layout.get_offset rec_page.layout field_name in
  let field_pos = (offset rec_page.layout slot) + field_offset in
  Transaction.set_string ~tx:rec_page.tx ~block:rec_page.block
    ~offset:field_pos ~value ~to_log:true

(* Private methods. *)
let set_flag rec_page slot flag =
  Transaction.set_int ~tx:rec_page.tx ~block:rec_page.block
    ~offset:(offset rec_page.layout slot) ~value:(Int32.of_int flag)
    ~to_log:true

let is_valid_slot rec_page slot =
  (offset rec_page.layout slot) <= Transaction.block_size ~tx:rec_page.tx

let search_after rec_page slot flag =
  let slot = ref (slot + 1) in
  ()
  (*
  while is_valid_slot rec_page !slot do
    let cur_flag = Transaction.get_int32 ~tx:rec_page.tx ~block:rec_page.block
        ~offset:(offset rec_page !slot) in
    
  done*)
