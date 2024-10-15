type t = {
  empty : int;
  used : int;
  tx : Transaction.t;
  block : File.Block_id.t;
  layout : Layout.t;
}

let offset layout slot = slot * Layout.get_slot_size layout

let block rec_page = rec_page.block

let make tx block layout =
  Transaction.pin ~tx ~block;
  { empty = 0; used = 1; tx; block; layout }

let get_int32 rec_page slot field_name =
  let field_offset = Layout.get_offset rec_page.layout field_name in
  let field_pos = offset rec_page.layout slot + field_offset in
  Transaction.get_int32 ~tx:rec_page.tx ~block:rec_page.block ~offset:field_pos

let get_string rec_page slot field_name =
  let field_offset = Layout.get_offset rec_page.layout field_name in
  let field_pos = offset rec_page.layout slot + field_offset in
  Transaction.get_string ~tx:rec_page.tx ~block:rec_page.block ~offset:field_pos

let set_int32 rec_page slot field_name value =
  let field_offset = Layout.get_offset rec_page.layout field_name in
  let field_pos = offset rec_page.layout slot + field_offset in
  Transaction.set_int ~tx:rec_page.tx ~block:rec_page.block ~offset:field_pos
    ~value ~to_log:true

let set_string rec_page slot field_name value =
  let field_offset = Layout.get_offset rec_page.layout field_name in
  let field_pos = offset rec_page.layout slot + field_offset in
  Transaction.set_string ~tx:rec_page.tx ~block:rec_page.block ~offset:field_pos
    ~value ~to_log:true

let set_flag rec_page slot flag =
  Transaction.set_int ~tx:rec_page.tx ~block:rec_page.block
    ~offset:(offset rec_page.layout slot)
    ~value:(Int32.of_int flag) ~to_log:true

let is_valid_slot rec_page slot =
  offset rec_page.layout (slot + 1) <= Transaction.block_size ~tx:rec_page.tx

let search_after rec_page slot flag =
  let rec f i =
    if is_valid_slot rec_page i then
      let cur_flag =
        Transaction.get_int32 ~tx:rec_page.tx ~block:rec_page.block
          ~offset:(offset rec_page.layout i)
      in
      if Int32.to_int cur_flag = flag then i else f (i + 1)
    else -1
  in
  f (slot + 1)

let delete rec_page slot = set_flag rec_page slot rec_page.empty

let format rec_page =
  let rec f i =
    if is_valid_slot rec_page i then (
      Transaction.set_int ~tx:rec_page.tx ~block:rec_page.block ~offset:i
        ~value:(Int32.of_int rec_page.empty)
        ~to_log:false;
      let schema = Layout.get_schema rec_page.layout in
      let iter_f field_name =
        let field_offset = Layout.get_offset rec_page.layout field_name in
        let field_pos = offset rec_page.layout i + field_offset in
        match
          Schema.get_type (Layout.get_schema rec_page.layout) field_name
        with
        | Integer ->
            Transaction.set_int ~tx:rec_page.tx ~block:rec_page.block
              ~offset:field_pos ~value:(Int32.of_int 0) ~to_log:false
        | Varchar ->
            Transaction.set_string ~tx:rec_page.tx ~block:rec_page.block
              ~offset:field_pos ~value:"" ~to_log:false
      in
      List.iter iter_f (Schema.fields schema))
    else ()
  in
  f 0

let insert_after rec_page slot =
  let new_slot = search_after rec_page slot rec_page.empty in
  if new_slot >= 0 then set_flag rec_page new_slot rec_page.used;
  new_slot

let next_after rec_page slot = search_after rec_page slot rec_page.used
