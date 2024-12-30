module Schema = Record_page.Schema
module Layout = Record_page.Layout
module Table_scan = Scans__Table_scan

type t = { max_view_defn : int; table_mgr : Table_manager.t }

let make ?(max_view_defn = 100) ~is_new ~(table_mgr : Table_manager.t) ~tx =
  if is_new then (
    let schema = Schema.make () in
    Schema.add_string_field schema "view name" table_mgr.max_name;
    Schema.add_string_field schema "view definition" max_view_defn;
    Table_manager.create_table ~table_mgr ~tbl_name:"view catalog" ~schema ~tx);
  { max_view_defn; table_mgr }

let create_view { max_view_defn; table_mgr } ~view_name ~view_defn ~tx =
  let layout =
    Table_manager.get_layout ~table_mgr ~tbl_name:"view catalog" ~tx
  in
  let tbl_scan = Table_scan.make ~tx ~tbl_name:"view catalog" ~layout in
  tbl_scan#set_string ~field_name:"view name" ~value:view_name;
  tbl_scan#set_string ~field_name:"view definition" ~value:view_defn;
  tbl_scan#close

let get_view_defn_opt { max_view_defn; table_mgr } ~view_name ~tx =
  let result = ref None in
  let layout =
    Table_manager.get_layout ~table_mgr ~tbl_name:"view catalog" ~tx
  in
  let tbl_scan = Table_scan.make ~tx ~tbl_name:"view catalog" ~layout in
  while tbl_scan#next do
    if String.equal (tbl_scan#get_string ~field_name:"view name") view_name then
      result := Some (tbl_scan#get_string ~field_name:"view definition")
  done;
  tbl_scan#close;
  !result
