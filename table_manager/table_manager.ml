module Layout = Record_page__Layout
module Schema = Record_page__Schema
module Type = Record_page__Type

type t = {
  max_name : int;
  mutable table_catalog_layout : Layout.t;
  mutable field_catalog_layout : Layout.t;
}

let create_table ~table_mgr ~tbl_name ~schema ~tx =
  let layout = Layout.make schema in
  (* insert record into table catalog *)
  let tbl_catalog =
    Table_scan.make ~tx ~tbl_name:"tablecatalog"
      ~layout:table_mgr.table_catalog_layout
  in
  tbl_catalog#insert;
  tbl_catalog#set_string ~field_name:"tablename" ~value:tbl_name;
  tbl_catalog#set_int32 ~field_name:"slotsize"
    ~value:(Int32.of_int layout.slot_size);
  tbl_catalog#close;
  (* insert record into field catalog for each field *)
  let fld_catalog =
    Table_scan.make ~tx ~tbl_name:"fieldcatalog"
      ~layout:table_mgr.field_catalog_layout
  in
  List.iter
    (fun fld_name ->
      fld_catalog#insert;
      fld_catalog#set_string ~field_name:"tablename" ~value:tbl_name;
      fld_catalog#set_string ~field_name:"fieldname" ~value:fld_name;
      fld_catalog#set_int32 ~field_name:"type"
        ~value:(Int32.of_int (Schema.get_type schema fld_name |> Type.to_int));
      fld_catalog#set_int32 ~field_name:"length"
        ~value:(Int32.of_int (Schema.get_length schema fld_name));
      fld_catalog#set_int32 ~field_name:"offset"
        ~value:(Int32.of_int (Layout.get_offset layout fld_name));)
    (Schema.fields schema);
  fld_catalog#close

let make ~is_new ~tx =
  let table_mgr =
    {
      max_name = 16;
      table_catalog_layout = Layout.make (Schema.make ());
      field_catalog_layout = Layout.make (Schema.make ());
    }
  in
  let tbl_catalog_schema = Schema.make () in
  Schema.add_string_field tbl_catalog_schema "tablename" table_mgr.max_name;
  Schema.add_int_field tbl_catalog_schema "slotsize";
  table_mgr.table_catalog_layout <- Layout.make tbl_catalog_schema;
  let fld_catalog_schema = Schema.make () in
  Schema.add_string_field fld_catalog_schema "tablename" table_mgr.max_name;
  Schema.add_string_field fld_catalog_schema "fieldname" table_mgr.max_name;
  Schema.add_int_field fld_catalog_schema "type";
  Schema.add_int_field fld_catalog_schema "length";
  Schema.add_int_field fld_catalog_schema "offset";
  table_mgr.field_catalog_layout <- Layout.make fld_catalog_schema;
  if is_new then (
    create_table ~table_mgr ~tbl_name:"tablecatalog" ~schema:tbl_catalog_schema
      ~tx;
    create_table ~table_mgr ~tbl_name:"fieldcatalog" ~schema:fld_catalog_schema
      ~tx);
  table_mgr

let get_layout ~table_mgr ~tbl_name ~tx =
  let size = -1 in
  let tbl_catalog =
    Table_scan.make ~tx ~tbl_name:"tablecatalog"
      ~layout:table_mgr.table_catalog_layout
  in
  let slot_size_ref = ref None in
  while tbl_catalog#next do
    let matches_table_name =
      tbl_catalog#get_string ~field_name:"tablename"
      = tbl_name
    in
    if matches_table_name then
      slot_size_ref :=
        tbl_catalog#get_int32 ~field_name:"slotsize"
        |> Int32.to_int |> Option.some
  done;
  tbl_catalog#close;
  let slot_size = match !slot_size_ref with None -> 0 | Some size -> size in
  let schema = Schema.make () in
  let offsets : (string, int) Hashtbl.t = Hashtbl.create 10 in
  let fld_catalog =
    Table_scan.make ~tx ~tbl_name:"fieldcatalog"
      ~layout:table_mgr.field_catalog_layout
  in
  while fld_catalog#next do
    let matches_table_name =
      fld_catalog#get_string ~field_name:"tablename"
      = tbl_name
    in
    if matches_table_name then (
      let fld_name =
        fld_catalog#get_string ~field_name:"fieldname"
      in
      let fld_type =
        fld_catalog#get_int32 ~field_name:"type"
        |> Int32.to_int |> Type.of_int
      in
      let fld_len =
        fld_catalog#get_int32 ~field_name:"length"
        |> Int32.to_int
      in
      let fld_offset =
        fld_catalog#get_int32 ~field_name:"offset"
        |> Int32.to_int
      in
      Hashtbl.add offsets fld_name fld_offset;
      Schema.add_field schema fld_name fld_type fld_len)
  done;
  fld_catalog#close;
  Layout.{ schema; offsets; slot_size }
