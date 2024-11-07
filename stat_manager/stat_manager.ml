module Table_scan = Scans__Table_scan

module StatInfo = struct
  type t = { mutable num_blocks : int; mutable num_records : int }

  let make ~num_blocks ~num_records = { num_blocks; num_records }
end

type t = {
  table_mgr : Table_manager.t;
  mutable table_stats : (string, StatInfo.t) Hashtbl.t;
  mutable num_calls : int;
}

let calc_table_stats ~tbl_name ~layout ~tx =
  let stat_info = StatInfo.make ~num_blocks:0 ~num_records:0 in
  let table_scan = Table_scan.make ~tbl_name ~layout ~tx in
  while table_scan#next do
    stat_info.num_records <- stat_info.num_records + 1;
    stat_info.num_blocks <- Record_id.get_block_num ~rid:table_scan#get_rid + 1
  done;
  table_scan#close;
  stat_info

let refresh_stats ~stat_mgr:{ table_mgr; table_stats; _ } ~tx =
  let table_stats = Hashtbl.create 10 in
  let num_calls = 0 in
  let layout =
    Table_manager.get_layout ~table_mgr ~tbl_name:"tablecatalog" ~tx
  in
  let table_scan = Table_scan.make ~tx ~tbl_name:"tablecatalog" ~layout in
  while table_scan#next do
    let tbl_name = table_scan#get_string ~field_name:"tablename" in
    let layout =
      Table_manager.get_layout ~table_mgr ~tbl_name:"tablename" ~tx
    in
    let stat_info = calc_table_stats ~tbl_name ~layout ~tx in
    Hashtbl.add table_stats tbl_name stat_info
  done;
  table_scan#close

let make ~table_mgr ~tx =
  let stat_mgr =
    { table_mgr; table_stats = Hashtbl.create 10; num_calls = 0 }
  in
  refresh_stats ~stat_mgr ~tx

let get_stat_info ~stat_mgr ~tbl_name ~layout ~tx =
  stat_mgr.num_calls <- stat_mgr.num_calls + 1;
  if stat_mgr.num_calls > 100 then refresh_stats ~stat_mgr ~tx;
  let stat_info_opt = Hashtbl.find_opt stat_mgr.table_stats tbl_name in
  match stat_info_opt with
  | None ->
      let stat_info = calc_table_stats ~tbl_name ~layout ~tx in
      Hashtbl.add stat_mgr.table_stats tbl_name stat_info;
      stat_info
  | Some stat_info -> stat_info
