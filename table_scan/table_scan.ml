module Layout = Record_page__Layout
module Schema = Record_page__Schema

type t = {
  tx: Transaction.t;
  layout: Layout.t;
  rec_page: Record_page.t;
  file_name: string;
  mutable cur_slot: int;
}

let make ~tx ~tbl_name ~layout =
  let file_name = tbl_name ^ ".tbl" in 
  if Transaction.size ~tx ~filename:file_name = 0
  then
    () else ()
