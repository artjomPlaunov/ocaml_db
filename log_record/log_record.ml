type t =
  | Checkpoint
  | Start
  | Commit
  | UpdateInt of Log_record__Update_int.t
  | UpdateString of Log_record__Update_string.t
  | Rollback

let make ~byte =
  let open File in
  let page = Page.from_bytes byte in
  match Page.get_int32 page 0 |> Int32.to_int with
  | 0 -> failwith "checkpoint record"
  | 1 -> failwith "start record"
  | 2 -> failwith "commit record"
  | 3 -> UpdateInt (Log_record__Update_int.make ~page)
  | 4 -> UpdateString (Log_record__Update_string.make ~page)
  | 5 -> failwith "rollback record"
  | _ -> failwith "we're dead"
