open File

type t = {
  buffers : (Block_id.t, Buffer_manager__Db_buffer.t) Hashtbl.t;
  mutable pins : Block_id.t list;
  buffer_mgr : Buffer_manager.t;
}

let make ~buffer_mgr = { buffers = Hashtbl.create 10; pins = []; buffer_mgr }

let remove_block bls block =
  let rec f ls b =
    match ls with
    | [] -> []
    | h :: t -> if Block_id.eq h b then t else h :: f t b
  in
  f bls block

let get_buffer ~buf_list ~block = Hashtbl.find buf_list.buffers block

let pin ~buf_list ~block =
  let buf = Buffer_manager.pin buf_list.buffer_mgr block in
  Hashtbl.add buf_list.buffers block buf;
  if List.mem block buf_list.pins then ()
  else buf_list.pins <- block :: buf_list.pins

let unpin ~buf_list ~block =
  let buf = Hashtbl.find buf_list.buffers block in
  Buffer_manager.unpin buf_list.buffer_mgr buf;
  buf_list.pins <- remove_block buf_list.pins block;
  if List.mem block buf_list.pins then ()
  else Hashtbl.remove buf_list.buffers block

let unpin_all ~buf_list =
  List.iter
    (fun b ->
      Buffer_manager.unpin buf_list.buffer_mgr (Hashtbl.find buf_list.buffers b))
    buf_list.pins;
  Hashtbl.clear buf_list.buffers;
  buf_list.pins <- []
