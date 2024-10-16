open File

type t = {
  bufferpool : Db_buffer.t array;
  mutable num_available : int;
  cache : Lru_replacer.t;
  max_wait_time : int;
}

exception BufferAbortException

let make ?(max_wait_time = 10000) ~file_manager ~log_manager ~num_buffers () =
  let bufferpool =
    Array.init num_buffers (fun i -> Db_buffer.make ~file_manager ~log_manager)
  in
  let cache = Lru_replacer.make ~capacity_k:3 ~num_buffers in
  { bufferpool; num_available = num_buffers; cache; max_wait_time }

let available buffer_manager = buffer_manager.num_available

let flush_all buffer_manager tx_num =
  Array.iter
    (fun buffer ->
      if Db_buffer.modifying_tx buffer == tx_num then Db_buffer.flush buffer)
    buffer_manager.bufferpool

let timedout { max_wait_time; _ } start_time =
  let time_now = Unix.time () in
  max_wait_time < int_of_float time_now - start_time

let find_buffer_opt buffer_mgr block =
  Array.find_opt
    (fun buffer -> Db_buffer.block buffer = block)
    buffer_mgr.bufferpool

let choose_unpinned_buffer_opt buffer_mgr =
  Array.find_opt
    (fun buffer -> Db_buffer.is_unpinned buffer)
    buffer_mgr.bufferpool

let get_frame_id buffer_mgr buffer =
  let id_ref = ref None in
  Array.iteri
    (fun id buf -> if Db_buffer.equal buf buffer then id_ref := Some id)
    buffer_mgr.bufferpool;
  assert (Option.is_some !id_ref);
  Option.get !id_ref

let unpin buffer_mgr buffer =
  Db_buffer.unpin buffer;
  if Db_buffer.is_unpinned buffer then
    let frame_id = get_frame_id buffer_mgr buffer in
    Lru_replacer.set_evictable ~cache:buffer_mgr.cache ~frame_id ~to_evict:true

let try_pinning_opt buffer_mgr block : Db_buffer.t option =
  let find_buf_opt = find_buffer_opt buffer_mgr block in
  let unpinned_buf_opt = choose_unpinned_buffer_opt buffer_mgr in
  match (find_buf_opt, unpinned_buf_opt) with
  | None, None -> None
  | None, Some unpinned_buf ->
      Db_buffer.assign_to_block unpinned_buf block;
      if Db_buffer.is_unpinned unpinned_buf then (
        let frame_id = get_frame_id buffer_mgr unpinned_buf in
        Lru_replacer.record_access buffer_mgr.cache frame_id;
        Lru_replacer.set_evictable ~cache:buffer_mgr.cache ~frame_id
          ~to_evict:false);
      Db_buffer.pin unpinned_buf;
      Some unpinned_buf
  | Some find_buf, _ ->
      if Db_buffer.is_unpinned find_buf then (
        let frame_id = get_frame_id buffer_mgr find_buf in
        Lru_replacer.record_access buffer_mgr.cache frame_id;
        Lru_replacer.set_evictable ~cache:buffer_mgr.cache ~frame_id
          ~to_evict:false);
      Db_buffer.pin find_buf;
      Some find_buf

let waiting_too_long start_time max_time =
  (* system.currtime_ms - starttime > max_time*)
  let cur_time = int_of_float (Unix.gettimeofday () *. 1000.0) in
  cur_time - start_time > max_time

(* TODO: this code does not work in a multithreaded context,
   so it is a dumbed down version that just tries to immediately get
     a pin. This needs to be reworked with threads in mind,
     and possibly include the waiting code. *)
let pin buffer_mgr block =
  match try_pinning_opt buffer_mgr block with
  | Some db_buf -> db_buf
  | None -> raise BufferAbortException
