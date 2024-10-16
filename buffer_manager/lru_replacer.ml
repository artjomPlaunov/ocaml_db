module IntSet = Set.Make (struct
  type t = int

  let compare = compare
end)

type t = {
  capacity_k : int;
  mutable evictable_set : IntSet.t;
  mutable buffer_access : (bool * float Deque.t) array;
}

let make ~capacity_k ~num_buffers =
  {
    capacity_k;
    evictable_set = IntSet.of_list (List.init num_buffers (fun i -> i));
    buffer_access = Array.init num_buffers (fun _ -> (false, Deque.init ()));
  }

let evict cache =
  let pinned_time_index_arr =
    Array.mapi
      (fun frame_id (is_pinned, access_times) ->
        let length = Deque.length ~list:access_times in
        let time =
          if length = 0 then 0. else Deque.peek_right_exn ~list:access_times
        in
        (is_pinned, time, frame_id))
      cache.buffer_access
  in
  let compare (p1, t1, i1) (p2, t2, i2) =
    match compare p1 p2 with
    | 0 -> ( match compare t1 t2 with 0 -> compare i1 i2 | cmp -> cmp)
    | cmp -> cmp
  in
  let min_element compare arr =
    if Array.length arr = 0 then None
    else
      let first_elt = arr.(0) in
      let min_elt =
        Array.fold_left
          (fun init tup -> if compare init tup <= 0 then init else tup)
          first_elt arr
      in
      Some min_elt
  in
  match min_element compare pinned_time_index_arr with
  | None -> None
  | Some (is_pinned, _, frame_id) -> if is_pinned then Some frame_id else None

let record_access cache frame_id =
  assert (frame_id < Array.length cache.buffer_access);
  let is_pinned, access_times = Array.get cache.buffer_access frame_id in
  let length = Deque.length ~list:access_times in
  assert (length <= cache.capacity_k);
  if length < cache.capacity_k then
    Deque.push_left ~list:access_times ~value:(Unix.time ())
  else
    let _ = Deque.pop_right_exn ~list:access_times in
    Deque.push_left ~list:access_times ~value:(Unix.time ())

let remove cache frame_id =
  assert (frame_id < Array.length cache.buffer_access);
  cache.buffer_access.(frame_id) <- (false, Deque.init ())

let set_evictable ~cache ~frame_id ~to_evict =
  let is_pinned, access_times = cache.buffer_access.(frame_id) in
  (* TODO double negative: to_evict and is_pinned are opposites *)
  if to_evict then (
    (* sanity check ensuring that it pinned and going to be unpinned *)
    assert (not (IntSet.mem frame_id cache.evictable_set));
    cache.buffer_access.(frame_id) <- (false, access_times);
    cache.evictable_set <- IntSet.add frame_id cache.evictable_set)
  else (
    (* sanity check ensuring the opposite *)
    assert (IntSet.mem frame_id cache.evictable_set);
    cache.buffer_access.(frame_id) <- (true, access_times);
    cache.evictable_set <- IntSet.remove frame_id cache.evictable_set)

let num_evictable cache = IntSet.cardinal cache.evictable_set
