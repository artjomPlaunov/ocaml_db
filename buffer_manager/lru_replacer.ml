module IntSet = Set.Make (struct
  type t = int

  let compare = compare
end)

type access_times = Int64.t Deque.t

type t = {
  capacity_k : int;
  mutable evictable_set : IntSet.t;
  mutable buffer_access : access_times array;
}

external clock_gettime_ns : unit -> Int64.t = "clock_gettime_ocaml"

let make ~capacity_k ~num_buffers =
  {
    capacity_k;
    evictable_set = IntSet.of_list (List.init num_buffers (fun i -> i));
    buffer_access = Array.init num_buffers (fun _ -> Deque.init ());
  }

let evict cache =
  let len_time_id_arr =
    Array.mapi
      (fun frame_id access_times ->
        let length = Deque.length access_times in
        let time =
          if length = 0 then Int64.zero else Deque.peek_right_exn access_times
        in
        (length, time, frame_id))
      cache.buffer_access
  in
  let compare (l1, t1, i1) (l2, t2, i2) =
    match compare l1 l2 with
    | 0 -> ( match compare t1 t2 with 0 -> compare i1 i2 | cmp -> cmp)
    | cmp -> cmp
  in
  let min_element compare arr =
    if Array.length arr = 0 then None
    else
      let first_elt = arr.(0) in
      let min_elt =
        Array.fold_left
          (fun init time_id ->
            if compare init time_id <= 0 then init else time_id)
          first_elt arr
      in
      Some min_elt
  in
  match min_element compare len_time_id_arr with
  | None -> None
  | Some (_, _, frame_id) -> Some frame_id

let record_access cache frame_id =
  assert (frame_id < Array.length cache.buffer_access);
  let access_times = Array.get cache.buffer_access frame_id in
  let length = Deque.length access_times in
  assert (0 <= length && length <= cache.capacity_k);
  if length < cache.capacity_k then
    let time_now = clock_gettime_ns () in
    Deque.push_left access_times time_now
  else (
    assert (length = cache.capacity_k);
    let _ = Deque.pop_right_exn access_times in
    let time_now = clock_gettime_ns () in
    Deque.push_left access_times time_now)

let remove cache frame_id =
  assert (frame_id < Array.length cache.buffer_access);
  cache.buffer_access.(frame_id) <- Deque.init ()

let set_evictable ~cache ~frame_id ~to_evict =
  let access_times = cache.buffer_access.(frame_id) in
  (* TODO double negative: to_evict and is_pinned are opposites *)
  if to_evict then (
    (* sanity check ensuring that it pinned and going to be unpinned *)
    assert (not (IntSet.mem frame_id cache.evictable_set));
    cache.buffer_access.(frame_id) <- access_times;
    cache.evictable_set <- IntSet.add frame_id cache.evictable_set)
  else (
    (* ensuring the opposite *)
    assert (IntSet.mem frame_id cache.evictable_set);
    cache.buffer_access.(frame_id) <- access_times;
    cache.evictable_set <- IntSet.remove frame_id cache.evictable_set)

let num_evictable cache = IntSet.cardinal cache.evictable_set
