module IntSet = Set.Make (struct
  type t = int

  let compare = compare
end)

type t = {
  capacity_k : int;
  mutable free : IntSet.t;
  mutable used : IntSet.t;
  mutable buffer_access : (int * float Deque.t) array;
}

let make ~capacity_k ~buffer_size =
  {
    capacity_k;
    free = IntSet.of_list (List.init buffer_size (fun i -> i));
    used = IntSet.empty;
    buffer_access = Array.init buffer_size (fun _ -> (0, Deque.init ()));
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
  | Some (pin, _, i) -> if pin <> 0 then None else Some i

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

let remove cache index =
  assert (index < Array.length cache.buffer_access);
  cache.buffer_access.(index) <- (0, Deque.init ())

let num_evictable () =
  failwith
    "todo: return the number of evictable frames that are in the replacer"
