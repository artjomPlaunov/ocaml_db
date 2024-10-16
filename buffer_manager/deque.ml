type 'a node = {
  value : 'a;
  mutable prev : 'a node option;
  mutable next : 'a node option;
}

type 'a t = {
  mutable size : int;
  head_ptr : 'a node option ref;
  tail_ptr : 'a node option ref;
}

let init () = { size = 0; head_ptr = ref None; tail_ptr = ref None }

let push_left t value =
  let new_head = { value; prev = None; next = None } in
  match !(t.head_ptr) with
  | None ->
      t.head_ptr := Some new_head;
      t.tail_ptr := Some new_head;
      t.size <- t.size + 1
  | Some old_head ->
      old_head.prev <- Some new_head;
      new_head.next <- Some old_head;
      t.head_ptr := Some new_head;
      t.size <- t.size + 1

let push_right t value =
  let new_tail = { value; prev = None; next = None } in
  match !(t.tail_ptr) with
  | None ->
      t.head_ptr := Some new_tail;
      t.tail_ptr := Some new_tail;
      t.size <- t.size + 1
  | Some old_tail ->
      old_tail.next <- Some new_tail;
      new_tail.prev <- Some old_tail;
      t.tail_ptr := Some new_tail;
      t.size <- t.size + 1

let pop_left_exn t =
  match !(t.head_ptr) with
  | None -> failwith "can't pop from empty t"
  | Some node ->
      t.head_ptr := node.next;
      (match node.next with
      | None -> t.tail_ptr := None
      | Some next_node -> next_node.prev <- None);
      t.size <- t.size - 1;
      node.value

let pop_right_exn t =
  match !(t.tail_ptr) with
  | None -> failwith "can't pop from empty list"
  | Some node ->
      t.tail_ptr := node.prev;
      (match node.prev with
      | None -> t.head_ptr := None
      | Some prev_node -> prev_node.next <- None);
      t.size <- t.size - 1;
      node.value

let peek_right_exn t =
  match !(t.tail_ptr) with
  | None -> failwith "can't pop from empty list"
  | Some node -> node.value

let length t = t.size
