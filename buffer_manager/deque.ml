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

let push_left ~list ~value =
  let new_head = { value; prev = None; next = None } in
  match !(list.head_ptr) with
  | None ->
      list.head_ptr := Some new_head;
      list.tail_ptr := Some new_head;
      list.size <- list.size + 1
  | Some old_head ->
      old_head.prev <- Some new_head;
      new_head.next <- Some old_head;
      list.head_ptr := Some new_head;
      list.size <- list.size + 1

let push_right ~list ~value =
  let new_tail = { value; prev = None; next = None } in
  match !(list.tail_ptr) with
  | None ->
      list.head_ptr := Some new_tail;
      list.tail_ptr := Some new_tail;
      list.size <- list.size + 1
  | Some old_tail ->
      old_tail.next <- Some new_tail;
      new_tail.prev <- Some old_tail;
      list.tail_ptr := Some new_tail;
      list.size <- list.size + 1

let pop_left_exn ~list =
  match !(list.head_ptr) with
  | None -> failwith "can't pop from empty list"
  | Some node ->
      list.head_ptr := node.next;
      (match node.next with
      | None -> list.tail_ptr := None
      | Some next_node -> next_node.prev <- None);
      list.size <- list.size - 1;
      node.value

let pop_right_exn ~list =
  match !(list.tail_ptr) with
  | None -> failwith "can't pop from empty list"
  | Some node ->
      list.tail_ptr := node.prev;
      (match node.prev with
      | None -> list.head_ptr := None
      | Some prev_node -> prev_node.next <- None);
      list.size <- list.size - 1;
      node.value

let peek_right_exn ~list =
  match !(list.tail_ptr) with
  | None -> failwith "can't pop from empty list"
  | Some node -> node.value

let length ~list = list.size
