open Storage_manager
open File

(*
Both leaf and internal nodes have the same layout, but the pointer fields can have 
different meanings. 

Initial design choice was to just store pointers as 4 byte offsets, denoting a block 
offset location within a file. The actual file itself is implicitly defined; from the 
B-tree's perspective, a block offset is an offset within the storage manager file that 
is storing the B tree. These are the pointers within the internal nodes of the B tree, 
as well as the sibling pointers in the leaf nodes.

From the perspective of the index manager using the B-tree, a pointer stored in the 
leaf node of the B-tree is a pointer within the original table. Hence there is no need
to store file names, as there are really just two files: the file storing the b tree 
on disk, and the file of the original table, where the end result record is stored. 

Later on, this may be changed to create a more generic pointer interface which would 
allow a user to supply their own pointer and dereferencing methods. However this would 
also require distinguishing between pointers being used within the B tree (internal nodes,
sibling pointers), from the pointer fields in the leaf nodes that point to the location 
of the indexed records. For now, this would be too much overhead in terms of typing. 

Here is a schema of the layout; 16 bytes are reserved; the first 12 bytes store metadata, 
and 4 bytes for the M+1 pointer. Then as many (key, pointer) pairs are packed as space 
allows, denoted by M (number of keys)

N - size of key

Byte offset    Internal Node Layout
     |    
     v    
  0  +------------------------+
     | node_type    [4 bytes] |  = INTERNAL
  4  +------------------------+
     | parent_ptr   [4 bytes] |
  8  +------------------------+
     | num_keys     [4 bytes] |  (0-3 used out of 3 max)
 12  +------------------------+
     | ptr_1        [4 bytes] |  → child block for keys < key_1
 16  +------------------------+
     | key_1        [N bytes] |
 20  +------------------------+
     | ptr_2        [4 bytes] |  → child block for keys between key_1 & key_2
 24  +------------------------+
     | key_2        [N bytes] |
 28  +------------------------+
     | ptr_3        [4 bytes] |  → child block for keys between key_2 & key_3
 32  +------------------------+
     | key_3        [N bytes] |
 36  +------------------------+
     | ptr_4        [4 bytes] |  → child block for keys >= key_3
 40  +------------------------+
....
Remaining Space (no more space in block to fit another (pointer, key) pair). 
...

Byte offset    Leaf Node Layout
     |    
     v    
  0  +------------------------+
     | node_type    [4 bytes] |  = LEAF
  4  +------------------------+
     | parent_ptr   [4 bytes] |
  8  +------------------------+
     | num_keys     [4 bytes] |  (0-3 used out of 3 max)
 12  +------------------------+
     | record_ptr_1 [4 bytes] |  → points to record for key_1
 16  +------------------------+
     | key_1        [N bytes] |
 20  +------------------------+
     | record_ptr_2 [4 bytes] |  → points to record for key_2
 24  +------------------------+
     | key_2        [N bytes] |
 28  +------------------------+
     | record_ptr_3 [4 bytes] |  → points to record for key_3
 32  +------------------------+
     | key_3        [N bytes] |
 36  +------------------------+
     | sibling_ptr  [4 bytes] |  → points to next leaf node
 40  +------------------------+
*)
module KeyType = struct
  type t = TVarchar of int | TInteger
  type value = Varchar of string | Integer of Int32.t

  let ( < ) k1 k2 =
    match (k1, k2) with
    | Varchar s1, Varchar s2 -> s1 < s2
    | Integer n1, Integer n2 -> n1 < n2
    | _ -> failwith "incomparable keys"

  let ( = ) k1 k2 =
    match (k1, k2) with
    | Varchar s1, Varchar s2 -> s1 = s2
    | Integer n1, Integer n2 -> n1 = n2
    | _ -> failwith "incomparable keys"

  let ( <= ) k1 k2 = k1 < k2 || k1 = k2
  let ( > ) k1 k2 = not (k1 <= k2)
  let ( >= ) k1 k2 = not (k1 < k2)

  let string_of_key k =
    match k with
    | Varchar s -> Printf.sprintf "Varchar %s" s
    | Integer d -> Printf.sprintf "Integer %d" (Int32.to_int d)

  let sizeof_key key_type =
    match key_type with TVarchar n -> n | TInteger -> 4

  let empty_key key_type =
    match key_type with
    | TVarchar n -> Varchar (String.make n '"') (* 0x22222222 *)
    | TInteger -> Integer Int32.max_int
end

(* Constants used in disk layout for unused fields -- for debugging purposes when
   analyzing a hexdump. *)
let leaf_constant = Int32.of_int 2863311530 (* 0xAAAAAAAA *)
let internal_constant = Int32.of_int 3149642683 (* 0xBBBBBBBB *)
let unused_pointer_constant = 3722304989 (*0xDDDDDDDD *)

type node_type = Leaf | Internal

let serialize_node_type node_ty =
  match node_ty with Leaf -> leaf_constant | Internal -> internal_constant

let int32_to_node_type i32 =
  if i32 = leaf_constant then Leaf
  else if i32 = internal_constant then Internal
  else failwith "invalid i32, can't convert to node type"

(* B TREE NODE ***************************************************************)
type node = {
  (* Leaf | Internal *)
  mutable node_type : node_type;
  (* Block offset pointer to parent.*)
  mutable parent : int;
  (* current number of keys. *)
  mutable cur_size : int;
  (* The keys and pointers array represent the node
     layout on disk shown above, although we use
     two separate arrays to store the interleaving.

     Index i of pointers is the left pointer of
     index i in the keys array, except for the
     last element of pointers (since there are
     n+1 pointers). The last element corresponds
     to the right most pointer in the layout.
  *)
  keys : KeyType.value array;
  pointers : int array;
  (* Max number of keys that can be stored.*)
  capacity : int;
  key_type : KeyType.t;
}

(* B TREE ********************************************************************)
type t = {
  sm : Storage_manager.t;
  key : KeyType.t;
  mutable root : node;
  mutable root_num : int;
}

(* Calculate N number of keys for the B tree node. This corresponds to an N+1
   branching factor (each branch is a pointer).

   Each key/pointer pair is 4 + (key_size) bytes, since a pointer is 4 bytes.
   Substract out 16 bytes being used to store the first 12 bytes of metadata,
   and the last pointer (the N+1 pointer), that leaves us the space we have
   for the remaining N key/pointer pairs.
*)
let get_num_keys block_size key_ty =
  (block_size - 16) / (4 + KeyType.sizeof_key key_ty)

let empty_node btree =
  let block_size = File_manager.get_blocksize btree.sm.file_manager in
  let key_ty = btree.key in
  let capacity = get_num_keys block_size key_ty in
  {
    node_type = Leaf;
    parent = 0;
    cur_size = 0;
    keys = Array.init capacity (fun _ -> KeyType.empty_key key_ty);
    pointers = Array.init (capacity + 1) (fun _ -> unused_pointer_constant);
    capacity;
    key_type = key_ty;
  }

let deserialize page key_ty block_size =
  let node_type = Page.get_int32 page 0 |> int32_to_node_type in
  let parent = Page.get_int32 page 4 |> Int32.to_int in
  let cur_size = Page.get_int32 page 8 |> Int32.to_int in
  let capacity = get_num_keys block_size key_ty in
  let keys = Array.init capacity (fun _ -> KeyType.empty_key key_ty) in
  (* all pointers have value 0xDDDDDDDD *)
  let pointers = Array.init (capacity + 1) (fun _ -> unused_pointer_constant) in
  let pair_size = 4 + KeyType.sizeof_key key_ty in
  (* Read keys and pointers *)
  for i = 0 to cur_size - 1 do
    (* Read pointer i *)
    let pointer_offset = 12 + (i * pair_size) in
    let key_offset = 12 + (i * pair_size) + 4 in
    pointers.(i) <- Page.get_int32 page pointer_offset |> Int32.to_int;
    (* Read key i *)
    match key_ty with
    | TVarchar n ->
        let str = Page.get_string_raw page key_offset n in
        keys.(i) <- Varchar str
    | TInteger ->
        let num = Page.get_int32 page key_offset in
        keys.(i) <- Integer num
  done;

  (* Read final pointer *)
  let last_pointer_offset = 12 + (cur_size * pair_size) in
  pointers.(cur_size) <- Page.get_int32 page last_pointer_offset |> Int32.to_int;
  (* sister pointer is always the last pointer in our preset size array -- at index capacity *)
  let sister_pointer_offset = 12 + (capacity * pair_size) in
  if node_type = Leaf then
    pointers.(capacity) <-
      Page.get_int32 page sister_pointer_offset |> Int32.to_int;
  { node_type; parent; cur_size; keys; pointers; capacity; key_type = key_ty }

(* Fetch a block from the btree and deserialize it into a btree node.
   params: p is a pointer to a block in the btree. *)
let get_node btree p =
  let block_size = File_manager.get_blocksize btree.sm.file_manager in
  let page = Storage_manager.get_block ~storage_manager:btree.sm ~block_num:p in
  deserialize page btree.key block_size

let serialize node block_size =
  let page = Page.make ~block_size in
  Page.set_int32 page 0 (serialize_node_type node.node_type);
  Page.set_int32 page 4 (Int32.of_int node.parent);
  Page.set_int32 page 8 (Int32.of_int node.cur_size);
  let pair_size = 4 + KeyType.sizeof_key node.key_type in
  for i = 0 to node.capacity - 1 do
    let key_offset = 12 + (i * pair_size) + 4 in
    match node.keys.(i) with
    | Varchar s -> Page.set_string_raw page key_offset s
    | Integer n -> Page.set_int32 page key_offset n
  done;
  for i = 0 to node.capacity do
    let pointer_offset = 12 + (i * pair_size) in
    Page.set_int32 page pointer_offset (Int32.of_int node.pointers.(i))
  done;
  let final_pointer_offset = 12 + (node.capacity * pair_size) in
  if node.node_type = Leaf then
    Page.set_int32 page final_pointer_offset
      (Int32.of_int node.pointers.(node.capacity));
  page

let write_node btree node n =
  let block_size = File_manager.get_blocksize btree.sm.file_manager in
  let page = serialize node block_size in
  Storage_manager.update_block_num ~storage_manager:btree.sm ~block_num:n ~page

let write_node_append btree node =
  let block_size = File_manager.get_blocksize btree.sm.file_manager in
  let page = serialize node block_size in
  let block = Storage_manager.append ~storage_manager:btree.sm ~page in
  Block_id.block_num block

(* Create an empty b-tree, initialize on disk, and return
   in memory data structure.

   Params:
   - storage_manager - assumed to be fresh with 'make'.
   - key type.
*)
let create sm key_ty =
  let block_size = File_manager.get_blocksize sm.file_manager in
  let metadata = Storage_manager.get_head_page ~storage_manager:sm in
  Page.set_int32 metadata 4 (Int32.of_int 1);
  Storage_manager.update_block_num ~storage_manager:sm ~block_num:0
    ~page:metadata;
  let capacity = get_num_keys block_size key_ty in
  let root_node =
    {
      node_type = Leaf;
      parent = 0;
      cur_size = 0;
      keys = Array.init capacity (fun _ -> KeyType.empty_key key_ty);
      pointers = Array.init (capacity + 1) (fun _ -> unused_pointer_constant);
      capacity;
      key_type = key_ty;
    }
  in
  let root_page = serialize root_node block_size in
  let _ = Storage_manager.append ~storage_manager:sm ~page:root_page in
  { sm; key = key_ty; root = root_node; root_num = 1 }

(* Utility function -
   Shift all values right by 1 starting from index i in the keys array.
   Extra param "left" determines if we shift pointers from left of the key
   or right of the key. This means shifting from index i or i+1 in the
   pointers array.
*)
let shift_key_pointer_pair keys pointers capacity n key pointer idx left =
  pointers.(capacity) <- pointers.(capacity - 1);
  for i = capacity - 1 downto idx + 1 do
    keys.(i) <- keys.(i - 1);
    (* If left, we can shift all the way down to idx.
       Otherwise (not left), make sure i > idx+1. *)
    if left || i > idx + 1 then pointers.(i) <- pointers.(i - 1)
  done;
  keys.(idx) <- key;
  if left then pointers.(idx) <- pointer else pointers.(idx + 1) <- pointer

(* n is the total number of keys *)
let insert_key_pointer_pair keys pointers capacity n key pointer left =
  if key < keys.(0) (* Key is less than all keys*) then
    shift_key_pointer_pair keys pointers capacity n key pointer 0 false
  else
    (* TODO: This can be a binary search too. *)
    let i = ref 0 in
    while !i < n && keys.(!i) <= key do
      i := !i + 1
    done;
    (* Negation of while loop condition, i.e. the property at this location:
       i = n || keys[i] > key
       Either we iterated over all the elements (key is larger than all),
       or we are in the middle somewhere, and we found the first key greater
       than our key. We slide all the keys/pointers forward from this index,
       and insert at this location (or just insert directly if we are at the
       end.)
    *)
    if !i = n then (
      keys.(!i) <- key;
      if left then pointers.(!i) <- pointer else pointers.(!i + 1) <- pointer)
    else shift_key_pointer_pair keys pointers capacity n key pointer !i left

let insert_in_leaf btree block key pointer =
  let node = get_node btree block in

  (* Empty node, add at the front. *)
  if node.cur_size = 0 then (
    node.keys.(0) <- key;
    node.pointers.(0) <- pointer;
    node.cur_size <- 1)
  else (
    assert (node.cur_size <> node.capacity);
    insert_key_pointer_pair node.keys node.pointers node.capacity node.cur_size
      key pointer true;
    node.cur_size <- node.cur_size + 1);
  if btree.root_num = block then btree.root <- node;
  write_node btree node block;
  node

(* Insert in root procedure.
   This is used in insert in parent where p1 is the root node, hence we
   don't have a parent node and we need an insert in root procedure.

   Insert in root creates a new root node with the key key_v, and
   sets p1 (old root) as the left pointer, p2 as the right pointer.
*)
let insert_in_root btree p1 key_value p2 =
  let block_size = File_manager.get_blocksize btree.sm.file_manager in
  (* First, create in memory representation for the new root node: *)
  let new_root = empty_node btree in
  (* Root is internal since p1 and p2 are children. *)
  new_root.node_type <- Internal;
  new_root.keys.(0) <- key_value;
  new_root.pointers.(0) <- p1;
  new_root.pointers.(1) <- p2;
  new_root.cur_size <- 1;
  (* Create new root page layout, and append it to disk in the storage manager.
      This gives us the block offset of the new root node in the b-tree file. *)
  let new_root_block_offset = write_node_append btree new_root in
  (* Fetch p2 node *)
  let p2_node = get_node btree p2 in

  (* Make new_root the root of the tree.*)
  (* Save old root in p1_node *)
  let p1_node = btree.root in
  p1_node.parent <- new_root_block_offset;
  p2_node.parent <- new_root_block_offset;
  btree.root <- new_root;
  btree.root_num <- new_root_block_offset;
  write_node btree p1_node p1;
  write_node btree p2_node p2;

  (* Update root node offset in storage manager metadata. *)
  let sm_head_page = Storage_manager.get_head_page ~storage_manager:btree.sm in
  Page.set_int32 sm_head_page 4 (Int32.of_int btree.root_num);
  Storage_manager.set_head_page ~storage_manager:btree.sm sm_head_page

let insert_in_parent_aux btree p1 key_value p2 p0 p0_node =
  let cur_size = p0_node.cur_size in
  insert_key_pointer_pair p0_node.keys p0_node.pointers p0_node.capacity
    p0_node.cur_size key_value p2 false;
  p0_node.cur_size <- cur_size + 1;
  let p2_node = get_node btree p2 in
  p2_node.parent <- p0;
  write_node btree p0_node p0;
  write_node btree p2_node p2

(* correctly assign's children's parent pointer *)
(* function is only called from an internal node *)
let children_update_parents btree parent parent_ptr =
  for i = 0 to parent.cur_size do
    let ptr = parent.pointers.(i) in
    let child = get_node btree ptr in
    child.parent <- parent_ptr;
    write_node btree child ptr
  done

let rec split_parent btree p1 key_v p2 p0 p0_node =
  let { node_type; parent; capacity; cur_size; pointers; keys; _ } = p0_node in
  (* Update all children of node to point to node n. *)
  let new_p0_node = empty_node btree in
  new_p0_node.node_type <- node_type;

  let n = capacity + 1 in
  let keys_buf =
    Array.init n (fun i ->
        if i < n - 1 then keys.(i) else KeyType.empty_key btree.key)
  in
  let ptrs_buf =
    Array.init (n + 1) (fun i ->
        if i < n then pointers.(i) else unused_pointer_constant)
  in
  insert_key_pointer_pair keys_buf ptrs_buf n (n - 1) key_v p2 false;

  let mid = (n + 1) / 2 in
  (* TODO: i think if we make right nodes heavy we wouldn't need to handle the degenerate case for 3 *)
  let mid = if mid = n - 1 then mid - 1 else mid in

  Array.blit ptrs_buf 0 new_p0_node.pointers 0 (mid + 1);
  Array.blit keys_buf 0 new_p0_node.keys 0 mid;
  new_p0_node.cur_size <- mid;
  new_p0_node.node_type <- node_type;
  new_p0_node.parent <- parent;
  (* Write p0 to disk. *)
  write_node btree new_p0_node p0;
  if p0 = btree.root_num then btree.root <- new_p0_node;

  let p2_node = empty_node btree in
  if btree.root_num = p1 then btree.root <- new_p0_node;
  p2_node.node_type <- node_type;
  Array.blit ptrs_buf (mid + 1) p2_node.pointers 0 (n + 1 - (mid + 1));
  Array.blit keys_buf (mid + 1) p2_node.keys 0 (n - (mid + 1));
  p2_node.cur_size <- n - (mid + 1);
  p2_node.parent <- p0;

  (* Write p2 to disk, call insert in parent with new split parent. *)
  let p2 = write_node_append btree p2_node in
  children_update_parents btree p2_node p2;
  write_node btree p2_node p2;
  let split_key = keys_buf.(mid) in
  insert_in_parent btree p0 split_key p2

and insert_in_parent btree p1 key_v p2 =
  if btree.root_num = p1 (* If p1 is root, dispatch to insert_in_root. *) then
    insert_in_root btree p1 key_v p2
  else
    (* Fetch parent node into p0. *)
    let p1_node = get_node btree p1 in
    let p0 = p1_node.parent in
    let p0_node = get_node btree p0 in
    if
      p0_node.cur_size < p0_node.capacity
      (* If parent node has space, insert into parent node.*)
    then insert_in_parent_aux btree p1 key_v p2 p0 p0_node
      (* Otherwise we need to split the parent node and call insert_in_parent again.*)
    else split_parent btree p1 key_v p2 p0 p0_node

let split_leaf btree p1 k p2 p1_node =
  let { node_type; parent; capacity; cur_size; pointers; keys; _ } = p1_node in
  let n = capacity + 1 in
  let mid = (n + 1) / 2 in
  let keys_buf =
    Array.init n (fun i ->
        if i < cur_size then keys.(i) else KeyType.empty_key btree.key)
  in
  let ptrs_buf =
    Array.init (n + 1) (fun i ->
        if i < cur_size + 1 then pointers.(i) else unused_pointer_constant)
  in
  let sibling_ptr = pointers.(capacity) in
  insert_key_pointer_pair keys_buf ptrs_buf n (n - 1) k p2 true;

  let right_node = empty_node btree in
  (* copy pointers and keys i = mid to n-1 into right_node's pointers and keys *)
  Array.blit ptrs_buf mid right_node.pointers 0 (n - mid);
  Array.blit keys_buf mid right_node.keys 0 (n - mid);
  (* update metadata and parent + sibling pointers *)
  right_node.cur_size <- n - mid;
  right_node.pointers.(capacity) <- sibling_ptr;
  right_node.node_type <- node_type;
  right_node.parent <- parent;
  (* write right_node to disk *)
  let right_node_ptr = write_node_append btree right_node in

  let left_node = empty_node btree in
  (* copy pointers and keys i = 0 to mid-1 into left_node's pointers and keys *)
  Array.blit ptrs_buf 0 left_node.pointers 0 mid;
  Array.blit keys_buf 0 left_node.keys 0 mid;
  left_node.cur_size <- mid;
  left_node.pointers.(capacity) <- p2;
  left_node.node_type <- node_type;
  left_node.parent <- parent;
  (* flush left_node back to disk. *)
  write_node btree left_node p1;

  (* get the smallest key from p2 to insert into parent as key partitioning left and right nodes *)
  if btree.root_num = p1 then btree.root <- left_node;
  let split_key = right_node.keys.(0) in
  insert_in_parent btree p1 split_key right_node_ptr

(* Insert key k, record pointer p2 into btree from node p1.
   This is dispatched from the root node as p1.
   If p1 is an internal node, traverse the pointer according
   to the B+ tree condition.
*)
let rec insert_aux btree p1 k p2 =
  let p1_node = get_node btree p1 in
  match p1_node.node_type with
  (* Internal node, find pointer to traverse.*)
  | Internal ->
      (* find the first i where key[i] > key -- iterate while i < n && key[i] <= key *)
      let i = ref 0 in
      while !i < p1_node.cur_size && p1_node.keys.(!i) <= k do
        i := !i + 1
      done;
      let child = p1_node.pointers.(!i) in
      insert_aux btree child k p2
  | Leaf ->
      if p1_node.cur_size < p1_node.capacity then
        let _ = insert_in_leaf btree p1 k p2 in
        ()
      else split_leaf btree p1 k p2 p1_node

let insert btree k p = insert_aux btree btree.root_num k p

let rec create_graphviz_structs btree p edge_map =
  let { node_type; cur_size; keys; pointers; _ } = get_node btree p in
  let structs = ref "" in
  let struct_id = Printf.sprintf "struct%d" p in
  structs := !structs ^ Printf.sprintf "%s [label=\"" struct_id;
  if node_type = Internal then (
    for i = 0 to cur_size do
      let pointer_id = Printf.sprintf "<pointer%d>" i in
      let key_pointer_pair_or_last_pointer =
        if i <> cur_size then
          Printf.sprintf "%s %d|%s|" pointer_id pointers.(i)
            (KeyType.string_of_key keys.(i))
        else Printf.sprintf "%s %d\"];\n" pointer_id pointers.(cur_size)
      in
      structs := !structs ^ key_pointer_pair_or_last_pointer;
      let src = Printf.sprintf "%s:%s" struct_id pointer_id in
      let dst = Printf.sprintf "struct%d" pointers.(i) in
      Hashtbl.add edge_map src dst
    done;
    for i = 0 to cur_size do
      structs := !structs ^ create_graphviz_structs btree pointers.(i) edge_map
    done)
  else
    for i = 0 to cur_size - 1 do
      let key_str = Printf.sprintf "%s" (KeyType.string_of_key keys.(i)) in
      let separator_or_end = if i <> cur_size - 1 then "|" else "\"];\n" in
      structs := !structs ^ key_str ^ separator_or_end
    done;
  !structs

let create_graphviz_str btree p =
  let header = "digraph BTree {\nrankdir=TB;\nnode [shape=record];\n" in
  let footer = "}\n" in
  let edge_map = Hashtbl.create 10 in
  let structs = create_graphviz_structs btree p edge_map in
  let edges =
    Hashtbl.fold
      (fun src dst acc -> acc ^ Printf.sprintf "%s -> %s\n" src dst)
      edge_map ""
  in
  header ^ structs ^ edges ^ footer
