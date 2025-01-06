open Storage_manager
open File


(*
Both leaf and internal nodes have the same layout, but the pointer fields can have 
different meanings. The Btree just works on block numbers, for internal operations 
it can use the storage manager to turn that block number into a block id and work 
on an actual block. The block numbers at the leaves can be used by the index 
manager using the b tree.

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

type key_type = TVarchar of int | TInteger
type key_val = Varchar of string | Integer of Int32.t


let key_lt k1 k2 = match (k1,k2) with
| (Varchar s1, Varchar s2) -> s1 < s2
| (Integer n1, Integer n2) -> n1 < n2
| _ -> failwith "incomparable keys"

let string_of_key k = match k with 
    | Varchar s -> Printf.sprintf "Varchar %s" s
    | Integer d -> Printf.sprintf "Integer %d" (Int32.to_int d) 



(* Constants used in disk layout for unused fields -- for debugging purposes when 
   analyzing a hexdump. *)
let leaf_constant = Int32.of_int 2863311530  (* 0xAAAAAAAA *)
let internal_constant = Int32.of_int 3149642683  (* 0xBBBBBBBB *)
let unused_pointer_constant = 3722304989 (*0xDDDDDDDD *)

let sizeof_key key_type = match key_type with 
  | TVarchar n -> n
  | TInteger -> 4

let empty_key key_type = match key_type with 
  | TVarchar n -> Varchar (String.make n '"')   (* 0x22222222 *)
  | TInteger -> Integer (Int32.max_int)

type node_type = Leaf | Internal

let serialize_node_type node_ty = match node_ty with 
    Leaf -> leaf_constant | Internal -> internal_constant

(* B TREE NODE ***************************************************************)
type node = {
(* Leaf | Internal *)
  node_type: node_type;

(* Block offset pointer to parent.*)
  mutable parent: int;

(* current number of keys. *)
  mutable cur_size: int;

(*  The keys and pointers array represent the node
    layout on disk shown above, although we use 
    two separate arrays to store the interleaving. 
    
    Index i of pointers is the left pointer of 
    index i in the keys array, except for the 
    last element of pointers (since there are 
    n+1 pointers). The last element corresponds 
    to the right most pointer in the layout.     
    *)
  keys: key_val array;
  pointers: int array;

(* Max number of keys that can be stored.*)
  capacity: int;

  key_type: key_type
}

(* B TREE ********************************************************************)
type t = {
  sm: Storage_manager.t;
  key: key_type;
  mutable root: node;
  root_num: int;
}

(*  Calculate N number of keys for the B tree node. This corresponds to an N+1
    branching factor (each branch is a pointer). 

    Each key/pointer pair is 4 + (key_size) bytes, since a pointer is 4 bytes. 
    Substract out 16 bytes being used to store the first 12 bytes of metadata, 
    and the last pointer (the N+1 pointer), that leaves us the space we have 
    for the remaining N key/pointer pairs.  
    *)
let get_num_keys block_size key_ty = 
    (block_size-16)/(4+(sizeof_key key_ty))

let print_node node = 
    let _ = match node.node_type with 
    | Leaf -> Printf.printf "Leaf Node\n"
    | Internal -> Printf.printf "Internal Node\n" in 
    Printf.printf "Parent: %d\n" node.parent;
    for i = 0 to (node.cur_size - 1) do 
        Printf.printf "P%d: %d\n" i node.pointers.(i); 
        Printf.printf "K%d: %s\n" i (string_of_key node.keys.(i))
    done;
    ()
    



let deserialize page key_ty block_size = 
    let node_type = if Page.get_int32 page 0 = leaf_constant then Leaf else Internal in
    let parent = Int32.to_int (Page.get_int32 page 4) in 
    let cur_size = Int32.to_int (Page.get_int32 page 8) in 
    let capacity = get_num_keys block_size key_ty in 
    let keys = Array.init capacity (fun _ -> empty_key key_ty) in 
    let pointers = Array.init (capacity + 1) (fun _ -> unused_pointer_constant (* 0xDDDDDDDD *)) in 
    let pair_size = 4 + (sizeof_key key_ty) in
    
    (* Read keys and pointers *)
    for i = 0 to (cur_size - 1) do
        (* Read pointer i *)
        pointers.(i) <- Int32.to_int (Page.get_int32 page (12 + (i*pair_size)));
        
        (* Read key i *)
        match key_ty with
        | TVarchar n -> 
            let s = Page.get_string_raw page (12 + (i*pair_size) + 4) n in
            keys.(i) <- Varchar s
        | TInteger ->
            let n = Page.get_int32 page (12 + (i*pair_size) + 4) in
            keys.(i) <- Integer n
    done;
    
    (* Read final pointer *)
    pointers.(cur_size) <- Int32.to_int (Page.get_int32 page (12 + (cur_size*pair_size)));

    {
        node_type;
        parent;
        cur_size;
        keys;
        pointers;
        capacity;
        key_type = key_ty
    }


let serialize node block_size = 
    let page = Page.make ~block_size in 
    Page.set_int32 page 0 (serialize_node_type node.node_type);
    Page.set_int32 page 4 (Int32.of_int node.parent);
    Page.set_int32 page 8 (Int32.of_int node.cur_size);
    let pair_size = 4 + (sizeof_key node.key_type) in 
    for i = 0 to (node.capacity - 1) do 
        match node.keys.(i) with 
        | Varchar s -> Page.set_string_raw page (12 + (i*pair_size) + 4) s
        | Integer n -> Page.set_int32 page (12 + (i*pair_size) + 4) n
    done;
    for i = 0 to node.capacity do 
        Page.set_int32 page (12 + (i*pair_size)) (Int32.of_int node.pointers.(i))
    done;
    page

(*  Create an empty b-tree, initialize on disk, and return 
in memory data structure.

Params:
- storage_manager - assumed to be fresh with 'make'. 
- key type.
   *)
let empty sm key_ty = 
    let block_size = File_manager.get_blocksize sm.file_manager in 
    let metadata = Storage_manager.get_head_page ~storage_manager:sm in 
    Page.set_int32 metadata 4 (Int32.of_int 1); 
    Storage_manager.update_block_num ~storage_manager:sm ~block_num:0 ~page:metadata;
    let capacity = get_num_keys block_size key_ty in 
    let root_node = {
        node_type=Leaf;
        parent = 0;
        cur_size = 0;
        keys = Array.init capacity (fun _ -> empty_key key_ty);
        pointers = Array.init (capacity+1) (fun _ -> unused_pointer_constant);
        capacity;
        key_type = key_ty;
    } in 
    let root_page = serialize root_node block_size in 
    let _ = Storage_manager.append ~storage_manager:sm ~page:root_page in 
    {sm; key=key_ty; root=root_node; root_num=1}

let insert_key_pointer node key pointer idx = 
    for i = node.capacity - 1 downto (idx + 1) do 
        node.keys.(i) <- node.keys.(i-1);
        node.pointers.(i) <- node.pointers.(i-1); 
    done;
    node.keys.(idx) <- key;
    node.pointers.(idx) <- pointer

let insert_in_leaf btree block key pointer = 
    let sm = btree.sm in 
    let block_size = File_manager.get_blocksize sm.file_manager in 
    let leaf_block = Storage_manager.get_block ~storage_manager:sm ~block_num:block in 
    let node = deserialize leaf_block btree.key block_size in
    assert (node.node_type = Leaf);

    if node.cur_size = 0 
    then (
        node.keys.(0) <- key;
        node.pointers.(0) <- pointer;
        node.cur_size <- 1;
    )
    else ( 
        if node.cur_size > 0
        then (
            assert (node.cur_size <> node.capacity); 
            if key_lt key node.keys.(0) 
            then 
                let _ = insert_key_pointer node key pointer 0 in 
                node.cur_size <- node.cur_size + 1
            else 
                ()
        ) else ()
    );
    if btree.root_num = block then btree.root <- node;
    let page = serialize node block_size in 
    Storage_manager.update_block_num ~storage_manager:sm ~block_num:block ~page;
    node