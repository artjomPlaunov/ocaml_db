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

type key_type = Varchar of int | Integer

let sizeof_key key_type = match key_type with 
  | Varchar n -> n
  | Integer -> 4

type node_type = Leaf | Internal

type node = {
  node_type: node_type;
  parent: int;
}

type t = {
  sm: Storage_manager.t;
  key: key_type;
  root: node;
  root_num: int;
}

(*  Create an empty b-tree, initialize on disk, and return 
in memory data structure.

Params:
- storage_manager
- key type.
   *)

(*
let empty sm key = 
  let n = File_manager.get_blocksize sm.file_manager in 
*)
