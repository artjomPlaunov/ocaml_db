open File

type t = {
  mutable tx_num : int;
  mutable offset : int;
  mutable value : string;
  mutable block : Block_id.t;
}

let make ~page =
