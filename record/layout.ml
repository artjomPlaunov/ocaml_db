type t = {
  schema: Schema.t;
  offsets: (string, int) Hashtbl.t;
  slot_size: int;
}

let make schema =
  let offsets = Hashtbl.create 10 in
  let pos = ref 4 in
  (* val iter : ('a -> unit) -> 'a list -> unit*)
  let iter_f field_name =
    Hashtbl.add offsets field_name !pos;
    pos := (!pos) + (Schema.length_in_bytes schema field_name)
  in
  List.iter iter_f (Schema.fields schema);
  {schema; offsets; slot_size = !pos;}

let create schema offsets slot_size =
  {schema; offsets; slot_size}

let get_schema layout = layout.schema

let get_offset layout field_name =
  Hashtbl.find layout.offsets field_name
