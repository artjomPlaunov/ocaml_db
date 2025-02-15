let html =
  {|
    <!DOCTYPE html>
    <html>
    <head>
      <title>b-tree visualizer</title>
      <style>
        body { font-family: sans-serif; max-width: 800px; margin: 0 auto; padding: 20px; }
        .input-group { margin: 20px 0; }
        input { padding: 5px; margin-right: 10px; }
        button { padding: 5px 10px; }
        #graph { margin-top: 20px; border: 1px solid #ccc; padding: 10px; overflow: auto; }
      </style>
    </head>
    <body>
      <h1>b-tree visualizer</h1>
      <div class="input-group">
        <input type="text" id="value" placeholder="Enter a 4-char string" maxlength="4">
        <button onclick="insertValue()">Insert</button>
      </div>
      <div id="graph"></div>

      <script>
        async function insertValue() {
          const value = document.getElementById('value').value;
          if (value.length !== 4) {
            alert('Please enter exactly 4 characters');
            return;
          }
          const response = await fetch('/api/btree/insert', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ value })
          });
          const data = await response.json();
          document.getElementById('graph').innerHTML = data.svg;
          document.getElementById('value').value = '';
        }
      </script>
    </body>
    </html>
  |}

let btree_ref = ref None

let rec delete_directory dir =
  try
    let entries = Sys.readdir dir in
    Array.iter
      (fun entry ->
        let path = Filename.concat dir entry in
        if Sys.is_directory path then delete_directory path else Sys.remove path)
      entries;
    Sys.rmdir dir
  with Sys_error _ -> ()

let init_btree () =
  let dir = "web_btree_data" in
  delete_directory dir;
  (try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let file_manager = File.File_manager.make ~db_dirname:dir ~block_size:40 in
  let storage_manager =
    Storage_manager.make ~file_manager ~storage_file:"test_btree"
  in
  let key_type = Btree.KeyType.TVarchar 4 in
  let t = Btree.create storage_manager key_type in
  btree_ref := Some t;
  t

let handle_insert request =
  let%lwt body = Dream.body request in
  let json = Yojson.Basic.from_string body in
  let value_str = Yojson.Basic.Util.(member "value" json |> to_string) in
  let btree = match !btree_ref with Some t -> t | None -> init_btree () in
  Btree.insert btree (Btree.KeyType.Varchar value_str) 9999;
  let dot_content = Btree.create_graphviz_str btree btree.root_num in
  let temp_dot = Filename.temp_file "btree" ".dot" in
  let temp_svg = Filename.temp_file "btree" ".svg" in
  let () =
    Out_channel.with_open_bin temp_dot (fun oc ->
        Out_channel.output_string oc dot_content)
  in
  let _ = Sys.command (Printf.sprintf "dot -Tsvg %s -o %s" temp_dot temp_svg) in
  let svg = In_channel.with_open_bin temp_svg In_channel.input_all in
  Sys.remove temp_dot;
  Sys.remove temp_svg;
  Dream.json (Yojson.Safe.to_string (`Assoc [ ("svg", `String svg) ]))

let () =
  let _ = init_btree () in
  Dream.run ~port:8080
  @@ Dream.router
       [
         Dream.get "/" (fun _ ->
             btree_ref := None;
             html |> Dream.respond);
         Dream.post "/api/btree/insert" handle_insert;
       ]
