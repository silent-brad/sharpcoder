let read_env_file () =
  let root_dir = Sys.getcwd () in
  let ic = open_in (root_dir ^ "/.env") in
  let rec read_lines acc =
    try
      let line = input_line ic in
      if String.length line > 0 && line.[0] <> '#' then
        match String.split_on_char '=' line with
        | key :: rest ->
          let value = String.concat "=" rest in
          read_lines ((String.trim key, String.trim value) :: acc)
        | _ -> read_lines acc
      else read_lines acc
    with End_of_file ->
      close_in ic;
      acc
  in
  read_lines []

let get_api_key () =
  let env_vars = read_env_file () in
  match List.assoc_opt "GEMINI_API_KEY" env_vars with
  | Some key -> key
  | None -> failwith "GEMINI_API_KEY not found in .env file"

let get_model_name () =
  let env_vars = read_env_file () in
  match List.assoc_opt "MODEL" env_vars with
  | Some key -> key
  | None -> failwith "MODEL not found in .env file"

let escape_json_string s =
  let buf = Buffer.create (String.length s) in
  String.iter (fun c ->
    match c with
    | '"' -> Buffer.add_string buf "\\\""
    | '\\' -> Buffer.add_string buf "\\\\"
    | '\n' -> Buffer.add_string buf "\\n"
    | '\r' -> Buffer.add_string buf "\\r"
    | '\t' -> Buffer.add_string buf "\\t"
    | c -> Buffer.add_char buf c
  ) s;
  Buffer.contents buf

