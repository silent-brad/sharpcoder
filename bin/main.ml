let read_env_file () =
  let ic = open_in ".env" in
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

let call_gemini api_key prompt =
  let url = Printf.sprintf
    "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.0-flash-lite:generateContent?key=%s"
    api_key in
  let body = Printf.sprintf {|{"contents": [{"parts": [{"text": "%s"}]}]}|}
    (escape_json_string prompt) in
  let tmp_file = Filename.temp_file "gemini" ".json" in
  let oc = open_out tmp_file in
  output_string oc body;
  close_out oc;
  let cmd = Printf.sprintf "curl -s -X POST '%s' -H 'Content-Type: application/json' -d @%s"
    url tmp_file in
  let ic = Unix.open_process_in cmd in
  let buf = Buffer.create 1024 in
  (try
    while true do
      Buffer.add_channel buf ic 1
    done
  with End_of_file -> ());
  let _ = Unix.close_process_in ic in
  Sys.remove tmp_file;
  Buffer.contents buf

let extract_text_from_response json_str =
  let open Yojson.Basic.Util in
  try
    let json = Yojson.Basic.from_string json_str in
    json
    |> member "candidates"
    |> index 0
    |> member "content"
    |> member "parts"
    |> index 0
    |> member "text"
    |> to_string
  with _ ->
    Printf.sprintf "Error parsing response: %s" json_str

(*
let () =
  print_string "You: ";
  flush stdout;
  let prompt = input_line stdin in
  let api_key = get_api_key () in
  let response = call_gemini api_key prompt in
  let text = extract_text_from_response response in
  Printf.printf "\nGemini: %s\n" text
*)

let () =
  let api_key = get_api_key () in
  let prompt = "You are a Kierkegaardian scholar with a love for Shakespearian poetry. Write a poem about Abraham and his faith at Mount Moriah inspired by Kierkegaard's Fear and Trembling." in
  let response = call_gemini api_key prompt in
  let text = extract_text_from_response response in
  Printf.printf "Response: %s\n" text

