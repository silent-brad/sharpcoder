open Utils

let call_gemini prompt =
  let api_key = get_api_key () in
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

let _system_prompt_template =
  {|You are a coding assistant whose goal it is to help us solve coding tasks.
  You have access to a series of tools you can execute.
  When you want to use a tool, reply with exactly one line in the format: 'tool: TOOL_NAME({JSON_ARGS})' and nothing else.
  Use compact single-line JSON with double quotes. After receiving a tool_result(...) message, continue the task.
  If no tool is needed, respond normally.|}

(* Extract JSON object starting at position, handling nested braces *)
let extract_json_object text start =
  let len = String.length text in
  if start >= len || text.[start] <> '{' then None
  else
    let depth = ref 0 in
    let in_string = ref false in
    let i = ref start in
    while !i < len && (!depth > 0 || !i = start) do
      let c = text.[!i] in
      if !in_string then begin
        if c = '"' && (!i = 0 || text.[!i - 1] <> '\\') then in_string := false
      end else begin
        match c with
        | '"' -> in_string := true
        | '{' -> incr depth
        | '}' -> decr depth
        | _ -> ()
      end;
      incr i
    done;
    if !depth = 0 then Some (String.sub text start (!i - start))
    else None

(* Search for tool calls (which follow: `tool: tool_name({...})`) in text and return them as a list *)
let extract_tool_calls text : (string * Yojson.Basic.t) list =
  let re = Str.regexp {|<tool_call name="\([a-zA-Z0-9_]+\)">|} in
  let rec find_all start acc =
    try
      let _ = Str.search_forward re text start in
      let tool_name = Str.matched_group 1 text in
      let json_start = Str.match_end () in
      match extract_json_object text json_start with
      | Some args_str ->
        let args = try Yojson.Basic.from_string args_str with _ -> `Assoc [] in
        find_all (json_start + String.length args_str) ((tool_name, args) :: acc)
      | None -> find_all (Str.match_end ()) acc
    with Not_found -> List.rev acc
  in
  find_all 0 []

type llm_call =
  { role: string
  ; content: string
  }

let execute_llm_call (convo: llm_call list) : string =
  let api_key = get_api_key () in
  let url = Printf.sprintf
    "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.0-flash-lite:generateContent?key=%s"
    api_key in
  let contents = List.map (fun msg ->
      Printf.sprintf {|{"role": "%s", "parts": [{"text": "%s"}]}|}
        (if msg.role = "assistant" then "model" else "user")
        (escape_json_string msg.content)
    ) convo in
  let body = Printf.sprintf {|{"contents": [%s]}|} (String.concat ", " contents) in
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
  extract_text_from_response (Buffer.contents buf)

