open Tools
open Llm

let you_color = "\027[94m"
let assistant_color = "\027[93m"
let reset_color = "\027[0m"

let rlm_context = ref ""
let rlm_context_threshold = 50000

let get_arg args key default =
  let open Yojson.Basic.Util in
  try args |> member key |> to_string
  with _ -> default

let execute_tool name args =
  match name with
  | "read_file" ->
    let filename = get_arg args "filename" "." in
    Printf.printf "Tool: read_file(%s)\n" filename;
    (match read_file_tool filename with
      | Some file ->
        let content_len = String.length file.content in
        if content_len > rlm_context_threshold then begin
          rlm_context := file.content;
          Printf.printf "[RLM] Large file detected (%d chars), loaded into $CONTEXT\n" content_len;
          Printf.sprintf
            {|{"path": "%s", "rlm_mode": true, "context_size": %d, "message": "File too large for direct context. Loaded into $CONTEXT variable. Use nu_exec tool to analyze (e.g., $CONTEXT | lines | where {|l| $l =~ \"pattern\"} | first 10) or sub_agent for semantic analysis of slices."}|}
            file.file_path content_len
        end else
          Printf.sprintf {|{"path": "%s", "content": "%s"}|}
            file.file_path (Utils.escape_json_string file.content)
      | None -> {|{"error": "file not found"}|})
  | "list_files" ->
    let path = get_arg args "path" "." in
    Printf.printf "Tool: list_files(%s)\n" path;
    (match list_files_tool path with
      | Some files ->
        let file_strs = List.map (fun f ->
            Printf.sprintf {|{"path": "%s", "kind": "%s"}|} f.file_path f.file_kind
          ) files.files in
        Printf.sprintf {|{"path": "%s", "files": [%s]}|} files.path (String.concat ", " file_strs)
      | None -> {|{"error": "path not found"}|})
  | "edit_file" ->
    let path = get_arg args "path" "." in
    let old_str = get_arg args "old_str" "" in
    let new_str = get_arg args "new_str" "" in
    Printf.printf "Tool: edit_file(%s)\n" path;
    let result = edit_file_tool path old_str new_str in
    Printf.sprintf {|{"path": "%s", "action": "%s"}|} result.path result.action
  | "grep" ->
    let path = get_arg args "path" "." in
    let pattern = get_arg args "pattern" "" in
    Printf.printf "Tool: grep(%s)\n" path;
    (match grep_tool path pattern with
      | Some results ->
        let result_strs = List.map (fun (r : Tools.grep_result) ->
            Printf.sprintf {|{"path": "%s", "line_number": %d}|} r.path r.line_number
          ) results.results in
        Printf.sprintf {|{"path": "%s", "results": [%s]}|} results.path (String.concat ", " result_strs)
      | None -> {|{"error": "path not found"}|})
  | "find" ->
    let path = get_arg args "path" "." in
    let pattern = get_arg args "pattern" "" in
    Printf.printf "Tool: find(%s)\n" path;
    (match find_tool path pattern with
      | Some results ->
        let result_strs = List.map (fun (r : Tools.find_result) ->
            Printf.sprintf {|{"path": "%s", "line_number": %d}|} r.path r.line_number
          ) results.results in
        Printf.sprintf {|{"path": "%s", "results": [%s]}|} results.path (String.concat ", " result_strs)
      | None -> {|{"error": "path not found"}|})
  | "sed" ->
    let path = get_arg args "path" "." in
    let pattern = get_arg args "pattern" "" in
    let new_str = get_arg args "new_str" "" in
    Printf.printf "Tool: sed(%s)\n" path;
    let results = sed_tool path pattern new_str in
    let result_strs = List.map (fun (r : Tools.sed_result) ->
        Printf.sprintf {|{"path": "%s", "line_number": %d}|} r.path r.line_number
      ) results.results in
    Printf.sprintf {|{"path": "%s", "results": [%s]}|} results.path (String.concat ", " result_strs)
  | "nu" ->
    let cmd = get_arg args "cmd" "" in
    Printf.printf "Tool: nu(%s)\n" cmd;
    (match run_nushell_cmd_tool cmd with
      | { cmd = cmd'; output = output } ->
        Printf.sprintf {|{"cmd": "%s", "output": "%s"}|} cmd' output)
  | "sub_agent" ->
    let query = get_arg args "query" "" in
    let context = get_arg args "context" !rlm_context in
    Printf.printf "Tool: sub_agent(query=%s, context_len=%d)\n" query (String.length context);
    let result = sub_agent_tool query context in
    Printf.sprintf {|{"query": "%s", "response": "%s"}|}
      (Utils.escape_json_string result.query)
      (Utils.escape_json_string result.response)
  | "nu_exec" ->
    let code = get_arg args "code" "" in
    let context = get_arg args "context" !rlm_context in
    Printf.printf "Tool: nu_exec(code_len=%d, context_len=%d)\n" 
      (String.length code) (String.length context);
    let result = nu_exec_tool code context in
    Printf.sprintf {|{"code": "%s", "output": "%s"}|}
      (Utils.escape_json_string result.code)
      (Utils.escape_json_string result.output)
  | "load_context" ->
    let filename = get_arg args "filename" "" in
    Printf.printf "Tool: load_context(%s)\n" filename;
    (match read_file_tool filename with
      | Some file -> 
        rlm_context := file.content;
        Printf.sprintf {|{"loaded": true, "context_size": %d, "preview": "%s"}|}
          (String.length file.content)
          (Utils.escape_json_string (String.sub file.content 0 (min 200 (String.length file.content))))
      | None -> {|{"error": "file not found"}|})
  | _ -> Printf.sprintf {|{"error": "unknown tool: %s"}|} name

let run_coding_agent_loop () =
  let system_prompt = get_full_system_prompt () in
  let conversation = ref [{ role = "user"; content = system_prompt }] in
  
  try
    while true do
      Printf.printf "%sYou:%s " you_color reset_color;
      flush stdout;
      let user_input = input_line stdin in
      conversation := !conversation @ [{ role = "user"; content = String.trim user_input }];
      
      let continue_loop = ref true in
      while !continue_loop do
        let assistant_response = execute_llm_call !conversation in
        (*Printf.printf "Raw response: %s\n%!" assistant_response;*)
        let tool_invocations = extract_tool_calls assistant_response in
        
        if tool_invocations = [] then begin
          Printf.printf "%sAssistant:%s %s\n%!" assistant_color reset_color assistant_response;
          conversation := !conversation @ [{ role = "assistant"; content = assistant_response }];
          continue_loop := false
        end else begin
          conversation := !conversation @ [{ role = "assistant"; content = assistant_response }];
          List.iter (fun (name, args) ->
              Printf.printf "Executing tool: %s with args: %s\n%!" name (Yojson.Basic.to_string args);
              let resp = execute_tool name args in
              Printf.printf "Tool result: %s\n%!" resp;
              conversation := !conversation @ [{ role = "user"; content = Printf.sprintf "<tool_result>%s</tool_result>" resp }]
            ) tool_invocations
        end
      done
    done
  with
  | End_of_file -> print_endline "\nGoodbye!"
  | Sys.Break -> print_endline "\nInterrupted."

let () = run_coding_agent_loop ()
