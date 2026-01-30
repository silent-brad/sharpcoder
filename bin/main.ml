open Tools
open Llm

let you_color = "\027[94m"
let assistant_color = "\027[93m"
let reset_color = "\027[0m"

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
     | Some file -> Printf.sprintf {|{"path": "%s", "content": "%s"}|}
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
  | _ -> Printf.sprintf {|{"error": "unknown tool: %s"}|} name

let run_coding_agent_loop () =
  let system_prompt = get_full_system_prompt () in
  print_endline system_prompt;
  let conversation = ref [{ role = "user"; content = system_prompt }] in
  
  try
    while true do
      Printf.printf "%sYou:%s " you_color reset_color;
      flush stdout;
      let user_input = input_line stdin in
      conversation := !conversation @ [{ role = "user"; content = String.trim user_input }];
      
      let continue_loop = ref true in
      while !continue_loop do
        Printf.printf "[DEBUG] Calling LLM...\n%!";
        let assistant_response = execute_llm_call !conversation in
        Printf.printf "[DEBUG] Raw response: %s\n%!" assistant_response;
        let tool_invocations = extract_tool_calls assistant_response in
        Printf.printf "[DEBUG] Found %d tool calls\n%!" (List.length tool_invocations);
        
        if tool_invocations = [] then begin
          Printf.printf "%sAssistant:%s %s\n%!" assistant_color reset_color assistant_response;
          conversation := !conversation @ [{ role = "assistant"; content = assistant_response }];
          continue_loop := false
        end else begin
          conversation := !conversation @ [{ role = "assistant"; content = assistant_response }];
          List.iter (fun (name, args) ->
              Printf.printf "[DEBUG] Executing tool: %s with args: %s\n%!" name (Yojson.Basic.to_string args);
              let resp = execute_tool name args in
              Printf.printf "[DEBUG] Tool result: %s\n%!" resp;
              conversation := !conversation @ [{ role = "user"; content = Printf.sprintf "tool_result(%s)" resp }]
            ) tool_invocations
        end
      done
    done
  with
  | End_of_file -> print_endline "\nGoodbye!"
  | Sys.Break -> print_endline "\nInterrupted."

let () = run_coding_agent_loop ()
