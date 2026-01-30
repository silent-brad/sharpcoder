open Option
open Str


(* file.txt -> /User/path/to/project/file.txt *)
let resolve_abs_path path_str =
  (*let path_str = Filename.concat (Sys.getcwd ()) path_str in*)
  Filename.concat (Sys.getcwd ()) path_str

(* Returns file content *)
let read_file_tool_desc =
  {|Gets the full content of a file provided by the user.
:param filename: The name of the file to read.
:return: The full content of the file.|}

type file =
  { file_path: string
  ; content: string
  }

let read_file_tool filename : file option =
  let full_path = resolve_abs_path filename in
  if Sys.file_exists full_path then
    let file_text = In_channel.with_open_text full_path In_channel.input_all in
    Some { file_path = full_path
    ; content = file_text
    }
  else None

(*let () =
  let file' = read_file_tool "text.txt" in
  match file' with
    | Some file'' ->
      let path = file''.file_path in
      let text = file''.content in
      Printf.printf "File: %s\n" path;
      Printf.printf "Text: %s\n" text
    | None -> failwith "File not found."*)

(* Acts as `ls` *)
let file_list_tool_desc =
  {|Lists the files in a directory provided by the user.
:param path: The path to a directory to list files from.
:return: A list of files in the directory.|}
  type file_entry =
  { file_path: string
  ; file_kind: string
  }

type file_list =
  { path: string
  ; files: file_entry list
  }

let list_files_tool path : file_list option =
  let full_path = resolve_abs_path path in
  if Sys.is_directory full_path then
    let files = Sys.readdir full_path in
    let files = Array.to_list files in
    let files = List.map (fun file ->
        let file_path = Filename.concat full_path file in
        let file_type = Unix.stat file_path in
        { file_path = file_path
        ; file_kind = (match file_type.Unix.st_kind with
            | Unix.S_REG -> "file"
            | Unix.S_DIR -> "directory"
            | Unix.S_LNK -> "symlink"
            | _ -> "other")
        }
      ) files in
    Some { path = full_path
    ; files = files
    }
  else None

(*
let () =
  let files = list_files_tool "text" in
  match files with
    | Some files' ->
      let path = files'.path in
      let files = files'.files in
      Printf.printf "Path: %s\n" path;
      List.iter (fun file ->
          let file_path = file.file_path in
          let kind = file.file_kind in
          Printf.printf "File: %s\n" file_path;
          Printf.printf "Type: %s\n" kind
        ) files
    | None -> failwith "Path not found."
*)

(* Edit file
 * Creates a new file when old_str is empty,
 * Replacing text by finding old_str and replacing with new_str
 *)

let edit_file_tool_desc =
  {|Replaces first occurrence of old_str with new_str in file. If old_str is empty,
create/overwrite file with new_str.
:param path: The path to the file to edit.
:param old_str: The string to replace.
:param new_str: The string to replace with.
:return: A dictionary with the path to the file and the action taken.|}

type edit_file_result =
  { path: string
  ; action: string
  }

let is_substring needle haystack =
  try
    let re = regexp_string needle in
    let _ = search_forward re haystack 0 in
    true
  with Not_found -> false

let edit_file_tool filename old_str new_str : edit_file_result =
  let full_path = resolve_abs_path filename in
  if old_str = "" then
    (* Create new file with new_str *)
    let file' = open_out full_path in
    output_string file' new_str;
    close_out file';
    { path = full_path
    ; action = "created_file"
    }
  else
    (* Get text in file *)
    let file' = read_file_tool filename in
    match file' with
      | Some file'' ->
        let text = file''.content in
        (* Check if old_str is in text *)
        let index = is_substring old_str text in
        if index = false then
          { path = full_path
          ; action = "old_str_not_found"
          }
        else
          let edited = Str.global_replace (Str.regexp_string old_str) new_str text in
          let file' = open_out full_path in
          output_string file' edited;
          close_out file';
          { path = full_path
          ; action = "edited"
          }
      | None -> { path = full_path
                ; action = "file_not_found"
                }

(* Tool registry and system prompt generation *)

type tool_info =
  { name: string
  ; description: string
  ; signature: string
  }

let tool_registry : tool_info list =
  [ { name = "read_file"
    ; description = read_file_tool_desc
    ; signature = "read_file_tool : string -> file option"
    }
  ; { name = "list_files"
    ; description = file_list_tool_desc
    ; signature = "list_files_tool : string -> file_list option"
    }
  ; { name = "edit_file"
    ; description = edit_file_tool_desc
    ; signature = "edit_file_tool : string -> string -> string -> edit_file_result"
    }
  ]

let get_tool_str_representation (tool : tool_info) : string =
  Printf.sprintf "\nName: %s\nDescription: %s\nSignature: %s\n"
    tool.name
    tool.description
    tool.signature

let get_full_system_prompt () : string =
  let tool_str_repr = List.fold_left (fun acc tool ->
      acc ^ "TOOL\n===" ^ (get_tool_str_representation tool) ^ "\n" ^ (String.make 15 '=') ^ "\n"
    ) "" tool_registry in
  Printf.sprintf
    {|You are an AI coding assistant. You have access to the following tools:

%s

IMPORTANT: When you need to use a tool, you MUST respond with ONLY a single line in this exact format:
tool: TOOL_NAME({"arg1": "value1", "arg2": "value2"})

Examples:
- tool: read_file({"filename": "main.ml"})
- tool: list_files({"path": "src"})
- tool: edit_file({"path": "hello.lua", "old_str": "", "new_str": "print('Hello')"})

Do NOT use markdown code blocks. Do NOT add any other text on the tool line.
After you receive a tool_result(...) message, continue your task or respond normally.
If no tool is needed, respond with plain text.|} tool_str_repr
