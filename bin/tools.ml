open Option
open Str

let run_shell_cmd_with_result cmd : string =
  let ic = Unix.open_process_in cmd in
  let output = input_line ic in
  let _ = Unix.close_process_in ic in
  output

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

(* Grep *)
let grep_tool_desc =
  {|Searches for a pattern in a file.
:param path: The path to the file to search in.
:param pattern: The pattern to search for.
:return: A list of dictionaries with the path to the file and the line number where the pattern was found.|}

type grep_result =
  { path: string
  ; line_number: int
  }

type grep_result_list =
  { path: string
  ; results: grep_result list
  }

let grep_tool filename pattern : grep_result_list option =
  let full_path = resolve_abs_path filename in
  if Sys.file_exists full_path then
    let file_text = In_channel.with_open_text full_path In_channel.input_all in
    let lines = Str.split (Str.regexp_string "\n") file_text in
    let results = List.mapi (fun i line ->
        let line_number = i + 1 in
        let re = Str.regexp_string pattern in
        if Str.string_match re line 0 then
          { path = full_path
          ; line_number = line_number
          }
        else
          { path = full_path
          ; line_number = -1
          }
      ) lines in
    Some { path = full_path
         ; results = results
         }
  else None

(* SED *)
let sed_tool_desc =
  {|Searches for a pattern in a file and replaces it with a new string.
:param path: The path to the file to search in.
:param pattern: The pattern to search for.
:param new_str: The string to replace the pattern with.
:return: A list of dictionaries with the path to the file and the line number where the pattern was found.|}

type sed_result =
  { path: string
  ; line_number: int
  }

type sed_result_list =
  { path: string
  ; results: sed_result list
  }

let sed_tool filename pattern new_str : sed_result_list =
  let output = run_shell_cmd_with_result (Printf.sprintf "sed -i 's/%s/%s/g' %s" pattern new_str filename) in
  let results = List.map (fun line ->
      let re = Str.regexp_string pattern in
      if Str.string_match re line 0 then
        { path = filename
        ; line_number = 1
        }
      else
        { path = filename
        ; line_number = -1
        }
    ) (Str.split (Str.regexp_string "\n") output) in
  { path = filename
  ; results = results
  }

(* Find *)
let find_tool_desc =
  {|Searches for a pattern in a file.
:param path: The path to the file to search in.
:param pattern: The pattern to search for.
:return: A list of dictionaries with the path to the file and the line number where the pattern was found.|}

type find_result =
  { path: string
  ; line_number: int
  }

type find_result_list =
  { path: string
  ; results: find_result list
  }

let find_tool filename pattern : find_result_list option =
  let full_path = resolve_abs_path filename in
  if Sys.file_exists full_path then
    let file_text = In_channel.with_open_text full_path In_channel.input_all in
    let lines = Str.split (Str.regexp_string "\n") file_text in
    let results = List.mapi (fun i line ->
        let line_number = i + 1 in
        let re = Str.regexp_string pattern in
        if Str.string_match re line 0 then
          { path = full_path
          ; line_number = line_number
          }
        else
          { path = full_path
          ; line_number = -1
          }
      ) lines in
    Some { path = full_path
         ; results = results
         }
  else None

(* Run NuShell Commands *)
let run_nushell_cmd_tool_desc =
  {|Runs a Nu shell command and returns the output.
:param cmd: The command to run.
:return: The output of the command.|}

type run_nushell_cmd_result =
  { cmd: string
  ; output: string
  }

let run_nushell_cmd_tool cmd : run_nushell_cmd_result =
  let cmd_in_quotes = Str.global_replace (Str.regexp_string "\"") "\\\"" cmd in
  let nu_cmd = Printf.sprintf "nu -c \"%s\"" cmd_in_quotes in
  let output = run_shell_cmd_with_result nu_cmd in
  { cmd = nu_cmd
  ; output = output
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
    ; signature = "read_file : string -> file option"
    }
  ; { name = "list_files"
    ; description = file_list_tool_desc
    ; signature = "list_files : string -> file_list option"
    }
  ; { name = "edit_file"
    ; description = edit_file_tool_desc
    ; signature = "edit_file : string -> string -> string -> edit_file_result"
    }
  ; { name = "grep"
    ; description = grep_tool_desc
    ; signature = "grep : string -> string -> grep_result_list option"
    }
  ; { name = "sed"
    ; description = sed_tool_desc
    ; signature = "sed : string -> string -> string -> sed_result_list"
    }
  ; { name = "find"
    ; description = find_tool_desc
    ; signature = "find : string -> string -> find_result_list option"
    }
  ; { name = "nu"
    ; description = run_nushell_cmd_tool_desc
    ; signature = "nu: string -> run_nushell_cmd_result"
    }
  ]

let get_tool_str_representation (tool : tool_info) : string =
  Printf.sprintf {|<tool name="%s">
  <description>%s</description>
  <signature>%s</signature>
</tool>|}
    tool.name
    tool.description
    tool.signature

let get_full_system_prompt () : string =
  let tool_str_repr = List.fold_left (fun acc tool ->
      acc ^ (get_tool_str_representation tool) ^ "\n"
    ) "" tool_registry in
  Printf.sprintf
    {|You are an AI coding assistant. You have access to the following tools:

<tools>
%s</tools>

IMPORTANT: When you need to use a tool, you MUST respond with ONLY a single line in this exact format:
<tool_call name="TOOL_NAME">{"arg1": "value1", "arg2": "value2"}</tool_call>

Examples:
- <tool_call name="read_file">{"filename": "main.ml"}</tool_call>
- <tool_call name="list_files">{"path": "src"}</tool_call>
- <tool_call name="edit_file">{"path": "hello.lua", "old_str": "", "new_str": "print('Hello')"}</tool_call>

Do NOT use markdown code blocks. Do NOT add any other text on the tool line.
After you receive a <tool_result>...</tool_result> message, continue your task or respond normally.
If no tool is needed, respond with plain text.|} tool_str_repr
