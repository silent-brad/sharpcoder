# TODO
- [x] Add Shell commands (use Nushell)
- [x] Add grep, sed tools..
- [x] Changed system tool prompts to XML-style
- [ ] Add "rm", etc tools

- [x] Implement [Recusive Language Models](https://arxiv.org/html/2512.24601v1)
  - Auto-triggers when `read_file` encounters files > 50k chars
  - Large files loaded into `$CONTEXT`, LLM uses `nu_exec` (NuShell) to filter/search
  - `sub_agent` tool spawns focused sub-LMs for semantic analysis of chunks
