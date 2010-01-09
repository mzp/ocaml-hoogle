open Base
open Str
type t = {
  name : string;
  modules : string list
}

let name    = ref ""
let modules = ref []
let configs = ref []

let chop s =
  let r =
    regexp "[\r\n]"
  in
    global_replace r "" s

let add_current () =
  if !name <> "" then
    configs := { name = !name; modules = !modules } :: !configs;
  name := ""

let parse_line s =
  let r =
    regexp "- *\\(.*\\)"
  in
    if string_match r s 0 then begin
      add_current ();
      name := matched_group 1 s
    end else
      modules := s :: !modules

let read path =
  configs := [];
  name := "";
  try
    open_in_with path begin fun ch ->
      while true do
	parse_line @@ chop @@ input_line ch
      done;
    end;
    failwith "must not happen"
  with End_of_file ->
    add_current ();
    !configs

let find_package module_ configs =
  let config =
    List.find
      (fun { modules=modules } -> List.mem module_  modules)
      configs
  in
    config.name
