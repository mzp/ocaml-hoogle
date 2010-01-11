open Base
open Str
type t = {
  name : string;
  path : string option;
  modules : string list
}

let name    = ref ""
let path    = ref None
let modules = ref []
let configs = ref []

let chop s =
  let r =
    regexp "[\r\n]"
  in
    global_replace r "" s

let add_current () =
  if !name <> "" then
    configs := { name = !name; modules = List.rev !modules; path= !path } ::
      !configs;
  name := "";
  path := None;
  modules := []

let r_package =
  regexp "^- *\\(.*\\)$"

let r_path =
  regexp "^PATH *:: *\\(.*\\)$"

let parse_line s =
  if s = "" then
    ()
  else if string_match r_path s 0 then
    path := Some (matched_group 1 s)
  else if string_match r_package s 0 then begin
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
    List.rev !configs

