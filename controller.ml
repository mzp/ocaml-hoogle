open Base
open StdLabels

type t =
    String of string
  | Bool   of bool
  | Table  of (string * t) list list

let find_package module_ configs =
  let config =
    List.find configs
      ~f:(fun { Config.modules=modules } -> List.mem (List.hd module_) modules)
  in
    config.Config.name

let module_ =
  function [] -> [""]
    | [x] -> [x]
    | xs -> HList.init xs

let format configs {Search.id=id; kind=kind} =
  let kind, opt =
    match kind with
	Search.Value s ->
	  "value",[ "type", String s]
      | Search.Type "" ->
	  "type",[ "is_abstract", Bool true]
      | Search.Type s ->
	  "type",[ "is_abstract", Bool false;
		   "type", String s ]
      | Search.Module ->
	  "module",["module_name", String (String.concat ~sep:"." id)]
      | Search.ModuleType ->
	  "module_type",[]
      | Search.Class ->
	  "class",[]
      | Search.ClassType ->
	  "class_type",[]
      | _ ->
	  "", []
  in
    ["module" , String (String.concat ~sep:"." @@ module_ id);
     "name"   , String (HList.last id);
     "package", String (find_package id configs)]
    @ List.map  ["value"; "type"; "module"; "module_type"; "class"; "class_type"]
      ~f:(fun x -> ("is_" ^ x, Bool (x = kind)))
    @ opt

let int n =
  String (string_of_int n)

let pagenation ~offset ~window xs =
  let window =
    max 0 window
  in
  let count =
    List.length xs
  in
  let from =
    max 0 offset
  in
  let to_ =
    min count (offset+window)
  in
    List.concat [
      ["from" , int @@ from + 1;
       "to"   , int @@ to_;
       "count", int @@ count];
      if to_ < count then
	["is_next", Bool true;
	 "next_offset", int to_]
       else
	["is_next", Bool false];
      if 0 < from then
	["is_prev", Bool true;
	 "prev_offset", int @@ from - window]
      else
	["is_prev", Bool false];
      ["navigation", Table (
	 List.map (range 0 (int_of_float @@ ceil @@ (float_of_int count) /. (float_of_int window)))
	   ~f:begin fun i ->
	   if i*window <= offset && offset < (i+1)*window then
	     ["is_current", Bool true;
	      "number", int @@ i+1]
	   else
	     ["is_current", Bool false;
	      "number", int @@ i+1;
	      "offset", int @@ i*window]
	 end
       )]
    ], HList.take window @@ HList.drop offset xs

let available configs =
  Table begin
    List.map configs ~f:begin fun { Config.name = name; modules = modules} ->
      ["package", String name;
       "modules", Table begin
	 List.map modules ~f:begin fun s ->
	   ["name", String s]
	 end
       end ]
    end
  end
