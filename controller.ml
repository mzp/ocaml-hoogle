open Base
open StdLabels

type t =
    String of string
  | Bool   of bool

let find_package module_ configs =
  let config =
    List.find configs
      ~f:(fun { Config.modules=modules } -> List.mem (List.hd module_) modules)
  in
    config.Config.name

let format configs x =
  let kind, opt =
    match x.Search.kind with
	Search.Value s ->
	  "value",[ "type", String s]
      | Search.Type "" ->
	  "type",[ "is_abstract", Bool true]
      | Search.Type s ->
	  "type",[ "is_abstract", Bool false;
		   "type", String s ]
      | Search.Module ->
	  "module",[]
      | Search.ModuleType ->
	  "module_type",[]
      | Search.Class ->
	  "class",[]
      | Search.ClassType ->
	  "class_type",[]
      | _ ->
	  "", []
  in
    ["module" , String (String.concat ~sep:"." x.Search.module_);
     "name"   , String x.Search.name;
     "package", String (find_package x.Search.module_ configs)]
    @ List.map  ["value"; "type"; "module"; "module_type"; "class"; "class_type"]
      ~f:(fun x -> ("is_" ^ x, Bool (x = kind)))
    @ opt
