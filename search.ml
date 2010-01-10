open Base
open StdLabels

type desc =
    Value of string
  | Type
  | Label
  | Constructor
  | Module
  | ModuleType
  | Class
  | ClassType

type t = {
  module_ : string;
  package : string;
  name    : string;
  desc    : desc
}

let init configs =
  (* initialize *)
  Toploop.set_paths ();
  Searchid.module_list :=
    HList.concat_map (fun s -> s.Config.modules) configs
  @ !Searchid.module_list

let sure f x =
  try
    f x
  with Searchid.Error _ ->
    []

let string_of_sign sign =
  let b =
    Buffer.create 10
  in
  let ppf =
    Format.formatter_of_buffer b
  in
    Printtyp.signature ppf sign;
    Format.pp_print_flush ppf ();
    Buffer.contents b

let find_package id configs =
  try
    Config.find_package (List.hd id) configs
  with _ ->
    "<unknown package>"

let to_result configs (id, kind) =
  let id' =
    Longident.flatten id
  in
  let name =
    match id with
	Longident.Lident x -> x
      | Longident.Ldot (_, x) -> x
      | _ -> match kind with Searchid.Pvalue | Searchid.Ptype | Searchid.Plabel -> "z" | _ -> "Z"
  in
    match kind with
	Searchid.Pvalue ->
	  let _, vd =
	    Env.lookup_value id !Searchid.start_env
	  in
	  let t =
	    Str.replace_first (Str.regexp "^[^:]*:") ""
	      (string_of_sign [Types.Tsig_value (Ident.create name, vd)])
	  in
	  {
	    module_ = String.concat ~sep:"." @@ HList.init id';
	    name    = HList.last id';
	    package = find_package id' configs;
	    desc   =  Value t
	  }
      | _ ->
	  {
	    module_ = String.concat ~sep:"." @@ HList.init id';
	    name    = HList.last id';
	    package = find_package id' configs;
	    desc = Type
	  }

let lift f configs s =
  s
  +> sure f
  +> List.map ~f:(to_result configs)

let search s configs =
  init configs;
  lift (Searchid.search_string_type ~mode:`Included) configs s
  @ lift Searchid.search_string_symbol  configs s
  @ lift Searchid.search_pattern_symbol configs s
