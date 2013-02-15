open Base
open StdLabels
open Types

type kind =
    Value of string
  | Type of string
  | Module
  | ModuleType
  | Class
  | ClassType
  | Other

type t = {
  id : string list;
  kind    : kind
}

let init_modules =
  !Searchid.module_list

module Toploop = struct
  open Config
  open Misc

  let init_path () =
    let dirs =
      if !Clflags.use_threads then "+threads" :: !Clflags.include_dirs
      else if !Clflags.use_vmthreads then "+vmthreads" :: !Clflags.include_dirs
      else !Clflags.include_dirs in
    let exp_dirs =
      List.map (expand_directory Config.standard_library) dirs in
    load_path := "" :: List.rev_append exp_dirs (Clflags.std_include_dir ());
    Env.reset_cache ()

  let set_paths () =
    init_path ();
    (* Add whatever -I options have been specified on the command line,
       but keep the directories that user code linked in with ocamlmktop
       may have added to load_path. *)
    load_path := !load_path @ [Filename.concat Config.standard_library "camlp4"];
    load_path := "" :: (List.rev !Clflags.include_dirs @ !load_path);
    Dll.add_path !load_path;
end

module Topdirs = struct
  open Misc
  let dir_directory s =
    let d = expand_directory Config.standard_library s in
    Config.load_path := d :: !Config.load_path;
    Dll.add_path [d]
end

let init modules paths =
  (* initialize *)
  Toploop.set_paths ();
  List.iter ~f:Topdirs.dir_directory paths;
  Searchid.module_list := modules @ init_modules

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

(* CR jfuruse: We can get vd.val_type and print it *)
let string_of_value id =
  let name =
    match id with
    | Longident.Lident x -> x 
    | Longident.Ldot (_, x) -> x
    | _ -> "z"
  in
  let _, vd =
    Env.lookup_value id !Searchid.start_env
  in
  Str.replace_first (Str.regexp "=[^=]*$") ""  (* remove equality of external *)
  @@ Str.replace_first (Str.regexp "^[^:]*: *") "" (* remove "val id :" "exteranl id :" *)
    (string_of_sign [Types.Sig_value (Ident.create name, vd)])

let ident_of_path ~default = function
    Path.Pident i -> i
  | Path.Pdot (_, s, _) -> Ident.create s
  | Path.Papply _ -> Ident.create default

let dummy_item = Sig_modtype (Ident.create "dummy", Modtype_abstract)

let string_of_type_decl path =
  let td =
    Env.find_type path !Searchid.start_env
  in
    try
      match td.type_manifest with
	  None ->
	    raise Not_found
	| Some ty ->
	    match Ctype.repr ty with
		{ desc = Tobject _} ->
		  let
		      clt = Env.find_cltype path !Searchid.start_env
		  in
		    string_of_sign
		      [Sig_class_type (ident_of_path path ~default:"ct", clt, Trec_first);
		       dummy_item; dummy_item]
	      | _ -> raise Not_found
    with Not_found ->
      string_of_sign
	[Sig_type(ident_of_path path ~default:"t", td, Trec_first)]

let string_of_type id =
  let strip s =
    if String.contains s '=' then
      Str.replace_first (Str.regexp "^[^=]*= *") "" s
    else
      ""
  in
  let path, decl =
    Env.lookup_type id !Searchid.start_env in
    strip @@ string_of_type_decl path


(** Wrap infix/perfix operators with "(" and ")" *)
let infix s =
  if Str.string_match (Str.regexp "[!$%&*+-./:<=>?@^|~]+") s 0 then
    Printf.sprintf "(%s)" s
  else
    s

let to_result (id, kind) =
  let id' =
    Longident.flatten id
  in
  let t =
    {
      id = id';
      kind = Other
    }
  in
    match kind with
	Searchid.Pvalue ->
          (* We can apply infix only against the last elem *)
	  {kind = Value (string_of_value id); id = List.map ~f:infix id'}
      | Searchid.Ptype ->
	  { t with kind = Type (string_of_type id) }
      | Searchid.Pmodule ->
	  { t with kind = Module }
      | Searchid.Pmodtype ->
	  { t with kind = ModuleType }
      | Searchid.Pclass ->
	  { t with kind = Class }
      | Searchid.Pcltype ->
	  { t with kind = ClassType }
      | _ ->
	  t

let raw_search s modules paths =
  init modules paths; 
  sure (Searchid.search_string_type ~mode:`Exact) s
  @ sure (Searchid.search_string_type ~mode:`Included) s
  @ sure Searchid.search_pattern_symbol s

let search s modules paths =
  List.rev 
  @@ ExtList.List.unique 
  @@ List.rev_map ~f:to_result 
  @@ raw_search s modules paths
