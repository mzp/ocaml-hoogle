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
  module_ : string list;
  name    : string;
  kind    : kind
}

let init_modules =
  !Searchid.module_list

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

let string_of_value id =
  let name =
    match id with
	Longident.Lident x -> x
      | Longident.Ldot (_, x) -> x
      | _ -> "z"
  in
  let _, vd =
    Env.lookup_value id !Searchid.start_env
  in
    Str.replace_first (Str.regexp "=[^=]*$") "" @@
      Str.replace_first (Str.regexp "^[^:]*: *") ""
      (string_of_sign [Types.Tsig_value (Ident.create name, vd)])

let ident_of_path ~default = function
    Path.Pident i -> i
  | Path.Pdot (_, s, _) -> Ident.create s
  | Path.Papply _ -> Ident.create default

let dummy_item = Tsig_modtype (Ident.create "dummy", Tmodtype_abstract)

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
		      [Tsig_cltype (ident_of_path path ~default:"ct", clt, Trec_first);
		       dummy_item; dummy_item]
	      | _ -> raise Not_found
    with Not_found ->
      string_of_sign
	[Tsig_type(ident_of_path path ~default:"t", td, Trec_first)]

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


let infix s =
  if Str.string_match (Str.regexp "[!$%&*+-./:<=>?@^|~]+") s 0 then
    Printf.sprintf "(%s)" s
  else
    s

let to_result (id, kind) =
  let id' =
    Longident.flatten id
  in
  let module_ xs =
    match xs with
	[]  -> []
      | [x] -> [x]
      |  _  -> HList.init xs
  in
  let t =
    {
      module_ = module_ id';
      name    = HList.last id';
      kind = Other
    }
  in
    match kind with
	Searchid.Pvalue ->
	  { t with kind = Value (string_of_value id); name = infix t.name}
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

let lift f s =
  s
  +> sure f
  +> List.map ~f:to_result

let search s modules paths =
  init modules paths;
  lift (Searchid.search_string_type ~mode:`Included) s
  @ lift Searchid.search_string_symbol  s
  @ lift Searchid.search_pattern_symbol s

