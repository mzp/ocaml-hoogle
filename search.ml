open Base
open StdLabels

let _ =
  (* initialize *)
  Toploop.set_paths ();
  Searchid.module_list := "String"::"Pervasives"::!Searchid.module_list

type t = {
  module_: string;
  package : string;
  name : string;
  type_  : string;
}

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


let to_result (id, kind) =
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
	  {
	    module_ = String.concat ~sep:"." @@ HList.init id';
	    name    = HList.last id';
	    package = "<not yet>";
	    type_   = Str.replace_first (Str.regexp "^val[^:]*:") ""
	      (string_of_sign [Types.Tsig_value (Ident.create name, vd)])
	  }
      | _ ->
	  {
	    module_ = String.concat ~sep:"." @@ HList.init id';
	    name    = HList.last id';
	    package = "<not yet>";
	    type_   = "<not yet>"
	  }

let lift f s =
  s
  +> sure f
  +> List.map ~f:to_result

let search s =
  lift (Searchid.search_string_type ~mode:`Included) s
  @ lift Searchid.search_string_symbol s
  @ lift Searchid.search_pattern_symbol s
