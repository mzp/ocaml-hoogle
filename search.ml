open Base
open StdLabels

let _ =
  (* initialize *)
  Toploop.set_paths ();
  Searchid.module_list := "String"::"Pervasives"::!Searchid.module_list

type t = {
  module_: string;
  package : string;
  name   : string;
  type_  : string;
}

let sure f x =
  try
    f x
  with Searchid.Error _ ->
    []

let to_result (id, kind) =
  let id' =
    Longident.flatten id
  in
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
