open Base
open StdLabels
open CamlGI
open CamlGI.Cgi
open CamlGI.Template


let _ =
  (* initialize *)
  Toploop.set_paths ();
  Searchid.module_list := "String"::!Searchid.module_list

type t = {
  module_: string;
  package : string;
  name   : string;
  type_  : string
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

let index_page (cgi : cgi) =
  cgi#template @@ template "templates/index.html"

let search_page (cgi : cgi) =
  let to_table t =
    ["module" , Template.VarString t.module_;
     "name"   , Template.VarString t.name;
     "type"   , Template.VarString t.type_;
     "package", Template.VarString t.package]
  in
  let result =
    search @@ cgi#param "q"
  in
  let t =
    template "templates/search.html"
  in
    t#set "query" @@ cgi#param "q";
    t#conditional "found" @@ (result <> []);
    t#table "result" @@ List.map ~f:to_table result;
    cgi#template t

let _ =
  register_script begin fun req ->
    let q =
      new cgi req
    in
      q#header ~content_type:"text/html; charset=utf-8" ();
      if q#param_exists "q" then
	search_page q
      else
	index_page q
  end
