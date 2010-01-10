open Base
open StdLabels
open CamlGI
open CamlGI.Cgi
open CamlGI.Template

open Search

let index_page (cgi : cgi) =
  cgi#template @@ template "templates/index.html"

let search_page (cgi : cgi) =
  let to_table t =
    let kind, opt =
      match t.desc with
	  Value s ->
	    "value",[ "type", Template.VarString s]
	| Type s ->
	    "type",[ "type", Template.VarString s;
		     "is_abstract", Template.VarConditional (s = "")]
	| _ ->
	    "", []
    in
      ["module" , Template.VarString t.module_;
       "name"   , Template.VarString t.name;
       "package", Template.VarString t.package]
      @ List.map  ["value"; "type"] ~f:(fun x -> ("is_" ^ x,Template.VarConditional (x = kind)))
      @ opt
  in
  let result =
    Search.search (cgi#param "q") (Config.read "modules.txt")
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
