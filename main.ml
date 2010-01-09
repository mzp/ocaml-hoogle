open Base
open StdLabels
open CamlGI
open CamlGI.Cgi
open CamlGI.Template

open Search

let init () =
  (* initialize *)
  let modules =
    Config.read "modules.txt"
  in
    Toploop.set_paths ();
    Searchid.module_list :=
      HList.concat_map (fun s -> s.Config.modules) modules
    @ !Searchid.module_list

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
    init ();
    Search.search @@ cgi#param "q"
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
