open Base
open StdLabels
open CamlGI
open CamlGI.Cgi
open CamlGI.Template

open Search

let rec to_var = function
    Controller.String s ->
      Template.VarString s
  | Controller.Bool b ->
      Template.VarConditional b

let index_page (cgi : cgi) =
  cgi#template @@ template "templates/index.html"

let search_page (cgi : cgi) =
  let configs =
    Config.read "modules.txt"
  in
  let result =
    HList.concat_map (fun {Config.modules=m} -> m) configs
    +> Search.search (cgi#param "q")
    +> List.map ~f:(List.map ~f:(fun (name,v) -> (name,to_var v))
		  $ Controller.format configs)
  in
  let t =
    template "templates/search.html"
  in
    t#set "query" @@ cgi#param "q";
    t#conditional "found" @@ (result <> []);
    t#table "result" result;
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
