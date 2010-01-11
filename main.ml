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
  | Controller.Table rows ->
      Template.VarTable begin
	List.map rows ~f:begin fun row ->
	  List.map row ~f:begin fun (name, cell) ->
	    (name, to_var cell)
	  end
	end
      end

let set t (name,var) =
  match var with
      Controller.String s ->
	t#set name s
    | Controller.Bool b ->
	t#conditional name b
    | Controller.Table rows ->
	t#table name begin
	  List.map rows ~f:begin
	    List.map ~f:begin fun (name,var) ->
	      (name,to_var var)
	    end
	  end
	end

let safe_int_of_string s =
  try
    int_of_string s
  with _ ->
    0

let index_page (cgi : cgi) =
  cgi#template @@ template "templates/index.html"

let search_page (cgi : cgi) =
  let configs =
    Config.read "modules.txt"
  in
  let modules =
    HList.concat_map (fun {Config.modules=m} -> m) configs
  in
  let page, content =
    Search.search (cgi#param "q") modules []
    +> Controller.pagenation ~window:20 ~offset:(try
						   int_of_string @@ cgi#param "o"
						 with  _ -> 0)
  in
  let result =
    content
    +> List.map ~f:(List.map ~f:(fun (name,v) -> (name,to_var v))
		      $ Controller.format configs)
  in
  let t =
    template "templates/search.html"
  in
    List.iter page ~f:(set t);
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
