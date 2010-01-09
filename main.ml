open Base
open StdLabels
open CamlGI
open CamlGI.Cgi
open CamlGI.Template


(*let _ =
  let _ =
    Toploop.set_paths () in
  let _ =
    Searchid.module_list := "String"::!Searchid.module_list
  in
  List.iter (Searchid.search_string_type Sys.argv.(1) `Included) ~f:begin
    fun (id, kind) ->
      print_endline @@ String.concat ~sep:"." @@ Longident.flatten id
  end
*)

(* initialize *)
let _ =
  Toploop.set_paths ();
  Searchid.module_list := "String"::!Searchid.module_list

let index_page (cgi : cgi) =
  cgi#template @@ template "templates/index.html"

let _ =
  register_script begin fun req ->
    let q =
      new cgi req
    in
      q#header ~content_type:"text/html; charset=utf-8" ();
      index_page q
  end

