open Base
open StdLabels

let _ =
  let _ =
    Toploop.set_paths () in
  let _ =
    Searchid.module_list := "String"::!Searchid.module_list
  in
  List.iter (Searchid.search_string_type Sys.argv.(1) `Included) ~f:begin
    fun (id, kind) ->
      print_endline @@ String.concat ~sep:"." @@ Longident.flatten id
  end
