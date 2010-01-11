open Base
open OUnit
open Controller
open StdLabels

let rec sort xs =
  List.sort xs ~cmp:(fun (x,_) (y,_) -> compare x y)
  +> List.map ~f:begin fun (name,x) ->
    match x with
	Table xs ->
	  (name,Table (List.map ~f:sort xs))
      | _ ->
	  (name,x)
  end

let ok x y = assert_equal ~printer:(fun xs -> String.concat ~sep:"\n" @@ List.map ~f:Std.dump xs) (sort x) (sort y)

let config = [
  { Config.name = "some package"; modules = ["A"; "B"]};
  { Config.name = "other package"; modules = ["String"]};
]

let _ = begin "controller.ml" >::: [
  "value" >:: begin fun () ->
    ok [
      "is_value"      , Bool true;
      "is_type"       , Bool false;
      "is_module"     , Bool false;
      "is_module_type", Bool false;
      "is_class"      , Bool false;
      "is_class_type" , Bool false;
      "name", String "concat";
      "package", String "other package";
      "module", String "String";
      "type", String "string -> string list -> string";
    ] @@ Controller.format config {
      Search.name = "concat";
      module_=["String"];
      kind = Search.Value "string -> string list -> string"}
  end;
  "type" >:: begin fun () ->
    ok [
      "is_value"      , Bool false;
      "is_type"       , Bool true;
      "is_module"     , Bool false;
      "is_module_type", Bool false;
      "is_class"      , Bool false;
      "is_class_type" , Bool false;
      "name", String "t";
      "package", String "other package";
      "module", String "String";
      "is_abstract", Bool false;
      "type", String "string";
    ] @@ Controller.format config {
      Search.name = "t";
      module_=["String"];
      kind = Search.Type "string" };
  end;
  "abstract type" >:: begin fun () ->
    ok [
      "is_value"      , Bool false;
      "is_type"       , Bool true;
      "is_module"     , Bool false;
      "is_module_type", Bool false;
      "is_class"      , Bool false;
      "is_class_type" , Bool false;
      "name", String "t";
      "package", String "other package";
      "module", String "String";
      "is_abstract", Bool true;
    ] @@ Controller.format config {
      Search.name = "t";
      module_=["String"];
      kind = Search.Type "" };
  end;
  "module" >:: begin fun () ->
    ok [
      "is_value"      , Bool false;
      "is_type"       , Bool false;
      "is_module"     , Bool true;
      "is_module_type", Bool false;
      "is_class"      , Bool false;
      "is_class_type" , Bool false;
      "name", String "String";
      "package", String "other package";
      "module", String "String";
    ]
    @@ Controller.format config {
      Search.name = "String";
      module_=["String"];
      kind = Search.Module }
  end;
  "module type" >:: begin fun () ->
    ok [
      "is_value"      , Bool false;
      "is_type"       , Bool false;
      "is_module"     , Bool false;
      "is_module_type", Bool true;
      "is_class"      , Bool false;
      "is_class_type" , Bool false;
      "name", String "String";
      "package", String "other package";
      "module", String "String";
    ]
    @@ Controller.format config {
      Search.name = "String";
      module_=["String"];
      kind = Search.ModuleType }
  end;
  "class" >:: begin fun () ->
    ok [
      "is_value"      , Bool false;
      "is_type"       , Bool false;
      "is_module"     , Bool false;
      "is_module_type", Bool false;
      "is_class"      , Bool true;
      "is_class_type" , Bool false;
      "name", String "String";
      "package", String "other package";
      "module", String "String";
    ]
    @@ Controller.format config {
      Search.name = "String";
      module_=["String"];
      kind = Search.Class }
  end;
  "class type" >:: begin fun () ->
    ok [
      "is_value"      , Bool false;
      "is_type"       , Bool false;
      "is_module"     , Bool false;
      "is_module_type", Bool false;
      "is_class"      , Bool false;
      "is_class_type" , Bool true;
      "name", String "String";
      "package", String "other package";
      "module", String "String";
    ]
    @@ Controller.format config {
      Search.name = "String";
      module_=["String"];
      kind = Search.ClassType }
  end;
  "pagenation(first)" >:: begin fun () ->
    let opt,xs =
      pagenation ~offset:0 ~window:10 @@ range 0 30 in
      ok ["from", String "1";
	  "to"  , String "10";
	  "count", String "30";
	  "is_next", Bool true;
	  "next_offset", String "10";
	  "is_prev", Bool false;
	  "navigation", Table [
	    ["is_current", Bool true;
	     "number", String "1"];
	    ["is_current", Bool false;
	     "number", String "2";
	     "offset", String "10"];
	    ["is_current", Bool false;
	     "number", String "3";
	     "offset", String "20"]
	  ]]
	opt;
      assert_equal (range 0 10) xs
  end;
  "pagenation(middle)" >:: begin fun () ->
    let opt,xs =
      pagenation ~offset:10 ~window:10 @@ range 0 30 in
      ok ["from", String "11";
	  "to"  , String "20";
	  "count", String "30";
	  "is_next", Bool true;
	  "next_offset", String "20";
	  "is_prev", Bool true;
	  "prev_offset", String "0";
	  "navigation", Table [
	    ["is_current", Bool false;
	     "number", String "1";
	     "offset",String "0"];
	    ["is_current", Bool true;
	     "number", String "2"];
	    ["is_current", Bool false;
	     "number", String "3";
	     "offset", String "20"]
	  ]]
	opt;
      assert_equal (range 10 20) xs
  end;
  "pagenation(last)" >:: begin fun () ->
    let opt,xs =
      pagenation ~offset:20 ~window:10 @@ range 0 30 in
      ok ["from", String "21";
	  "to"  , String "30";
	  "count", String "30";
	  "is_next", Bool false;
	  "is_prev", Bool true;
	  "prev_offset", String "10";
	  "navigation", Table [
	    ["is_current", Bool false;
	     "number", String "1";
	     "offset",String "0"];
	    ["is_current", Bool false;
	     "number", String "2";
	     "offset", String "10"];
	    ["is_current", Bool true;
	     "number", String "3"]
	  ]]
	opt;
      assert_equal (range 20 30) xs
  end;

] end +> run_test_tt_main
