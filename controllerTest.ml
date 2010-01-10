open Base
open OUnit
open Controller

let sort xs = List.sort (fun x y -> compare (fst x) (fst y)) xs

let ok x y = assert_equal (sort x) (sort y)

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
] end +> run_test_tt_main
