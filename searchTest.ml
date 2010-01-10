open Base
open OUnit
open Search

let ok x y = assert_equal ~printer:Std.dump x y

let _ = begin "search.ml" >::: [
  "empty" >:: begin fun () ->
    ok [] @@ search "" []
  end;
  "value" >:: begin fun () ->
    ok [{name = "concat";
	 module_=["String"];
	 kind = Value "string -> string list -> string"}] @@
      search "concat" ["String"]
  end;
  "nest module" >:: begin fun () ->
    ok [{name = "concat";
	 module_=["StdLabels";"String"];
	 kind = Value "sep:string -> string list -> string"}] @@
      search "string list -> string" ["StdLabels"]
  end;
  "type" >:: begin fun () ->
    ok {name = "t"; module_=["String"]; kind = Type "string"} @@
      List.nth (search "t" ["String"]) 1
  end;
  "module" >:: begin fun () ->
    ok [{name = "String"; module_=["String"]; kind = Module}] @@
      (search "String" ["String"])
  end;
  "module sig" >:: begin fun () ->
    ok [{name = "S"; module_=["Set"]; kind = ModuleType}] @@
      (search "S" ["Set"])
  end;
] end +> run_test_tt_main


