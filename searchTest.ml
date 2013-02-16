open Base
open OUnit
open Search

let ok x y = assert_equal ~printer:Std.dump x y

let path =
  input_line @@ Unix.open_process_in "ocamlfind query extlib"

let _ = begin "search.ml" >::: [
  "empty" >:: begin fun () ->
    ok [] @@ search "" [] []
  end;
  "value" >:: begin fun () ->
    ok [{id = ["String";"concat"];
	 kind = Value "string -> string list -> string"}] @@
      search "concat" ["String"] []
  end;
  "nest module" >:: begin fun () ->
    ok [{id=["StdLabels";"String";"concat"];
	 kind = Value "sep:string -> string list -> string"}] @@
      search "string list -> string" ["StdLabels"] []
  end;
  "type" >:: begin fun () ->
    ok {id=["String"; "t"]; kind = Type "string"} @@
      List.nth (search "t" ["String"] []) 0
  end;
  (* CR jfuruse: We fail here 
  "module" >:: begin fun () ->
    ok [{id=["String"]; kind = Module}] @@
      search "String" ["String"] []
  end; *)
  "module sig" >:: begin fun () ->
    ok [{id=["Set";"S"]; kind = ModuleType}] @@
      search "S" ["Set"] []
  end;
  "external" >:: begin fun () ->
    ok [{id=["Std";"dump"];
	 kind = Value "'a -> string"}] @@
      search "dump" ["Std"] [path]
  end;
] end +> run_test_tt_main
