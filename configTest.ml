open Base
open OUnit
open Config

let _ =
  open_out_with "config-test.txt" begin fun ch ->
    output_string ch "- some package\n";
    output_string ch "PATH:: /path/to/some\n";
    output_string ch "A\n";
    output_string ch "B\n";
    output_string ch "\n";
    output_string ch "- other package\n";
    output_string ch "C\n"
  end

let _ = begin "config.ml" >::: [
  "read" >:: begin fun () ->
    assert_equal ~printer:Std.dump [
      { name = "some package"; modules = ["A"; "B"]; path=Some "/path/to/some"};
      { name = "other package"; modules = ["C"]; path=None};
    ]  @@
      Config.read "config-test.txt"
  end;
] end +> run_test_tt_main

