open OUnit2
open Pcxlib

let test_example_file _ =
  let img = Pcx.load "../../../test/DSCF3321.pcx" in
  match img with
  | Result.Error s -> assert_failure (Printf.sprintf "Got unexecpted error: %s" s)
  | Result.Ok img -> (
    assert_equal (Pcx.dimensions img) (1023, 767);
    assert_equal (Pcx.encoding img) RLE;
    assert_equal (Pcx.depth img) 8;
    assert_equal (Pcx.planes img) 1;
  )

let suite =
  "BasicLoading" >::: [
    "Load file" >:: test_example_file;
  ]

let () =
  run_test_tt_main suite
