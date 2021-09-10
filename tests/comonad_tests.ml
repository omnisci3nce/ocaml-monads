let xs = [0; 0; 1; 2; 3; 0; 0]

let test_sum_neighbours () = () 

let dummy_test () = ()

let () =
  let open Alcotest in
  run "Comonads" [
    "Zipper", [
      test_case "sum neighbours" `Quick test_sum_neighbours;
      test_case "extend extract = id" `Quick dummy_test;
      test_case "extract . extend f = f" `Quick dummy_test;
      test_case "extend f . extend g = extend (f . extend g)" `Quick dummy_test
    ]
  ]