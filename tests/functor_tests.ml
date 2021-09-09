open Lib

let plusFive = (+) 5
let plusTen  = (+) 10
let plusFifteen = compose plusFive plusTen

let test_list_id () =
  let open ListF in
  let test_id x = (fmap id x = x) in
  Alcotest.(check bool) "fmap id [1;2;3] = [1;2;3]" (test_id [1;2;3]) true

let test_list_compose () =
  let open ListF in
  let map_composition_of_functions = fmap plusFifteen [5;10;15] in
  let compose_mapped_functions     = compose (fmap plusFive) (fmap plusTen) [5;10;15] in

  Alcotest.(check (list int)) "fsf" map_composition_of_functions compose_mapped_functions

let test_result_id () = ()

let test_result_compose () = ()

let () =
  let open Alcotest in
  run "Functors" [
    "List", [
      test_case "Functor Law 1" `Quick test_list_id;
      test_case "Functor Law 2" `Quick test_list_compose
    ];
    "Result", [
      test_case "Functor Law 1" `Quick test_result_id;
      test_case "Functor Law 2" `Quick test_result_compose
    ]
  ]