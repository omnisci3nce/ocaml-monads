open Lib

let plusFive = (+) 5
let plusTen  = (+) 10
let plusFifteen = compose plusFive plusTen
let map_composition_of_functions fmap = fmap plusFifteen
let compose_mapped_functions fmap = compose (fmap plusFive) (fmap plusTen)

let test_list_id () =
  let open ListF in
  let test_id x = (fmap id x = x) in
  Alcotest.(check bool) "fmap id [1;2;3] = [1;2;3]" (test_id [1;2;3]) true

let test_list_compose () =
  let open ListF in
  let xs = [5; 10; 15] in
  Alcotest.(check (list int)) "fmap plusFifteen = compose (fmap plusFive) (fmap plusTen)" (map_composition_of_functions fmap xs) (compose_mapped_functions fmap xs)

let test_option_id () =
  let open OptionF in
  let test_id x = (fmap id x = x) in
  Alcotest.(check bool) "fmap id Some 5 = Some 5" (test_id (Some 5)) true

let test_option_compose () =
  let open OptionF in
  let s = Some 5 in
  Alcotest.(check (option int)) "fmap plusFifteen = compose (fmap plusFive) (fmap plusTen)" (map_composition_of_functions fmap s) (compose_mapped_functions fmap s)

let () =
  let open Alcotest in
  run "Functors" [
    "List", [
      test_case "Functor Law 1" `Quick test_list_id;
      test_case "Functor Law 2" `Quick test_list_compose
    ];
    "Option", [
      test_case "Functor Law 1" `Quick test_option_id;
      test_case "Functor Law 2" `Quick test_option_compose
    ]
  ]