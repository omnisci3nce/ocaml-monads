let dummy_test () = ()

let () =
  let open Alcotest in
  run "Monads" [
    "State", [
      test_case "test 1" `Quick dummy_test;
    ]
  ]