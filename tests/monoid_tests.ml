let dummy_test () = ()

let () =
  let open Alcotest in
  run "Monoids" [
    "Sum", [
      test_case "test 1" `Quick dummy_test;
      ];
      "Prod", [
        test_case "test 1" `Quick dummy_test;
    ]
  ]