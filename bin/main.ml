open Lib

let examples1 () =
  let xs = [2; 3; 4] in
  let ss = List.map Int.to_string xs in
  let sum =
    List.fold_left Sum.append Sum.empty in
  let prod =
    List.fold_left Prod.append Prod.empty in
  let concat = List.fold_left StringM.append StringM.empty in
  Printf.printf "sum: %d prod: %d string: %s\n" (sum xs) (prod xs) (concat ss)

let examples2 () = 
  let xs = [2; 3; 4] in
  let module SumM = Monoid_Utils(Sum) in
  let module ProdM = Monoid_Utils(Prod) in
  let sum = SumM.concat in
  let prod = ProdM.concat in
  Printf.printf "sum: %d prod: %d\n" (sum xs) (prod xs)

let examples3 () =
  let xs   = [true; false; false; true] in
  let xs'  = [true; true] in
  let xs'' = [false; false] in
  let module AllU = Monoid_Utils(All) in
  let module AnyU = Monoid_Utils(Any) in
  Printf.printf "all: %b %b %b\n" (AllU.concat xs) (AllU.concat xs') (AllU.concat xs'');
  Printf.printf "any: %b %b %b\n" (AnyU.concat xs) (AnyU.concat xs') (AnyU.concat xs'')

let plusFive = (+) 5
let plusTen  = (+) 10
let plusFifteen = compose plusFive plusTen
let map_composition_of_functions fmap = fmap plusFifteen
let compose_mapped_functions fmap = compose (fmap plusFive) (fmap plusTen)

let examples4 () =
  let open ListF in
  let test_id x = (fmap id x = x) in (* Functor law 1 *)
  let map_composition_of_functions = fmap plusFifteen [5] in
  let compose_mapped_functions     = compose (fmap plusFive) (fmap plusTen) [5] in
  let test_compose = map_composition_of_functions = compose_mapped_functions in (* Functor law 2 *)
  Printf.printf "[List] Functor Law 1: %b\n" (test_id [1; 2; 3]);
  Printf.printf "[List] Functor Law 2: %b\n" (test_compose)

let examples5 () =
  let open OptionF in
  let s = Some 5 in
  let a = map_composition_of_functions fmap s in
  let b = compose_mapped_functions fmap s in
  Printf.printf "[Option] Functor Law 2: %d and %d\n" Option.(get a) Option.(get b)

let () =
  examples1 ();
  examples2 ();
  examples3 ();
  examples4 ();
  examples5 ();