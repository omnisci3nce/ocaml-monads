(** Monoids *)

let id x = x
let flip f x y = f y x
let compose f g x = f (g x)
let cons x xs = x :: xs

(*
  A monoid is an algebraic structure closed under an associative operation
  associative property: (2 x 3) x 4  =  2 x (3 x 4)
  (often denoted mappend) and with a neutral element (often denoted mempty).
*)

module type Monoid = sig
  (** Monoid * *)
  type t

  (** Neutral element *)
  val empty : t

  (** Associative operation - composes two elements *)
  val append : t -> t -> t
end

module Monoid_Utils (M: Monoid) = struct
  open M
  
  let (<+>) x y = append x y                          (* Shorthand for append *)
  let concat xs = List.fold_left (<+>) M.empty xs     (* Any monoid can be concatenated *)
end

module Sum = struct
  type t = int
  let empty = 0
  let append = (+)
end

module Prod = struct
  type t = int
  let empty = 1
  let append = ( * )
end

module StringM = struct
  type t = string
  let empty = ""
  let append s1 s2 = s1 ^ s2
end

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

(* Boolean AND is associative *)
module All = struct
  type t = bool
  let empty = true
  let append = (&&)
end

(* Boolean OR is associative *)
module Any = struct
  type t = bool
  let empty = true
  let append = (||)
end

let examples3 () =
  let xs   = [true; false; false; true] in
  let xs'  = [true; true] in
  let xs'' = [false; false] in
  let module AllU = Monoid_Utils(All) in
  let module AnyU = Monoid_Utils(Any) in
  Printf.printf "all: %b %b %b\n" (AllU.concat xs) (AllU.concat xs') (AllU.concat xs'');
  Printf.printf "any: %b %b %b\n" (AnyU.concat xs) (AnyU.concat xs') (AnyU.concat xs'')

(* --------------------------------- *)
(** Functors *)
(* *)

module type Functor = sig
  type 'a t

  val fmap : ('a -> 'b) -> 'a t -> 'b t
end

(** Functor Laws *)
(*  1) fmap id = id *)
(*  2) mapping a composition of functions is
   * equivalent to composing the mapped functions *)

(* Functor signature for Lists *)
module ListF : (Functor with type 'a t = 'a list) = struct
  type 'a t = 'a list
  let fmap f = List.map f
end

let examples4 () =
  let open ListF in
  let test_id x = (fmap id x = x) in (* Functor law 1 *)
  let plusFive = (+) 5 in
  let plusTen  = (+) 10 in
  let plusFifteen = compose plusFive plusTen in
  let map_composition_of_functions = fmap plusFifteen [5] in
  let compose_mapped_functions     = compose (fmap plusFive) (fmap plusTen) [5] in
  let test_compose = map_composition_of_functions = compose_mapped_functions in (* Functor law 2 *)
  Printf.printf "[List] Functor Law 1: %b\n" (test_id [1; 2; 3]);
  Printf.printf "[List] Functor Law 2: %b\n" (test_compose)


(* --------------------------------- *)
(** Monads *)

module type Monad = sig
  type 'a t

  (* Lift a value into the Monad *)
  val return : 'a -> 'a t  
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end


module Maybe : Monad = struct
  type 'a t = 'a option (* Wrap option type in monadic interface *)

  let return x = Some x
  let bind m f = 
    match m with
    | Some x -> f x
    | None -> None
end

(* module Writer : Monad *)
(* module Promise : Monad *)

let () =
  examples1 ();
  examples2 ();
  examples3 ();
  examples4 ();