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