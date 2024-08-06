(* Define a module signature for Semigroup *)
module type SEMIGROUP = sig
  type t
  val combine : t -> t -> t
end

(* Create a semigroup module for integers under addition *)
module IntAdditionSemigroup : SEMIGROUP with type t = int = struct
  type t = int
  let combine x y = x + y
end

(* Create a semigroup module for strings under concatenation *)
module StringConcatSemigroup : SEMIGROUP with type t = string = struct
  type t = string
  let combine x y = x ^ y
end

(* Define a generic function that uses a Semigroup *)
let combine_all (type a) (module S : SEMIGROUP with type t = a) (list : a list) : a =
  List.fold_left S.combine (List.hd list) (List.tl list)

(* Usage examples *)
let int_list = [1; 2; 3; 4; 5]
let string_list = ["Hello"; " "; "World"; "!"]

let () =
  Printf.printf "Int result: %d\n" (combine_all (module IntAdditionSemigroup) int_list);
  Printf.printf "String result: %s\n" (combine_all (module StringConcatSemigroup) string_list)

(* Define a custom type *)
type pair = { x : int; y : int }

(* Create a semigroup module for Pair *)
module PairSemigroup : SEMIGROUP with type t = pair = struct
  type t = pair
  let combine p1 p2 = { x = p1.x + p2.x; y = p1.y + p2.y }
end

let pair_list = [{ x = 1; y = 2 }; { x = 3; y = 4 }; { x = 5; y = 6 }]

let () =
  let result = combine_all (module PairSemigroup) pair_list in
  Printf.printf "Pair result: { x = %d; y = %d }\n" result.x result.y