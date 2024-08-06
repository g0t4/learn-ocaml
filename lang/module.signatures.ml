(* Define a module signature (like an interface) *)
module type PRINTABLE = sig
  type t
  val to_string : t -> string
  val print : t -> unit
end

(* Implement a module that satisfies the PRINTABLE signature *)
module IntPrintable : PRINTABLE with type t = int = struct
  type t = int
  let to_string x = string_of_int x
  let print x = print_endline (to_string x)
end

(* Another implementation of PRINTABLE *)
module StringPrintable : PRINTABLE with type t = string = struct
  type t = string
  let to_string x = x
  let print x = print_endline x
end

(* A function that works with any PRINTABLE module *)
let print_twice (type a) (module P : PRINTABLE with type t = a) (x : a) =
  P.print x;
  P.print x

(* Usage *)
let () =
  print_twice (module IntPrintable) 42;
  print_twice (module StringPrintable) "Hello, OCaml!"
