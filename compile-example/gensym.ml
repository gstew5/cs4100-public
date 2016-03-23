open Batteries
open BatFormat

open Exp       

type gensym_pkg = int ref

let create_gensym_pkg (_ : unit) = ref 0
		      
let gensym (prefix : string) (p : gensym_pkg) : id =
  let x = !p in
  p := x + 1;
  prefix ^ sprintf "%d" x
