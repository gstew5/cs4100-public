exception Division_by_zero
exception Type_error	    

type unop = UNot
	    
type binop =
  | BPlus | BMinus | BTimes | BDiv (* arithmetic operators *)
  | BAnd                           (* boolean operators *)				
  | BLt | BIntEq                   (* comparisons *)

type exp =
  | EInt of int
  | EFloat of float
  | EBool of bool
  | EUnop of unop * exp
  | EBinop of binop * exp * exp

val print_result : exp -> unit
