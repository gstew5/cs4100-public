
type id = string
	    
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
  | EVar of id
  | ELet of id * exp * exp

val subst_var : id -> id -> exp -> exp
