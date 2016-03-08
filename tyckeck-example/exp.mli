
type id = string
	    
type unop = UNot

type binop =
  | BPlus | BMinus | BTimes | BDiv (* arithmetic operators *)
  | BAnd                           (* boolean operators *)				
  | BLt | BIntEq                   (* comparisons *)

type value =
  | VInt of int
  | VFloat of float
  | VBool of bool
	       
type exp =
  | EInt of int
  | EFloat of float
  | EBool of bool
  | EUnop of unop * exp
  | EBinop of binop * exp * exp
  | EVar of id
  | ELet of id * exp * exp
