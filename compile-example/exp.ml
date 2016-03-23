
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

let rec subst_var (new_id : id) (old_id : id) (e : exp) : exp =
  match e with
  | EInt _ -> e
  | EFloat _ -> e
  | EBool _ -> e
  | EUnop (u, e1) -> EUnop (u, subst_var new_id old_id e1)
  | EBinop (b, e1, e2) ->
     EBinop (b, subst_var new_id old_id e1, subst_var new_id old_id e2)
  | EVar x ->
     if x = old_id then EVar new_id
     else e
  | ELet (x, e1, e2) ->
     let e1' = subst_var new_id old_id e1 in
     let e2' = if x = old_id then e2 else subst_var new_id old_id e2 in
     ELet (x, e1', e2')
	     
