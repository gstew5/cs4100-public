open Errors
open Exp
       
type ty =
  | TInt
  | TFloat
  | TBool

let string_of_ty (t : ty) : string =
  match t with
  | TInt -> "int"
  | TFloat -> "float"
  | TBool -> "bool"
      
let is_arith_ty (t : ty) : bool =
  match t with
  | (TInt | TFloat) -> true
  | TBool -> false

let rec tycheck (gamma : ty Symtab.t) (e : exp) : ty =
  match e with
  | EInt _ -> TInt
  | EFloat _ -> TFloat
  | EBool _ -> TBool
  | EUnop(u, e1) ->
     let t1 = tycheck gamma e1 in
     (match (u, t1) with
      | (UNot, TBool) -> TBool
      | (UNot, _) -> raise Static_type_error)
  | EBinop(b, e1, e2) ->
     let t1 = tycheck gamma e1 in
     let t2 = tycheck gamma e2 in
     tycheck_binop b t1 t2
  | EVar x ->
     (match Symtab.get x gamma with
      | None -> raise (Static_scope_error x)
      | Some t -> t)
  | ELet(x, e1, e2) ->
     let t1 = tycheck gamma e1 in
     tycheck (Symtab.set x t1 gamma) e2

and tycheck_binop (b : binop) (t1 : ty) (t2 : ty) : ty =
  match b with
  | (BPlus | BMinus | BTimes | BDiv) ->
     if t1 = t2 && is_arith_ty t1 then t1
     else raise Static_type_error
       
  | BAnd ->
     if t1 = t2 && t1 = TBool then t1
     else raise Static_type_error

  | BLt ->
     if t1 = t2 && is_arith_ty t1 then TBool
     else raise Static_type_error

  | BIntEq ->
     if t1 = t2 && t1 = TInt then TBool
     else raise Static_type_error
     

