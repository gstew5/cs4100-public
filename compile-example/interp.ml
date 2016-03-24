open Batteries

open Errors
open Exp
open Tycheck
open Instr              

type value =
  | VInt of int
  | VFloat of float
  | VBool of bool

let binop_eval (b : binop) (v1 : value) (v2 : value) : value =
  match (b, v1, v2) with
  | (BPlus, VInt n1, VInt n2) -> VInt (n1 + n2)
  | (BPlus, VFloat f1, VFloat f2) -> VFloat (f1 +. f2)
  | (BPlus, _, _) -> raise Dynamic_type_error

  | (BMinus, VInt n1, VInt n2) -> VInt (n1 - n2)
  | (BMinus, VFloat f1, VFloat f2) -> VFloat (f1 -. f2)
  | (BMinus, _, _) -> raise Dynamic_type_error

  | (BTimes, VInt n1, VInt n2) -> VInt (n1 * n2)
  | (BTimes, VFloat f1, VFloat f2) -> VFloat (f1 *. f2)
  | (BTimes, _, _) -> raise Dynamic_type_error

  | (BDiv, VInt n1, VInt n2) ->
     if n2 = 0 then raise Division_by_zero
     else VInt (n1 / n2)
  | (BDiv, VFloat f1, VFloat f2) -> VFloat (f1 /. f2)
  | (BDiv, _, _) -> raise Dynamic_type_error

  | (BAnd, VBool b1, VBool b2) -> VBool (b1 && b2)
  | (BAnd, _, _) -> raise Dynamic_type_error

  | (BLt, VInt n1, VInt n2) -> VBool (n1 < n2)
  | (BLt, VFloat f1, VFloat f2) -> VBool (f1 < f2)
  | (BLt, _, _) -> raise Dynamic_type_error

  | (BIntEq, VInt n1, VInt n2) -> VBool (n1 = n2)
  | (BIntEq, _, _) -> raise Dynamic_type_error

let lookup_id (rho : value Symtab.t) (x : id) : value =
  match Symtab.get x rho with
  | None -> raise (Dynamic_scope_error x)
  | Some v -> v
			    
let rec eval (rho : value Symtab.t) (e : exp) : value =
  match e with
  | EInt n -> VInt n
  | EFloat f -> VFloat f
  | EBool b -> VBool b
  | EUnop(u, e1) ->
     let v1 = eval rho e1 in
     (match u with
      | UNot ->
	 (match v1 with
	  | VBool b -> VBool (not b)
	  | _ -> raise Dynamic_type_error))
  | EBinop(b, e1, e2) ->
     let v1 = eval rho e1 in
     let v2 = eval rho e2 in
     binop_eval b v1 v2
  | EVar x -> lookup_id rho x
  | ELet(x, e1, e2) ->
     let v1 = eval rho e1 in
     eval (Symtab.set x v1 rho) e2
			    
let tycheck_and_print_result (e : exp) : unit =
  let t = tycheck (Symtab.create ()) e in			    
  match eval (Symtab.create ()) e with
  | VInt n -> BatPrintf.printf "result = %d : %s\n" n (string_of_ty t)
  | VFloat f -> BatPrintf.printf "result = %f : %s\n" f (string_of_ty t)
  | VBool b -> if b then BatPrintf.printf "result = true : %s\n" (string_of_ty t)
	       else BatPrintf.printf "result = false : %s\n" (string_of_ty t)

let print_value (v : value) : unit =				     
  match v with 
  | VInt n -> BatPrintf.printf "result = %d\n" n
  | VFloat f -> BatPrintf.printf "result = %f\n" f 
  | VBool b -> if b then BatPrintf.printf "result = true\n" 
	       else BatPrintf.printf "result = false\n" 

let print_result (e : exp) : unit =
  print_value (eval (Symtab.create ()) e)

type ('a, 'b) either =
  | Left of 'a
  | Right of 'b
				     
let instrs_print_result (is : instr list) : unit =
  let eval (rho : value Symtab.t) (ie : iexp) : value =
    match ie with
    | IId x -> lookup_id rho x
    | IInt n -> VInt n
    | IFloat f -> VFloat f
    | IBool b -> VBool b
    | IUnop(u, x) -> 
       let v1 = lookup_id rho x in
       (match u with
	| UNot ->
	   (match v1 with
	    | VBool b -> VBool (not b)
	    | _ -> raise Dynamic_type_error))
    | IBinop(b, x1, x2) -> 
       let v1 = lookup_id rho x1 in
       let v2 = lookup_id rho x2 in
       binop_eval b v1 v2
  in 
  let interp_instr
	(rho : value Symtab.t)
	(i : instr)
      : (value Symtab.t, value) either =
    match i with
    | IAssign (x, ie) ->
       let v = eval rho ie in
       Left (Symtab.set x v rho)
    | IRet x ->
       Right (lookup_id rho x)
  in
  let rec interp_instrs
	    (rho : value Symtab.t)
	    (is : instr list)
	  : value =
    match is with
    | [] -> raise (Codegen_error "End of instruction stream")
    | i :: is' ->
       (match interp_instr rho i with
	| Left rho' -> interp_instrs rho' is'
	| Right v -> v)
  in
  print_value (interp_instrs (Symtab.create ()) is)
