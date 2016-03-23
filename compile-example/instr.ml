open Exp
open Gensym       

type iexp =
  | IId of id
  | IInt of int
  | IFloat of float
  | IUnop of unop * id
  | IBinop of binop * id * id

let rec print_iexp (i : iexp) =
  let print_binop (b : binop) : unit =
    match b with
    | BPlus -> print_string " + "
    | BMinus -> print_string " - "
    | BTimes -> print_string " * "
    | BDiv -> print_string " / "
    | BAnd -> print_string " && "
    | BLt -> print_string " < "
    | BIntEq -> print_string " == "
  in 
  match i with
  | IId x -> print_string x
  | IInt n -> print_int n
  | IFloat f -> print_float f
  | IUnop(UNot, x) ->
     print_string "not ";
     print_string x
  | IBinop(b, x1, x2) ->
     print_string x1;
     print_binop b;
     print_string x2
       
type instr =
  | IAssign of id * iexp
  | IRet of id

let print_instr (i : instr) =
  match i with
  | IAssign(x, ie) ->
     print_string x;
     print_string " <- ";
     print_iexp ie
  | IRet x ->
     print_string "ret ";
     print_string x

let rec instrs_of_exp (p : gensym_pkg) (out : id) (e : exp) : instr list =
  match e with
  | EInt n -> [IAssign(out, IInt n)]
  | EFloat f -> [IAssign(out, IFloat f)]
  | EBool true -> [IAssign(out, IInt 1)]
  | EBool false -> [IAssign(out, IInt 0)]
  | EVar x -> [IAssign(out, IId x)]
  | EUnop (u, e1) ->
     let e1_out = gensym "_unop" p in
     let is1 = instrs_of_exp p e1_out e in
     is1 @ [IAssign(out, IUnop(u, e1_out))]
  | EBinop (b, e1, e2) ->
     let e1_out = gensym "_binop1" p in
     let e2_out = gensym "_binop2" p in
     let is1 = instrs_of_exp p e1_out e1 in
     let is2 = instrs_of_exp p e2_out e2 in
     is1 @ is2 @ [IAssign(out, IBinop(b, e1_out, e2_out))]
  | ELet (x, e1, e2) ->
     let e1_out = gensym ("_" ^ x) p in
     let is1 = instrs_of_exp p e1_out e1 in
     let is2 = instrs_of_exp p out (subst_var e1_out x e2) in
     is1 @ is2

let rec print_instr_list (is : instr list) : unit =
  match is with
  | [] -> ()
  | i :: is' ->
     print_instr i;
     print_string "\n";
     print_instr_list is'
	     
let compile_exp (e : exp) : instr list =
  let p = create_gensym_pkg () in
  let out = gensym "_out" p in
  let is = instrs_of_exp p out e in
  is @ [IRet out]
	     
let compile_and_print_result (e : exp) : unit =
  let is = compile_exp e in
  print_instr_list is
