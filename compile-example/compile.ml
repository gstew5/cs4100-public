open Gensym
open Exp
open Instr
open Interp       

let rec instrs_of_exp (p : gensym_pkg) (out : id) (e : exp) : instr list =
  match e with
  | EInt n -> [IAssign(out, IInt n)]
  | EFloat f -> [IAssign(out, IFloat f)]
  | EBool b -> [IAssign(out, IBool b)]
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
	     
let compile_exp (e : exp) : instr list =
  let p = create_gensym_pkg () in
  let out = gensym "_out" p in
  let is = instrs_of_exp p out e in
  is @ [IRet out]
	     
let compile_and_print_result (e : exp) : unit =
  let is = compile_exp e in
  print_string "\nCOMPILED\n";
  print_instr_list is;
  print_string "\n";
  instrs_print_result is
