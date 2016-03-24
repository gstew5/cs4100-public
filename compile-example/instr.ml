open Exp
open Gensym

type iexp =
  | IId of id
  | IInt of int
  | IFloat of float
  | IBool of bool
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
  | IBool true -> print_string "true"
  | IBool false -> print_string "false"  
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

let rec print_instr_list (is : instr list) : unit =
  match is with
  | [] -> ()
  | i :: is' ->
     print_instr i;
     print_string "\n";
     print_instr_list is'
	     
