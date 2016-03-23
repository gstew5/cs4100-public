open Exp

type iexp =
  | IId of id
  | IInt of int
  | IFloat of float
  | IUnop of unop * id
  | IBinop of binop * id * id
       
type instr =
  | IAssign of id * iexp
  | IRet of id

val compile_and_print_result : exp -> unit
	      
