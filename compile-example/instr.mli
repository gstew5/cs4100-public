open Exp

type iexp =
  | IId of id
  | IInt of int
  | IFloat of float
  | IBool of bool		
  | IUnop of unop * id
  | IBinop of binop * id * id
       
type instr =
  | IAssign of id * iexp
  | IRet of id

val print_instr_list : instr list -> unit 
	      
