open Batteries
open BatInt32       

exception Division_by_zero

type binop = BPlus | BMinus | BTimes | BDiv

type exp =
  | EInt of Int32.t
  | EBinop of binop * exp * exp

let rec eval (e : exp) : Int32.t =
  match e with
  | EInt n -> n
  | EBinop(b, e1, e2) ->
     let n1 = eval e1 in
     let n2 = eval e2 in
     (match b with
      | BPlus -> add n1 n2
      | BMinus -> sub n1 n2
      | BTimes -> mul n1 n2
      | BDiv -> if n2 = zero then raise Division_by_zero
		else div n1 n2)
			      
let print_result (e : exp) : unit =
  BatPrintf.printf "result = %d\n" (to_int (eval e))
