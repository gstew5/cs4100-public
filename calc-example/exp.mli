exception Division_by_zero

type binop = BPlus | BMinus | BTimes | BDiv

type exp =
  | EInt of Int32.t
  | EBinop of binop * exp * exp

val print_result : exp -> unit
