open Exp
       
type ty =
  | TInt
  | TFloat
  | TBool

val string_of_ty : ty -> string
      
(** Type-check [e], returning its type. 
    @@Raises [Static_ype_error] if [e] is not well-typed. *)      
val tycheck : ty Symtab.t -> exp -> ty
