open Exp

(** The [Gensym] module defines a stateful fresh symbol generator. *)

(** The abstract type of [Gensym] packages *)
type gensym_pkg

(** Initialize a [Gensym] package *)
val create_gensym_pkg : unit -> gensym_pkg

(** Given a [Gensym] package, generate a fresh [id] prefixed with a
    user-supplied string prefix *)
val gensym : string -> gensym_pkg -> id				  
