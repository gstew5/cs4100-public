(** The [Symtab] module defines a symbol table data structure. *)

(** The abstract type of symbol tables mapping identifiers of 
    type [string] to values of type ['a]. *)
type 'a t

(** Create an empty symbol table. *)
val create : unit -> 'a t

(** Get the value associated with an [id], if one exists. *)			
val get : string -> 'a t -> 'a option

(** Set the value associated with an [id]. *)
val set : string -> 'a -> 'a t -> 'a t

