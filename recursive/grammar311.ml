(** An implementation of a recursive descent parser for 
    [Appel Grammar 3.11] 

    To build, type 

      ocamlbuild -package batteries grammar311.byte
*)

type tok = IF | THEN | ELSE | BEGIN | END | PRINT | SEMI | NUM of int | EQ

exception Parse_failure of tok option * string

(** We use a reference (a mutable memory cell) to record the current 
    list of yet-to-be-parsed tokens; [try_parse] populates this list. *)
let tokList : (tok list) ref = ref []
									  
let getToken (_ : unit) : tok =
  match !tokList with
  | [] -> raise (Parse_failure (None, "ran out of tokens"))
  | x :: _ -> x

let advance (_ : unit) : unit =
  tokList := BatList.tl !tokList

let eat_num (_ : unit) : unit =
  let tok = getToken () in
  match tok with
  | NUM _ -> advance ()
  | _ -> raise (Parse_failure (Some tok, "not a NUM"))
		  
let eat (t : tok) : unit =
  let tok = getToken () in
  if tok = t then advance ()
  else raise (Parse_failure (Some tok, "unexpected token"))

let rec s (_ : unit) : unit =
  let tok = getToken () in
  match tok with
  | IF -> eat IF; e (); eat THEN; s (); eat ELSE; s ()
  | BEGIN -> eat BEGIN; s (); l ()
  | PRINT -> eat PRINT; e ()
  | _ -> raise (Parse_failure (Some tok, "in nonterminal S"))

and l (_ : unit) : unit =
  let tok = getToken () in 
  match tok with
  | END -> eat END
  | SEMI -> eat SEMI; s (); l ()
  | _ -> raise (Parse_failure (Some tok, "in nonterminal L"))
			  
and e (_ : unit) : unit =
  let tok = getToken () in
  match tok with
  | NUM _ -> eat_num (); eat EQ; eat_num ()
  | _ -> raise (Parse_failure (Some tok, "in nonterminal E"))

let try_parse (l : tok list) : (tok option * string) =
  tokList := l;
  try s (); (None, "success") with Parse_failure (otok, err) -> (otok, err)
;;

let test1 =
  try_parse [IF; NUM 4; EQ; NUM 5;
	     THEN; PRINT; NUM 7; EQ; NUM 8;
	     ELSE; PRINT; NUM 9; EQ; NUM 10];;
  (* val test1 : tok option * string = (None, u"success") *)

let test2 =
  try_parse [IF; NUM 4; EQ; NUM 5;
	     THEN; BEGIN; PRINT; NUM 7; EQ; NUM 8; END;
	     ELSE; PRINT; NUM 9; EQ; NUM 10];;
  (* val test2 : tok option * string = (None, u"success") *)

let test3 =
  try_parse [IF; NUM 4; EQ; NUM 5;
	     THEN; BEGIN; PRINT; NUM 7; EQ; NUM 8; 
	     ELSE; PRINT; NUM 9; EQ; NUM 10];;
  (* val test3 : tok option * string = (Some ELSE, u"in nonterminal L") *)
  
  
								 
									
									

