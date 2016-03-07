{
  open Lexing
  open Parser
  open Printf

  exception Eof
  exception Syntax_err of string
	    
  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = pos.pos_cnum;
                 pos_lnum = pos.pos_lnum + 1;
      }
}

let newline = '\r' | '\n' | "\r\n"

rule token = parse
    [' ' '\t']                  { token lexbuf }
  | newline                     { next_line lexbuf; token lexbuf }
  | ['0'-'9']+ as lxm           { INTCONST(int_of_string lxm) }
  | ['0'-'9']+'.'['0'-'9']* as lxm{ FLOATCONST(float_of_string lxm) }	
  | '+'        { PLUS }
  | '-'        { MINUS }
  | '*'        { TIMES }
  | '/'        { DIV }
  | '('        { LPAREN }
  | ')'        { RPAREN }    
  | eof        { EOF }
  | _          { raise (Syntax_err ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }

