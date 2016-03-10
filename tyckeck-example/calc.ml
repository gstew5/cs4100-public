open Batteries
open BatFormat
open BatOptParse

open Errors
open Lexing
open Exp
open Tycheck       
open Interp       
			    
let print_pos outchan lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outchan "%s:%d:%d" pos.pos_fname 
	  pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)  
		    
let parse_with_err lexbuf =
  try Parser.prog Lexer.token lexbuf with
  | Parser.Error -> 
    eprintf "%a: syntax error\n" print_pos lexbuf;
    exit (-1)
      
  | Lexer.Syntax_err msg -> 
    eprintf "%a: %s\n" print_pos lexbuf msg;
    exit (-1)

let () =
  let opt_parser = OptParser.make () in
  let infile_opt = StdOpt.str_option ~metavar:"filename" () in
  let tycheck_opt = StdOpt.str_option ~metavar:"" () in
  OptParser.add opt_parser ~short_name:'i' ~long_name:"infile" infile_opt;
  OptParser.add opt_parser ~short_name:'t' ~long_name:"tycheck" tycheck_opt;
  let _ = OptParser.parse_argv opt_parser in
  try
    let infile =
      try Opt.get infile_opt
      with Opt.No_value -> ""
    in
    let inbuf  = if infile = "" then stdin else open_in infile in
    let lexbuf = Lexing.from_channel inbuf in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = infile };
    let p = parse_with_err lexbuf in
    if Opt.is_set tycheck_opt then tycheck_and_print_result p
    else print_result p
  with
  | Sys_error err -> eprintf "System error: %s\n" err
  | Failure err -> eprintf "Error: %s\n" err
  | Division_by_zero -> eprintf "Division by zero!\n"
  | Dynamic_type_error -> eprintf "!!! AT RUNTIME: Type error detected !!!\n"
  | Static_type_error -> eprintf "Compile-time: type error detected\n"
  | Dynamic_scope_error x -> eprintf "!!! AT RUNTIME: Scope error !!!\nVariable %s is unbound.\n" x
  | Static_scope_error x -> eprintf "Compile-time: Scope error\nVariable %s is unbound.\n" x
