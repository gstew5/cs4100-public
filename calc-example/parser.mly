%{
  open Exp
%}

%token EOF
%token <int32> INTCONST
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN 

%left PLUS MINUS
%left TIMES DIV

%start <Exp.exp> prog

%%

%inline binop:
| PLUS
  { BPlus }
| MINUS
  { BMinus }
| TIMES
  { BTimes }
| DIV
  { BDiv }
  
exp:
| LPAREN e = exp RPAREN
  { print_string "E -> ( E )\n"; e }
| n = INTCONST
  { BatPrintf.printf "E -> INTCONST(%d)\n" (Int32.to_int n); EInt n }
| e1 = exp b = binop e2 = exp
  { print_string "E -> E binop E\n"; EBinop(b, e1, e2) }

prog:
| e = exp EOF
  { print_string "PROG -> E\n"; e }



	

