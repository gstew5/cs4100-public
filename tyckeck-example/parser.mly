%{
  open Exp
%}

%token EOF
%token <int> INTCONST
%token <float> FLOATCONST	       
%token NOT
%token PLUS MINUS TIMES DIV
%token LT INTEQ
%token LPAREN RPAREN 

%nonassoc LT INTEQ       
%left PLUS MINUS
%left TIMES DIV
%nonassoc unary_over_binary      

%start <Exp.exp> prog

%%

%inline unop:
| NOT
  { UNot }

%inline binop:
| PLUS
  { BPlus }
| MINUS
  { BMinus }
| TIMES
  { BTimes }
| DIV
  { BDiv }
| LT
  { BLt }
| INTEQ
  { BIntEq }
  
exp:
| LPAREN e = exp RPAREN
  { print_string "E -> ( E )\n"; e }
| n = INTCONST
  { BatPrintf.printf "E -> INTCONST(%d)\n" n; EInt n }
| f = FLOATCONST
  { BatPrintf.printf "E -> FLOATCONST(%f)\n" f; EFloat f }
| u = unop e1 = exp %prec unary_over_binary
  { print_string "E -> unop E\n"; EUnop(u, e1) }
| e1 = exp b = binop e2 = exp
  { print_string "E -> E binop E\n"; EBinop(b, e1, e2) }

prog:
| e = exp EOF
  { print_string "PROG -> E\n"; e }



	

