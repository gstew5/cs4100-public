%{
  open Exp
%}

%token EOF
%token <int> INTCONST
%token <float> FLOATCONST
%token <string> ID
%token TRUE
%token FALSE
%token NOT
%token PLUS MINUS TIMES DIV
%token AND
%token LT INTEQ
%token LPAREN RPAREN
%token LET IN EQ       

%nonassoc IN
%left AND	  
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
| AND
  { BAnd }  
| LT
  { BLt }
| INTEQ
  { BIntEq }

id:
| i = ID
  { i }
  
exp:
| LPAREN e = exp RPAREN
  { e }
| n = INTCONST
  { EInt n }
| f = FLOATCONST
  { EFloat f }
| TRUE
  { EBool true }
| FALSE
  { EBool false }
| u = unop e1 = exp %prec unary_over_binary
  { EUnop(u, e1) }
| e1 = exp b = binop e2 = exp
  { EBinop(b, e1, e2) }
| x = id
  { EVar x }	
| LET x = id EQ e1 = exp IN e2 = exp
  { ELet(x, e1, e2) }				   
  
prog:
| e = exp EOF
  { e }



	

