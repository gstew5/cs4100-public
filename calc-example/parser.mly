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
  { e }
| n = INTCONST
  { EInt n }
| e1 = exp b = binop e2 = exp
  { EBinop(b, e1, e2) }

prog:
| e = exp EOF
  { e }



	

