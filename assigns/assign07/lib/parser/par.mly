%{
open Utils
%}

%token <int> NUM
%token <string> VAR
%token EOF
%token LPAREN RPAREN PLUS MINUS TIMES DIVIDED MOD
%token LT LE GT GE EQ NEQ ANDAND OROR ARROW
%token IF THEN ELSE LET IN FUN TRUE FALSE UNIT

%start <Utils.prog> prog
%type <Utils.expr> expr

%%

prog:
    expr EOF { $1 }
;
expr:
    IF expr THEN expr ELSE expr { If ($2, $4, $6) }
  | LET VAR EQ expr IN expr    { Let ($2, $4, $6) }
  | FUN VAR ARROW expr         { Fun ($2, $4) }
  | expr_or                    { $1 }
;
expr_or:
    expr_or OROR expr_and { Bop (Or, $1, $3) }
  | expr_and { $1 }
;
expr_and:
    expr_and ANDAND expr_cmp { Bop (And, $1, $3) }
  | expr_cmp { $1 }
;
expr_cmp:
    expr_cmp EQ expr_add  { Bop (Eq, $1, $3) }
  | expr_cmp NEQ expr_add { Bop (Neq, $1, $3) }
  | expr_cmp LT expr_add  { Bop (Lt, $1, $3) }
  | expr_cmp LE expr_add  { Bop (Lte, $1, $3) }
  | expr_cmp GT expr_add  { Bop (Gt, $1, $3) }
  | expr_cmp GE expr_add  { Bop (Gte, $1, $3) }
  | expr_add { $1 }
;
expr_add:
    expr_add PLUS expr_mul   { Bop (Add, $1, $3) }
  | expr_add MINUS expr_mul  { Bop (Sub, $1, $3) }
  | expr_mul { $1 }
;
expr_mul:
    expr_mul TIMES expr_app    { Bop (Mul, $1, $3) }
  | expr_mul DIVIDED expr_app  { Bop (Div, $1, $3) }
  | expr_mul MOD expr_app      { Bop (Mod, $1, $3) }
  | expr_app { $1 }
;
expr_app:
    expr_app expr_atom { App ($1, $2) }
  | expr_atom { $1 }
;
expr_atom:
    UNIT         { Unit }
  | TRUE         { True }
  | FALSE        { False }
  | NUM          { Num $1 }
  | VAR          { Var $1 }
  | LPAREN expr RPAREN { $2 }
;
