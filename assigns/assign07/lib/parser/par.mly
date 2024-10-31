%{
open Utils
%}

%token <int> NUM
%token <string> VAR
%token LPAREN RPAREN
%token IF THEN ELSE
%token LET EQUALS IN
%token FUN ARROW
%token PLUS MINUS TIMES DIV MOD
%token LT LTE GT GTE NEQ AND OR
%token UNIT TRUE FALSE
%token EOF
%right OR
%right AND
%left LT LTE GT GTE EQUALS NEQ
%left PLUS MINUS
%left TIMES DIV MOD
%nonassoc APP

%start <Utils.prog> prog
%%

prog:
  | e = expr EOF { e }
  ;

(*if-then-else, let, fun def for parser *)
expr:
  | e = expr2 {e}
  | IF e1 = expr THEN e2 = expr ELSE e3 = expr {If(e1, e2, e3)}
  | LET x = VAR EQUALS e1 = expr IN e2 = expr {Let(x, e1, e2)}
  | FUN x = VAR ARROW e = expr {Fun(x, e)}
  ;

(* binary ops (BOP) *)
expr2:
  | e = expr3 { e }
  | e1 = expr2 e2 = expr3 {App(e1, e2)} %prec APP
  | e1 = expr2 op = bop e2 = expr2 { Bop(op, e1, e2)}
  ;

(* Atomic exps *)
expr3:
  | UNIT { Unit }
  | TRUE { True }
  | FALSE { False }
  | n = NUM { Num n }
  | x = VAR { Var x }
  | LPAREN e = expr RPAREN { e }
  ;

%inline bop:
  | PLUS   { Add }
  | MINUS  { Sub }
  | TIMES  { Mul }
  | DIV    { Div }
  | MOD    { Mod }
  | LT     { Lt }
  | LTE    { Lte }
  | GT     { Gt }
  | GTE    { Gte }
  | EQUALS { Eq }
  | NEQ    { Neq }
  | AND    { And }
  | OR     { Or }
  ;