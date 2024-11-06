%{
open Utils
(*
(* counter for vars*)
let counter = ref 0

(*generate unique var names *)
let gensym prefix =
  let n = !counter in
  counter := n + 1;
  prefix ^ string_of_int n
%}
*)
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
%token REC  

%right OR
%right AND
%left LT LTE GT GTE EQUALS NEQ
%left PLUS MINUS
%left TIMES DIV MOD

%start <Utils.prog> prog
%%

prog:
  | e = expr EOF { e }
  ;

expr:
  | IF e1 = expr THEN e2 = expr ELSE e3 = expr { If(e1, e2, e3) }
  | LET REC x = VAR EQUALS e1 = expr IN e2 = expr { 
      Let(x,
          App(
              Fun("f",
                  App(
                      Fun("x_inner", App(App(Var "f", Var "f"), Fun("v", App(App(Var "f", Var "f"), Var "v")))),
                      Fun("x_inner", e1)
                  )
              ),
              Fun(x, e1)
          ),
          e2
      )
  }
  | LET x = VAR EQUALS e1 = expr IN e2 = expr { Let(x, e1, e2) }
  | FUN x = VAR ARROW e = expr { Fun(x, e) }
  | e = app_expr { e }
  ;


app_expr:
  | e = op_expr { e }
  | e1 = app_expr e2 = simple_expr { App(e1, e2) }
  ;

op_expr:
  | e = simple_expr { e }
  | e1 = op_expr op = bop e2 = op_expr { Bop(op, e1, e2) }
  ;

simple_expr:
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