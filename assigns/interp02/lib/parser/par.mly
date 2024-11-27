%{
open Utils

let rec mk_app e = function
  | [] -> e
  | x :: es -> mk_app (SApp (e, x)) es
%}

/* token declarations */
%token <int> NUM
%token <string> VAR
%token IF THEN ELSE
%token LET IN
%token FUN REC ARROW
%token ADD SUB MUL DIV MOD
%token LT LTE GT GTE EQ NEQ AND OR
%token UNIT TRUE FALSE
%token LPAREN RPAREN
%token ASSERT COLON INT BOOL
%token EOF

%right OR
%right AND
%left LT LTE GT GTE EQ NEQ
%left ADD SUB
%left MUL DIV MOD
%right ARROW

%start <prog> prog

%%

prog:
  | list(toplet) EOF { $1 }

toplet:
  | LET x = VAR args = list(arg) COLON ty = ty EQ e = expr {
      let full_ty = List.fold_right (fun (_, arg_ty) acc_ty -> FunTy (arg_ty, acc_ty)) args ty in
      { is_rec = false; name = x; args = args; ty = full_ty; value = e }
    }
  | LET REC x = VAR args = list(arg) COLON ty = ty EQ e = expr {
      let full_ty = List.fold_right (fun (_, arg_ty) acc_ty -> FunTy (arg_ty, acc_ty)) args ty in
      { is_rec = true; name = x; args = args; ty = full_ty; value = e }
    }

/* Argument for functions */
arg:
  | LPAREN x = VAR COLON ty = ty RPAREN { (x, ty) }
  | x = VAR { (x, BoolTy) }

/* Type definitions */
ty:
  | INT { IntTy }
  | BOOL { BoolTy }
  | UNIT { UnitTy }
  | t1 = ty ARROW t2 = ty { FunTy (t1, t2) }
  | LPAREN t = ty RPAREN { t }

/* Expressions */
expr:
  | LET x = VAR args = list(arg) COLON ty = ty EQ e1 = expr IN e2 = expr {

      let full_ty = List.fold_right (fun (_, arg_ty) acc_ty -> FunTy (arg_ty, acc_ty)) args ty in
      SLet { is_rec = false; name = x; args = args; ty = full_ty; value = e1; body = e2 }
    }
  | LET REC f = VAR args = list(arg) COLON ty = ty EQ e1 = expr IN e2 = expr {
      let full_ty = List.fold_right (fun (_, arg_ty) acc_ty -> FunTy (arg_ty, acc_ty)) args ty in
      SLet { is_rec = true; name = f; args = args; ty = full_ty; value = e1; body = e2 }
    }

  /* Conditional Expression */
  | IF e1 = expr THEN e2 = expr ELSE e3 = expr { SIf (e1, e2, e3) }

  /* Function definition with multiple arguments */
  | FUN; arg = arg; args = list(arg); ARROW; e = expr
    { SFun{ arg = arg; args = args; body = e } }
  | FUN; x = VAR; ARROW; e = expr
    { SFun{ arg = (x, BoolTy); args = []; body = e } }
  | e = expr2 { e }

expr2:
  | e1 = expr2 op = bop e2 = expr2 { SBop(op, e1, e2) }
  | ASSERT e = expr3 { SAssert e }
  | e = expr3 es = expr3* { mk_app e es }

expr3:
  | UNIT { SUnit }
  | TRUE { STrue }
  | FALSE { SFalse }
  | n = NUM { SNum n }
  | x = VAR { SVar x }
  | LPAREN e = expr RPAREN { e }

%inline bop:
  | ADD { Add }
  | SUB { Sub }
  | MUL { Mul }
  | DIV { Div }
  | MOD { Mod }
  | LT { Lt }
  | LTE { Lte }
  | GT { Gt }
  | GTE { Gte }
  | EQ { Eq }
  | NEQ { Neq }
  | AND { And }
  | OR { Or }
