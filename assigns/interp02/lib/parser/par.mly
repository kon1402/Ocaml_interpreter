%{
open Utils

let rec mk_app e = function
  | [] -> e
  | x :: es -> mk_app (SApp (e, x)) es
%}

%token <int> NUM
%token <string> VAR
%token UNIT TRUE FALSE
%token LPAREN RPAREN
%token ADD SUB MUL DIV MOD
%token LT LTE GT GTE EQ NEQ
%token AND OR
%token IF THEN ELSE
%token LET IN
%token FUN ARROW
%token REC
%token COLON
%token INT BOOL UNIT_TY
%token ASSERT
%token EOF

%right OR
%right AND
%left LT LTE GT GTE EQ NEQ
%left ADD SUB
%left MUL DIV MOD
%right ARROW

%start <Utils.prog> prog

%%

prog:
  | ds = list(decl) EOF { ds }

decl:
  (* Annotated declarations *)
  | LET; x = VAR; args = list(arg); COLON; t = ty; EQ; e = expr 
    { { is_rec = false; name = x; args = args; ty = t; value = e } }
  | LET; REC; x = VAR; args = nonempty_list(arg); COLON; t = ty; EQ; e = expr
    { { is_rec = true; name = x; args = args; ty = t; value = e } }
  (* Unannotated declarations *)
  | LET; x = VAR; EQ; e = expr 
    { { is_rec = false; name = x; args = []; ty = BoolTy; value = e } }
  | LET; REC; x = VAR; EQ; e = expr
    { { is_rec = true; name = x; args = []; ty = BoolTy; value = e } }

arg:
  | LPAREN; x = VAR; COLON; t = ty; RPAREN { (x, t) }
  | x = VAR { (x, BoolTy) }

ty:
  | INT { IntTy }
  | BOOL { BoolTy }
  | UNIT_TY { UnitTy }
  | t1 = ty; ARROW; t2 = ty { FunTy(t1, t2) }
  | LPAREN; t = ty; RPAREN { t }

expr:
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr 
    { SIf(e1, e2, e3) }
  (* Let expressions *)
  | LET; x = VAR; args = list(arg); COLON; t = ty; EQ; e1 = expr; IN; e2 = expr
    { SLet{ is_rec = false; name = x; args = args; ty = t; value = e1; body = e2 } }
  | LET; x = VAR; EQ; e1 = expr; IN; e2 = expr
    { SLet{ is_rec = false; name = x; args = []; ty = BoolTy; value = e1; body = e2 } }
  | LET; REC; x = VAR; args = nonempty_list(arg); COLON; t = ty; EQ; e1 = expr; IN; e2 = expr
    { SLet{ is_rec = true; name = x; args = args; ty = t; value = e1; body = e2 } }
  | LET; REC; x = VAR; EQ; e1 = expr; IN; e2 = expr
    { SLet{ is_rec = true; name = x; args = []; ty = BoolTy; value = e1; body = e2 } }
  (* Functions *)
  | FUN; arg = arg; args = list(arg); ARROW; e = expr
    { SFun{ arg = arg; args = args; body = e } }
  | FUN; x = VAR; ARROW; e = expr
    { SFun{ arg = (x, BoolTy); args = []; body = e } }
  | e = expr2 { e }

expr2:
  | e1 = expr2; op = bop; e2 = expr2 { SBop(op, e1, e2) }
  | ASSERT; e = expr3 { SAssert e }
  | e = expr3; es = expr3* { mk_app e es }

expr3:
  | UNIT { SUnit }
  | TRUE { STrue }
  | FALSE { SFalse }
  | n = NUM { SNum n }
  | x = VAR { SVar x }
  | LPAREN; e = expr; RPAREN { e }

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