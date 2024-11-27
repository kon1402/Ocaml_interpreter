/* par.mly */

%{
open Utils


%}

/* Token declarations */
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

/* Operator precedence and associativity */
%left ADD SUB
%left MUL DIV MOD
%left LT LTE GT GTE EQ NEQ
%right AND
%right OR
%left APP  /* Function application */
%right ARROW

%start <prog> prog

%%

prog:
  | toplet_list EOF { $1 }

toplet_list:
  | /* empty */ { [] }
  | toplet_list toplet { $1 @ [$2] }

toplet:
  | LET x=VAR args=arg_list COLON ty=ty EQ e=expr {
      let full_ty = List.fold_right (fun (_, arg_ty) acc_ty -> FunTy (arg_ty, acc_ty)) args ty in
      { is_rec=false; name=x; args=args; ty=full_ty; value=e }
    }
  | LET REC x=VAR args=arg_list COLON ty=ty EQ e=expr {
      let full_ty = List.fold_right (fun (_, arg_ty) acc_ty -> FunTy (arg_ty, acc_ty)) args ty in
      { is_rec=true; name=x; args=args; ty=full_ty; value=e }
    }
  | LET x=VAR EQ e=expr {
      { is_rec=false; name=x; args=[]; ty=BoolTy; value=e }
    }
  | LET REC x=VAR EQ e=expr {
      { is_rec=true; name=x; args=[]; ty=BoolTy; value=e }
    }
  | LET REC x=VAR vars=nonempty_var_list EQ e=expr {
      let args = List.map (fun x -> (x, BoolTy)) vars in
      { is_rec=true; name=x; args=args; ty=BoolTy; value=e }
    }

arg_list:
  | /* empty */ { [] }
  | arg_list arg { $1 @ [$2] }

nonempty_var_list:
  | x=VAR { [x] }
  | nonempty_var_list x=VAR { $1 @ [x] }

arg:
  | LPAREN x=VAR COLON ty=ty RPAREN { (x, ty) }
  | x=VAR { (x, BoolTy) }

ty:
  | INT { IntTy }
  | BOOL { BoolTy }
  | UNIT { UnitTy }
  | t1=ty ARROW t2=ty { FunTy (t1, t2) }
  | LPAREN ty=ty RPAREN { ty }

expr:
  | LET x=VAR args=arg_list COLON ty=ty EQ e1=expr IN e2=expr {
      let full_ty = List.fold_right (fun (_, arg_ty) acc_ty -> FunTy (arg_ty, acc_ty)) args ty in
      SLet { is_rec=false; name=x; args=args; ty=full_ty; value=e1; body=e2 }
    }
  | LET REC x=VAR args=arg_list COLON ty=ty EQ e1=expr IN e2=expr {
      let full_ty = List.fold_right (fun (_, arg_ty) acc_ty -> FunTy (arg_ty, acc_ty)) args ty in
      SLet { is_rec=true; name=x; args=args; ty=full_ty; value=e1; body=e2 }
    }
  | LET x=VAR EQ e1=expr IN e2=expr {
      SLet { is_rec=false; name=x; args=[]; ty=BoolTy; value=e1; body=e2 }
    }
  | LET REC x=VAR EQ e1=expr IN e2=expr {
      SLet { is_rec=true; name=x; args=[]; ty=BoolTy; value=e1; body=e2 }
    }
  | LET REC x=VAR vars=nonempty_var_list EQ e1=expr IN e2=expr {
      let args = List.map (fun x -> (x, BoolTy)) vars in
      SLet { is_rec=true; name=x; args=args; ty=BoolTy; value=e1; body=e2 }
    }
  | IF e1=expr THEN e2=expr ELSE e3=expr { SIf (e1, e2, e3) }
  | FUN arg=arg args=arg_list ARROW e=expr {
      List.fold_right (fun (x, ty) acc -> SFun { arg=(x, ty); args=[]; body=acc }) (arg :: args) e
    }
  | ASSERT e=expr { SAssert e }
  | expr0 { $1 }

expr0:
  | expr0 OR expr1 { SBop(Or, $1, $3) }
  | expr1 { $1 }

expr1:
  | expr1 AND expr2 { SBop(And, $1, $3) }
  | expr2 { $1 }

expr2:
  | expr3 relop expr3 { SBop($2, $1, $3) }
  | expr3 { $1 }

expr3:
  | expr3 ADD expr4 { SBop(Add, $1, $3) }
  | expr3 SUB expr4 { SBop(Sub, $1, $3) }
  | expr4 { $1 }

expr4:
  | expr4 MUL expr5 { SBop(Mul, $1, $3) }
  | expr4 DIV expr5 { SBop(Div, $1, $3) }
  | expr4 MOD expr5 { SBop(Mod, $1, $3) }
  | expr5 { $1 }

expr5:
  | expr5 expr6 %prec APP { SApp($1, $2) }
  | expr6 { $1 }

expr6:
  | UNIT { SUnit }
  | TRUE { STrue }
  | FALSE { SFalse }
  | NUM { SNum $1 }
  | VAR { SVar $1 }
  | LPAREN expr=expr RPAREN { expr }

relop:
  | LT { Lt }
  | LTE { Lte }
  | GT { Gt }
  | GTE { Gte }
  | EQ { Eq }
  | NEQ { Neq }
