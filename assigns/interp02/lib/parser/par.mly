%{
open Utils

let rec mk_app e = function
  | [] -> e
  | x :: es -> mk_app (SApp (e, x)) es
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
%right OR
%right AND
%left LT LTE GT GTE EQ NEQ
%left ADD SUB
%left MUL DIV MOD
%right ARROW

%start <prog> prog

%%

(* Entry point for the program *)
prog:
  | toplet_list EOF { $1 }

toplet_list:
  | /* empty */ { [] }
  | toplet_list toplet { $1 @ [$2] }

(* Top-level let and let rec expressions *)
toplet:
  /* Annotated top-level declarations */
  | LET x=VAR args=arg_list COLON ty=ty EQ e=expr {
      let full_ty = List.fold_right (fun (_, arg_ty) acc_ty -> FunTy(arg_ty, acc_ty)) args ty in
      { is_rec=false; name=x; args=args; ty=full_ty; value=e }
    }
  | LET REC x=VAR args=arg_list COLON ty=ty EQ e=expr {
      let full_ty = List.fold_right (fun (_, arg_ty) acc_ty -> FunTy(arg_ty, acc_ty)) args ty in
      { is_rec=true; name=x; args=args; ty=full_ty; value=e }
    }
  /* Unannotated top-level declarations */
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

(* Argument list *)
arg_list:
  | /* empty */ { [] }
  | arg_list arg { $1 @ [$2] }

(* Non-empty list of VARs *)
nonempty_var_list:
  | x=VAR { [x] }
  | nonempty_var_list x=VAR { $1 @ [x] }

(* Argument for functions *)
arg:
  | LPAREN x=VAR COLON ty=ty RPAREN { (x, ty) }
  | x=VAR { (x, BoolTy) }

(* Type definitions *)
ty:
  | INT { IntTy }
  | BOOL { BoolTy }
  | UNIT { UnitTy }
  | t1=ty ARROW t2=ty { FunTy(t1, t2) }
  | LPAREN t=ty RPAREN { t }

(* Expressions *)
expr:
  /* Annotated Let expressions */
  | LET x=VAR args=arg_list COLON ty=ty EQ e1=expr IN e2=expr {
      let full_ty = List.fold_right (fun (_, arg_ty) acc_ty -> FunTy(arg_ty, acc_ty)) args ty in
      SLet { is_rec=false; name=x; args=args; ty=full_ty; value=e1; body=e2 }
    }
  | LET REC x=VAR args=arg_list COLON ty=ty EQ e1=expr IN e2=expr {
      let full_ty = List.fold_right (fun (_, arg_ty) acc_ty -> FunTy(arg_ty, acc_ty)) args ty in
      SLet { is_rec=true; name=x; args=args; ty=full_ty; value=e1; body=e2 }
    }
  /* Unannotated Let expressions */
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
  /* Conditional Expression */
  | IF e1=expr THEN e2=expr ELSE e3=expr { SIf(e1, e2, e3) }
  /* Function definition with multiple arguments */
  | FUN arg=arg args=arg_list ARROW e=expr {
      List.fold_right
        (fun (x, ty) acc -> SFun { arg=(x, ty); args=[]; body=acc })
        (arg :: args) e
    }
  | e=expr_or { e }

(* Expressions involving logical OR *)
expr_or:
  | e1=expr_and { e1 }
  | e1=expr_or OR e2=expr_and { SBop(Or, e1, e2) }

(* Expressions involving logical AND *)
expr_and:
  | e1=expr_rel { e1 }
  | e1=expr_and AND e2=expr_rel { SBop(And, e1, e2) }

(* Expressions involving relational operators *)
expr_rel:
  | e1=expr_add { e1 }
  | e1=expr_rel op=relop e2=expr_add { SBop(op, e1, e2) }

(* Expressions involving addition and subtraction *)
expr_add:
  | e1=expr_mul { e1 }
  | e1=expr_add ADD e2=expr_mul { SBop(Add, e1, e2) }
  | e1=expr_add SUB e2=expr_mul { SBop(Sub, e1, e2) }

(* Expressions involving multiplication, division, and modulo *)
expr_mul:
  | e1=expr_app { e1 }
  | e1=expr_mul MUL e2=expr_app { SBop(Mul, e1, e2) }
  | e1=expr_mul DIV e2=expr_app { SBop(Div, e1, e2) }
  | e1=expr_mul MOD e2=expr_app { SBop(Mod, e1, e2) }

(* Function application *)
expr_app:
  | e=expr_atom es=expr_atom_list { mk_app e es }
  | e=expr_atom { e }

expr_atom_list:
  | /* empty */ { [] }
  | expr_atom_list e=expr_atom { $1 @ [e] }

(* Atomic expressions *)
expr_atom:
  | ASSERT e=expr_atom { SAssert e }
  | UNIT { SUnit }
  | TRUE { STrue }
  | FALSE { SFalse }
  | n=NUM { SNum n }
  | x=VAR { SVar x }
  | LPAREN e=expr RPAREN { e }

(* Relational operators *)
relop:
  | LT { Lt }
  | LTE { Lte }
  | GT { Gt }
  | GTE { Gte }
  | EQ { Eq }
  | NEQ { Neq }

(* Binary operators for expressions *)
%inline bop:
  | ADD { Add }
  | SUB { Sub }
  | MUL { Mul }
  | DIV { Div }
  | MOD { Mod }
  | AND { And }
  | OR { Or }
