(* lex.mll *)
{
open Par
}
(* Regular expressions given in spec*)
let whitespace = [' ' '\t' '\n' '\r']+
let num = '-'? ['0'-'9']+
let var = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*

rule read =
  parse
  | whitespace { read lexbuf }
  (* Keywords and multi-character operators *)
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "let" { LET }
  | "rec" { REC }
  | "in" { IN }
  | "fun" { FUN }
  | "mod" { MOD }
  | "true" { TRUE }
  | "false" { FALSE }
  | "&&" { AND }
  | "||" { OR }
  | "<=" { LTE }
  | ">=" { GTE }
  | "<>" { NEQ }
  | "->" { ARROW }
  | "()" { UNIT }
  | "int" { INT }
  | "bool" { BOOL }
  | "unit" { UNIT }
  | "assert" { ASSERT }
  (* Single-character tokens *)
  | ":" { COLON }
  | "=" { EQ }
  | "+" { ADD }
  | "*" { MUL }
  | "/" { DIV }
  | "<" { LT }
  | ">" { GT }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "_" { VAR "_" }
  (* Numbers and identifiers *)
  | num { NUM (int_of_string (Lexing.lexeme lexbuf)) }
  | var { VAR (Lexing.lexeme lexbuf) }
  | "-" { SUB } 
  | eof { EOF }
  | _ { failwith ("Invalid character: " ^ Lexing.lexeme lexbuf) }
