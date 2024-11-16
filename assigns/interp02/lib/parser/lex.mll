{
open Par
}

let whitespace = [' ' '\t' '\n' '\r']+
let num = '-'? ['0'-'9']+
let var = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*

rule read =
  parse
  | whitespace { read lexbuf }
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
  | "()" { UNIT }
  | ":" { COLON }
  | "int" { INT }
  | "bool" { BOOL }
  | "unit" { UNIT_TY }
  | "assert" { ASSERT }
  | "<=" { LTE }
  | ">=" { GTE }
  | "<>" { NEQ }
  | "&&" { AND }
  | "||" { OR }
  | "->" { ARROW }
  | "+" { ADD }
  | "-" { SUB }
  | "*" { MUL }
  | "/" { DIV }
  | "<" { LT }
  | ">" { GT }
  | "=" { EQ }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | num { NUM (int_of_string (Lexing.lexeme lexbuf)) }
  | var { VAR (Lexing.lexeme lexbuf) }
  | eof { EOF }
  | _ { failwith ("invalid character: " ^ Lexing.lexeme lexbuf) }
