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
  | "in" { IN }
  | "fun" { FUN }
  | "mod" { MOD }
  | "true" { TRUE }
  | "false" { FALSE }
  | "()" { UNIT }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "/" { DIVIDED }
  | "<=" { LE }
  | ">=" { GE }
  | "<>" { NEQ }
  | "<" { LT }
  | ">" { GT }
  | "=" { EQ }
  | "&&" { ANDAND }
  | "||" { OROR }
  | "->" { ARROW }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | num { NUM (int_of_string (Lexing.lexeme lexbuf)) }
  | var { VAR (Lexing.lexeme lexbuf) }
  | eof { EOF }
  | _ { failwith ("Unexpected character: " ^ (Lexing.lexeme lexbuf)) }
