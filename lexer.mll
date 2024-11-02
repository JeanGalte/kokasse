{
open Lexing
open Parser

exception Erreur_lexicale of string

exception Impossible

let id_or_kwd =
  [
    ("println", PRINT);
    ("fun", FUN);
    ("string", STRING_TYPE);
    ("int", INT_TYPE);
    ("bool", BOOL_TYPE);
   ]

let find_id (s : string) =
  match List.assoc_opt s id_or_kwd with
  | Some kw ->  kw
  | None -> IDENT s

let lex_binop b =
  match b with
  | "+" -> PLUS
  | "-" -> MOINS
  | "*" -> FOIS
  | "/" -> DIV
  | "&&" -> AND
  | "||" -> OR
  | _ -> raise Impossible
}

let space = [' ' '\r' '\n']
let bool = "true"|"false"
let digit = ['0'-'9']
let integer = '0' | '-'? ['1'-'9'] digit* 
let lower = ['a'-'z'] |"_"
let upper = ['A'-'Z']
let other = digit | lower | upper
let ident = lower (other | (other '-' (lower | upper)))*'-'?
let binop = "+"|"-"|"*"|"/"|"&&"|"||"
     

rule token = parse
  | eof { EOF }
  | space { token lexbuf }
  | "\n" { new_line lexbuf; token lexbuf }
  | "//" { single_line_comment lexbuf }
  | "/*" { multi_line_comment lexbuf }
  | integer as i { INT (int_of_string i) }
  | bool as b {let k = if b = "true" then true else false in (BOOL k) } 
  | binop as b {lex_binop b}   
  | '{' { LB }
  | '}' { RB }
  | '(' { LP }
  | ')' { RP }
  | ',' { COMMA }
  | ':' { DOUBLE_DOT }
  | ident as id { find_id id }
  | _ { raise (Erreur_lexicale ("Lexème non reconnu"^Lexing.lexeme lexbuf)) }
and
  single_line_comment = parse
  | "\n" { new_line lexbuf; token lexbuf }
  | _ { single_line_comment lexbuf }
and
  multi_line_comment = parse
  | "*/" { token lexbuf }
  | "\n" { new_line lexbuf; multi_line_comment lexbuf}
  | _ { multi_line_comment lexbuf }
 
