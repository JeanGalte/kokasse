{
open Lexing
open Parser

exception Erreur_lexicale of string

let id_or_kwd =
  [
    ("println", PRINT);
    ("fun", FUN)
  ]


let find_id (s : string) =
  match List.assoc_opt s id_or_kwd with
  | Some kw -> kw
  | None -> IDENT s

}

let space = [' ' '\r']
let digit = ['0'-'9']
let integer = '0' | '-'? ['1'-'9'] digit* 
let lower = ['a'-'z'] | _
let upper = ['A'-'Z']
let other = digit | lower | upper
let ident = lower (other | (other '-' (lower | upper)))*'-'?

rule token = parse
  | eof { EOF }
  | space { token lexbuf }
  | "\n" { new_line lexbuf; token lexbuf }
  | "//" { single_line_comment lexbuf }
  | "/*" { multi_line_comment lexbuf }
  | integer as i { INT (int_of_string i) }
  | ident as id { find_id id }
  | '{' { LB }
  | '}' { RB }
  | '(' { LP }
  | ')' { RP }
  | _ { raise (Erreur_lexicale ("Lexème non reconnu")) }
and
  single_line_comment = parse
  | "\n" { new_line lexbuf; token lexbuf }
  | _ { single_line_comment lexbuf }
and
  multi_line_comment = parse
  | "*/" { token lexbuf }
  | "\n" { new_line lexbuf; multi_line_comment lexbuf}
  | _ { multi_line_comment lexbuf }
  
