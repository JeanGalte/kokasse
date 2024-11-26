{
open Lexing
open Parser

exception Erreur_lexicale of string

exception Impossible

let id_or_kwd =
  [
    ("fun", FUN);
    ("fn", FN);
    ("if", IF);
    ("then", THEN);
    ("else", ELSE);
    ("val", VAL);
    ("var", VAR)
   ]

let find_id (s : string) =
  match List.assoc_opt s id_or_kwd with
  | Some kw ->  kw
  | None -> IDENT s

let lex_binop b =
  match b with
  | "%" -> MOD
  | "+" -> PLUS
  | "-" -> MOINS
  | "*" -> FOIS
  | "/" -> DIV
  | "&&" -> AND
  | "||" -> OR
  | "<=" -> LT
  | ">=" -> GT
  | ":=" -> DOUBLE_DOT_EGAL
  | "=" -> EGAL
  | ":" -> SEMIC
  | "->" -> RIGHTARR
  | _ -> raise Impossible

let lex_unop b =
  match b with
  | '!' -> NOT
  | '~' -> NINT
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
  | eof { [EOF] }
  | space { token lexbuf }
  | "\n" { new_line lexbuf; token lexbuf }
  | "//" { single_line_comment lexbuf }
  | "/*" { multi_line_comment lexbuf }
  | integer as i { [INT (int_of_string i)] }
  | bool as b {let k = if b = "true" then true else false in [BOOL k] } 
  | binop as b { [lex_binop b] }   
  | "elif" { [ELSE; IF] }
  | '{' { [LB] }
  | '}' { [RB] }
  | '(' { [LP] }
  | ')' { [RP] }
  | ',' { [COMMA] }
  | ':' { [DOUBLE_DOT] }
  | '<' { [LAB] }
  | '>' { [RAB] }
  | '[' { [CRG] }
  | ']' { [CRD] }
  | '"' { [lex_string (Buffer.create 30) lexbuf] }
  | ident as id { [find_id id] }
    | _ { raise (Erreur_lexicale ("Lexème non reconnu"^Lexing.lexeme lexbuf)) }
and
  single_line_comment = parse
  | "\n" { new_line lexbuf; token lexbuf }
  | eof { [EOF] }
  | _ { single_line_comment lexbuf }
and
  multi_line_comment = parse
  | "*/" { token lexbuf }
  | "\n" { new_line lexbuf; multi_line_comment lexbuf}
  | eof {raise (Erreur_lexicale "Commentaire non fermé")}
  | _ { multi_line_comment lexbuf }
and lex_string buf = parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' 'n'  { Buffer.add_char buf '\n'; lex_string buf lexbuf }     
  | '\\' 't'  { Buffer.add_char buf '\t'; lex_string buf lexbuf }
  | '\\' '"'  { Buffer.add_char buf '\"'; lex_string buf lexbuf }      
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      lex_string buf lexbuf
    }
  | _ { raise (Erreur_lexicale ("Caractère non reconnu : " ^ Lexing.lexeme lexbuf)) }
  | eof { raise
            (Erreur_lexicale ("La chaîne de caractère "^(Buffer.contents buf)^"n'est pas terminée"))
        }
 
