open Lexing
open Format
open Stdlib
open Parser
open Parser_exceptions

exception Impossible

exception Erreur_lex of string

let token_to_string (token : Parser.token) =
  match token with
  | EOF -> "EOF"
  | LB -> "LB"
  | RB -> "RB"
  | LP -> "LP"
  | RP -> "RP"
  | CRG -> "CRG"
  | CRD -> "CRD"
  | LAB -> "LAB"
  | RAB -> "RAB"
  | COMMA -> "COMMA"
  | DOUBLE_DOT -> "DOUBLE_DOT"
  | DOUBLE_DOT_EGAL -> "DOUBLE_DOT_EGAL"
  | SEMIC -> "SEMIC"
  | RIGHTARR -> "RIGHTARR"
  | DOT -> "DOT"
  | RET -> "RET"
  | PLUS -> "PLUS"
  | MOINS -> "MOINS"
  | FOIS -> "FOIS"
  | DIV -> "DIV"
  | MOD -> "MOD"
  | CONCAT -> "CONCAT"
  | GT -> "GT"
  | EGAL -> "EGAL"
  | DOUBLE_EGAL -> "DOUBLE_EGAL"
  | NEQ -> "NEQ"
  | AND -> "AND"
  | OR -> "OR"
  | LT -> "LT"
  | NOT -> "NOT"
  | NINT -> "NINT"
  | FUN -> "FUN"
  | FN -> "FN"
  | IF -> "IF"
  | THEN -> "THEN"
  | ELSE -> "ELSE"
  | ELIF -> "ELIF"
  | RETURN -> "RETURN"
  | VAL -> "VAL"
  | VAR -> "VAR"
  | INT i -> "INT(" ^ string_of_int i ^ ")"
  | BOOL b -> "BOOL(" ^ string_of_bool b ^ ")"
  | STRING s -> "STRING(" ^ s ^ ")"
  | IDENT id -> "IDENT(" ^ id ^ ")"

let print_token (t : Parser.token) : unit =
  print_string (" "^(token_to_string t)^" ")          

let print_tokens_flow (tokenf : lexbuf -> token) : lexbuf -> token =
  fun l ->
  let t = tokenf l in
  print_token t; print_newline (); t

let parse_with_error (lexbuf : lexbuf) (filename : string) : Ast.program =
  let lexed = Lexer.next_token Lexer.token in
  let lexed_with_print = print_tokens_flow lexed in
  try (Parser.prog lexed_with_print lexbuf) with
  | Lexer.Erreur_lexicale msg  ->
     let p = lexeme_start_p lexbuf in
     let l = p.pos_lnum in
     let c = p.pos_cnum - p.pos_bol in
     fprintf err_formatter
             "File \"%s\", line %d, character %d : \n %s\n" 
             filename
             l
             c
             msg;
     exit 1
  | Bloc_malforme ->
     let p = lexeme_start_p lexbuf in
     let l = p.pos_lnum in
     let c = p.pos_cnum - p.pos_bol in
     fprintf err_formatter
             "File \"%s\", line %d, character %d : \n\
              Le bloc est mal formé, il ne peut pas se terminer par une déclaration\n"
             filename
             l
             c;
     exit 1
  |  Parser.Error ->
      let p = lexeme_start_p lexbuf in
      let l = p.pos_lnum in
      let c = p.pos_cnum - p.pos_bol in
      fprintf err_formatter
              "File \"%s\", line %d, character %d :\n erreur à l'analyse syntaxique" 
              filename
              l
              c;
      exit 1

let () = 

  let p_only = ref false in
  let t_only = ref false in
  let filename = ref "" in 
  
  let usage_msg = "Utilisation  : ./koka [--parse-only | --type-only] <fichier.koka> " in 

  let get_filename (f : string) : unit =
    filename := f in
  
  let speclist =
    [("--parse-only", Arg.Set p_only, "S'arrêter après l'analyse syntaxique");("--type-only", Arg.Set t_only, "S'arrêter après l'analyse sémantique")] in
  Arg.parse speclist get_filename usage_msg; 
  let p,t = !p_only, !t_only in
  if p then    
    let l = Lexing.from_channel (open_in !filename) in 
     ignore (parse_with_error l !filename)
  else
    if t then
      print_string "l'analyse sémantique n'est pas encore implémentée"
    else
      print_string "la production de code n'est pas encore implémentée"
   
