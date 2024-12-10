open Lexing
open Format
open Stdlib
(* open Parser *)
open Parser_exceptions
open Typecheck

exception Impossible

exception Erreur_lex of string

(* let token_to_string (token : Parser.token) = *)
(*   match token with *)
(*   | EOF -> "EOF" *)
(*   | LB -> "LB" *)
(*   | RB -> "RB" *)
(*   | LP -> "LP" *)
(*   | RP -> "RP" *)
(*   | CRG -> "CRG" *)
(*   | CRD -> "CRD" *)
(*   | LAB -> "LAB" *)
(*   | RAB -> "RAB" *)
(*   | COMMA -> "COMMA" *)
(*   | DOUBLE_DOT -> "DOUBLE_DOT" *)
(*   | DOUBLE_DOT_EGAL -> "DOUBLE_DOT_EGAL" *)
(*   | SEMIC -> "SEMIC" *)
(*   | RIGHTARR -> "RIGHTARR" *)
(*   | DOT -> "DOT" *)
(*   | RET -> "RET" *)
(*   | PLUS -> "PLUS" *)
(*   | MOINS -> "MOINS" *)
(*   | FOIS -> "FOIS" *)
(*   | DIV -> "DIV" *)
(*   | MOD -> "MOD" *)
(*   | CONCAT -> "CONCAT" *)
(*   | GT -> "GT" *)
(*   | EGAL -> "EGAL" *)
(*   | DOUBLE_EGAL -> "DOUBLE_EGAL" *)
(*   | NEQ -> "NEQ" *)
(*   | AND -> "AND" *)
(*   | OR -> "OR" *)
(*   | LT -> "LT" *)
(*   | NOT -> "NOT" *)
(*   | NINT -> "NINT" *)
(*   | FUN -> "FUN" *)
(*   | FN -> "FN" *)
(*   | IF -> "IF" *)
(*   | THEN -> "THEN" *)
(*   | ELSE -> "ELSE" *)
(*   | ELIF -> "ELIF" *)
(*   | RETURN -> "RETURN" *)
(*   | VAL -> "VAL" *)
(*   | VAR -> "VAR" *)
(*   | INT i -> "INT(" ^ string_of_int i ^ ")" *)
(*   | BOOL b -> "BOOL(" ^ string_of_bool b ^ ")" *)
(*   | STRING s -> "STRING(" ^ s ^ ")" *)
(*   | IDENT id -> "IDENT(" ^ id ^ ")" *)

(* let print_token (t : Parser.token) : unit = *)
(*   print_string (" "^(token_to_string t)^" ")           *)

(* let print_tokens_flow (tokenf : lexbuf -> token) : lexbuf -> token = *)
(*   fun l -> *)
(*   let t = tokenf l in *)
(*   print_token t; print_newline (); t *)

let print_err_msg (lb : lexbuf) (filename : string) (msg : string): unit =
  let beg_pos = lexeme_start_p lb in
  let end_pos = lexeme_end_p lb in
  let l = beg_pos.pos_lnum in
  let beg_c = beg_pos.pos_cnum - beg_pos.pos_bol in
  let end_c = end_pos.pos_cnum - end_pos.pos_bol in
  fprintf err_formatter  "File \"%s\", line %d, characters %d-%d:\n%s" filename l beg_c end_c msg
  

let parse_with_error (lexbuf : lexbuf) (filename : string) : Ast.program =
  let enriched_lexer = Lexer.next_token Lexer.token in
  try (Parser.prog enriched_lexer lexbuf) with
  | Lexer.Erreur_lexicale msg  ->
     print_err_msg lexbuf filename msg;
     exit 1
  | Bloc_malforme ->
     let msg = "Le bloc est mal formé, il ne peut pas se terminer par une déclaration" in 
     print_err_msg lexbuf filename msg; 
     exit 1
  |  Parser.Error ->
      let msg = "erreur à l'analyse syntaxique" in
      print_err_msg lexbuf filename msg; 
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
  let l = Lexing.from_channel (open_in !filename) in
  let prog = parse_with_error l !filename in
  (if p then exit 0);
   (if t then
     print_string
       "La production de code n'étant pas encore implémentée, \
        l'option \'type-only\' n'a pas d'effet");   
   typecheck prog
