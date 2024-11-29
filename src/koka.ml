open Lexer
open Lexing
open Format

exception Impossible

type token2 = Parser.token list

let token2f (tokenf : lexbuf -> token2) : lexbuf -> Parser.token =
  let buf = ref None in
  fun l ->
     match !buf with
     | Some v ->
        (match v with
         | [x] -> buf := None; x
         | h :: tl -> buf := Some tl; h
         | _ -> raise Impossible)        
     | None ->
        match tokenf l with
        | [x] -> buf := None; x
        | h :: tl -> buf := Some tl; h
        | _ -> raise Impossible

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try (Parser.prog (token2f Lexer.token) lexbuf) with
  | Erreur_lexicale msg  ->
    fprintf err_formatter "%a: %s\n" print_position lexbuf msg;
    exit 1
  | Parser.Error ->
    fprintf err_formatter "%a: Erreur syntaxique \n" print_position lexbuf;
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
     ignore (parse_with_error l)
  else
    if t then
      print_string "l'analyse sémantique n'est pas encore implémentée"
    else
      print_string "la production de code n'est pas encore implémentée"
    
