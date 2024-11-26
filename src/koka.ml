open Lexer
open Lexing
open Format

exception Impsosible

type token2 = Parser.token list

let token2f (tokenf : lexbuf -> token2) : lexbuf -> Parser.token =
  let buf = ref None in
  fun l ->
     match !buf with
     | Some v ->
        buf := Some (List.tl v); List.hd v
     | None ->
        match tokenf l with
        | h :: tl -> buf := Some tl; h
        | _ -> raise Impossible

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Some (Parser.prog (token2f Lexer.token) lexbuf) with
  | Erreur_lexicale msg ->
    fprintf err_formatter "%a: %s\n" print_position lexbuf msg;
    None
  | Parser.Error ->
    fprintf err_formatter "%a: Erreur syntaxique \n" print_position lexbuf;
    exit 1

let () =
  if Array.length Sys.argv <> 2 then
    Printf.eprintf "Le format à utiliser est : %s <nom_de_fichier.koka>\n" Sys.argv.(0)
  else
    let filename = Sys.argv.(1) in
    let l = Lexing.from_channel (open_in filename) in 
    (match parse_with_error l with
    | None -> ()
    | Some ast -> Interp.eval ast
    )
    
    
