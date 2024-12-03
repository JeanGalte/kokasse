open Lexing
open Format
open Stdlib
open Parser
open Parser_exceptions

exception Impossible

exception Erreur_lex of string

type token2 = Parser.token list

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
  | RETURN -> "RETURN"
  | VAL -> "VAL"
  | VAR -> "VAR"
  | INT i -> "INT(" ^ string_of_int i ^ ")"
  | BOOL b -> "BOOL(" ^ string_of_bool b ^ ")"
  | STRING s -> "STRING(" ^ s ^ ")"
  | IDENT id -> "IDENT(" ^ id ^ ")"

let print_token (t : Parser.token) : unit =
  print_string (" "^(token_to_string t)^" ")

let rec last_elem l =
  match l with
  | [x] -> x
  | _ :: tl -> last_elem tl
  | _ -> raise Impossible


let is_beg_cont (token : token2) : bool =
  let t = last_elem token in
  match t with
  | PLUS | MOINS | FOIS | DIV | MOD | CONCAT
  | LT | GT | LAB | RAB | DOUBLE_EGAL | NEQ
  | AND  | OR | THEN | ELSE | RP | RB
  | COMMA | RIGHTARR | LB | EGAL | DOT | DOUBLE_DOT_EGAL
    -> true
  | _ -> false

let is_end_cont (token : token2) : bool =
  let t = last_elem token in 
  match t with
  | PLUS | MOINS | FOIS | DIV | MOD | CONCAT
  | LT | GT | LAB | RAB | DOUBLE_EGAL | NEQ
  | AND | OR | LP | LB | COMMA
    -> true
  | _ -> false

let ucond (lastt : Parser.token) (nextt : token2) : bool =
  not ((is_end_cont [lastt]) || (is_beg_cont nextt)) 

let add_indent_data (tokenf : lexbuf -> token2) : lexbuf -> token2 =
  let last_tok = ref Parser.SEMIC in
  let pile_indent = ref [0] in
  fun l ->
  match tokenf l with
  | [RET] ->
     let nextt = tokenf l in
     let p = lexeme_start_p l in
     let c = p.pos_cnum - p.pos_bol in
     let m = ref (List.hd !pile_indent) in
     let em = (
         if c > !m then
           let emit_lb =
             if (ucond !last_tok nextt) then
               (last_tok := LB; true)
             else
               false in
           if (!last_tok = LB) then pile_indent := c :: !pile_indent; 
           if emit_lb then
             Parser.LB :: nextt
         else
	   nextt 
           else
             let rb_count = ref 0 in 
	     while c < !m do
	       pile_indent := List.tl !pile_indent;
	       m := List.hd !pile_indent;
	       if (nextt <> [RB]) then (rb_count := !rb_count+1);
	       
	     done;
	     if c > !m then
               (print_string "Erreur d'indentation"; (exit 1))
	     else
	       let rbs =
                 List.init
                   (2 * !rb_count)
                   (fun i -> if i mod 2 = 0 then Parser.SEMIC else Parser.RB)
               in
               if !rb_count >= 1 then last_tok := RB;
               let p_d =
                 if (ucond !last_tok nextt) then
                   Parser.SEMIC :: nextt
                 else
                   nextt
          in
	  rbs @ p_d
       ) in
     last_tok := (last_elem nextt); em
  | [EOF] ->
     let c = 0 in
     let m = ref (List.hd !pile_indent) in
     let rb_count = ref 0 in
     while c < !m do
       pile_indent := List.tl !pile_indent;
       m := List.hd !pile_indent;
       rb_count := (!rb_count + 1);
     done;
     let rbs =
       List.init
         (2 * !rb_count)
         (fun i -> if i mod 2 = 0 then Parser.SEMIC else Parser.RB)
     in
        rbs @ [EOF]
  | _ as ltok -> last_tok := (last_elem ltok); ltok
  

let add_semics (tokenf : lexbuf -> token2) : lexbuf -> token2 =
  fun l ->
  let t = tokenf l in
  match t with
  | [RB] -> [SEMIC; RB]
  | _ as t -> t

let print_token_f (tokenf : lexbuf -> Parser.token) : lexbuf -> Parser.token =
  fun l ->
  let t = tokenf l in print_token t; print_newline () ; t

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
     let a = tokenf l in
     match a with
     | [x] -> buf := None; x
     | h :: tl -> buf := Some tl; h
     | _ -> raise Impossible


let parse_with_error (lexbuf : lexbuf) (filename : string) : Ast.program = 
  let lexed_with_indent = add_indent_data Lexer.token in
  let lexed_with_semics = add_semics lexed_with_indent in
  let lexed_without_list = token2f lexed_with_semics in
  let lexed_with_print  = print_token_f lexed_without_list in
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
   
