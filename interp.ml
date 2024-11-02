open Ast
    
exception Non_implemente

exception Erreur_typage

let print_atom (a : atom) : unit =
  match a with
  | Bool b -> print_string (if b then "true" else "false")
  | Int i -> print_string (string_of_int i)
  | String s -> print_string s
  | Un -> print_string "()"
  | Id _ -> raise Non_implemente

let eval_binop (b : binop) (a1 : atom) (a2 : atom) : atom =
  (match (a1, a2) with
  | (Int i1, Int i2) ->
    Int ((match b with
        | Plus -> ( + )
        | Less -> ( - )
        | Times -> ( * )
        | Div -> ( / )
        | _ -> raise Erreur_typage
      ) i1 i2)
  | (Bool b1, Bool b2) ->
     Bool ((match b with
        | Or -> ( || )
        | And -> ( && )
        | _ -> raise Erreur_typage
      ) b1 b2)
  | _ -> raise Erreur_typage
  )

let rec eval_expr (e : expr) : atom =
  match e with
  | Atom (Int i) -> Int i
  | Atom (Bool b) -> Bool b
  | Binop (b, e1, e2) -> eval_binop b (eval_expr e1) (eval_expr e2) 
  | _ -> raise Non_implemente

let eval (p : program) : unit =
  match p with
  | [x] when fst x = "main" ->
    let func = snd x in
    let eval_body = eval_expr func.body in
    print_atom eval_body; print_newline ()
  | _ -> raise Non_implemente
