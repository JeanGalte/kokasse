open Ast
    
exception Non_implemente

exception Erreur_typage

let div_koka a b = if b = 0 then a else a/b

let ( / ) = div_koka

let print_expr (e : expr) : unit =
  match e with
  | Bool b -> print_string (if b then "true" else "false")
  | Int i -> print_string (string_of_int i)
  | String s -> print_string s
  | Unit -> print_string "()"
  | _ -> raise Non_implemente
  

let eval_unop (a : expr) (o : unop) : expr =
  match (o, a) with
  | (Nint, Int i) -> Int (-i)
  | (Not, Bool b) -> Bool (not b)
  | _ -> raise Erreur_typage  

let eval_binop (b : binop) (a1 : expr) (a2 : expr) : expr =
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

let rec eval_expr (e : expr) : expr =
  match e with
  | Int i -> Int i
  | Bool b -> Bool b
  | Binop (b, e1, e2) -> eval_binop b (eval_expr e1) (eval_expr e2) 
  | If_then_else (e1, e2, e3) ->
    (
      match (eval_expr e1) with
      | Bool b -> eval_expr (if b then e2 else e3)
      | _ -> raise Erreur_typage
    )
  | Unop (u ,e) -> eval_unop (eval_expr e) u
  | _ -> raise Non_implemente

let eval (p : program) : unit =
  match p with
  | [x] when fst x = "main" ->
    let func = snd x in
    let eval_body = eval_expr func.body in
    print_expr eval_body; print_newline ()
  | _ -> raise Non_implemente
