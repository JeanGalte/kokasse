open Ast
open Printer

(*
  On renvoie le message d'erreur au format : message, ligne, début-fin de colonne
  Pour l'instant, pas implémenté pour la position exacte de l'erreur
  Pour ça je pense qu'il vaut mieux modifier tout l'AST, on dit qu'une expression
  a un contenu expressif et une position. Il faut donc l'intégrer au parsing. 
*)
exception Type_err of string

let check_type_err (e : expr) (expect_type : o_type) (actual_type : o_type) =
  if expect_type <> actual_type then 
  raise
    (Type_err
       (Printf.sprintf
       "L'expression %s devrait avoir le type %s, or elle a le type %s"
       (string_of_expr e)
       (string_of_type expect_type)
       (string_of_type actual_type)
       )
    )
  else ()

(* Vérifie si les deux types correspondent bien au type attendu *)
let check_type_err_bin
      (e1 : expr)
      (e2 : expr)
      (expect_type : o_type)
      (actual_type1 : o_type)
      (actual_type2 : o_type)
  =
  check_type_err e1 expect_type actual_type1;
  check_type_err e2 expect_type actual_type2

exception Unrec_id of string 

exception Not_callable of string 

module Venv = Map.Make(String)

type venv = o_type Venv.t

let get_type (id : ident) (e : venv) : o_type =
  try Venv.find id e
  with
    Not_found ->
    raise (Unrec_id (Printf.sprintf "Identifiant %s non reconnu" id))
  
let empty_venv : venv =
  Venv.of_list
          [
            ("True", Typename "bool");
            ("False", Typename "bool");
          ]

let rec type_expr (e : expr) (venv : venv)  : o_type =
  match e with
  | Unit -> Typename "unit"
  | Int _ -> Typename "int"
  | String _ -> Typename "string"
  | Bool _ -> Typename "bool"
  | Id i -> get_type i venv 
  | If_then_else (cond, e1, e2) -> type_if_expr cond e1 e2 venv     
  | Unop (u, e) -> type_unop_expr u e venv
  | Binop (b, e1, e2) -> type_binop_expr b e1 e2 venv
  | _ -> print_string "pas implémenté"; exit 2
and type_if_expr (cond : expr) (e1 : expr) (e2 : expr) (venv : venv) : o_type =
  let tcond = type_expr cond venv in
     let t1 = type_expr e1 venv in
     let t2 = type_expr e2 venv in
     check_type_err cond (Typename "bool") tcond;
     check_type_err_bin e1 e2 t1 t1 t2;
     t1
and type_unop_expr (u : unop) (e : expr) (venv : venv) : o_type =
  let t = type_expr e venv in 
  match u with
  | Not -> check_type_err e (Typename "bool") t; Typename "bool"
  | Nint ->  check_type_err e (Typename "int") t; Typename "int"
and type_binop_expr (b : binop) (e1 : expr) (e2 : expr) (venv : venv) : o_type=
  let t1, t2 = type_expr e1 venv, type_expr e2 venv in
  match b with 
  | Plus | Less | Times | Div | Mod ->
     check_type_err_bin e1 e2 (Typename "int") t1 t2; Typename "int"
  | Lt | Gt | Slt | Sgt ->
     check_type_err_bin e1 e2 (Typename "int") t1 t2; Typename "bool"
  | And | Or ->
     check_type_err_bin e1 e2 (Typename "bool") t1 t2; Typename "bool"
  | Eq | Neq ->
     check_type_err_bin e1 e2 t1 t1 t2; Typename "bool"
  | Concat ->
     try (check_type_err_bin e1 e2 (Typename "string") t1 t2; Typename "string")
     with Type_err _ ->
       match (t1, t2) with
       | (Composed_type ("list", elt1), Composed_type ("list", elt2)) ->
          if elt1 <> elt2 then
            raise (Type_err
                     (Printf.sprintf 
                        "Les éléments de %s sont de type %s, et ceux de %s de type %s"
                        (string_of_expr e1)
                        (string_of_type elt1)
                        (string_of_expr e2)
                        (string_of_type elt2)
                     )
                  );
          (Composed_type ("list", elt1))
       | _ ->
          raise (Type_err
                   (Printf.sprintf
                      "L'opérateur ++ exige deux chaines de caractères\
                       ou bien deux listes d'éléments de même type, or
                       %s est de type %s et %s est de type %s"
                      (string_of_expr e1)
                      (string_of_type t1)
                      (string_of_expr e2)
                      (string_of_type t2)
                   )
                )


let type_fun (f : expr) (venv : venv) (fun_name : string) : result_type =
  ignore fun_name;
  type_expr f venv, None
 
let rec type_prog (p : program) (venv : venv) : unit =
  match p with
  | [] -> ()
  | (fun_name, funbody) :: tl ->
     let args_map = Venv.of_list funbody.args in
     let fun_type =
       type_fun
         funbody.body
         (Venv.merge (fun _ x _ -> x) args_map venv)
         fun_name        
     in
     (match funbody.ret_type with
      | None -> ()
      | Some rt ->
         if rt <> fun_type then
           raise (Type_err 
                    (Printf.sprintf                   
                       "La fonction %s est anotée par le type %s, mais est typée %s"
                       fun_name
                       (string_of_rtype rt)
                       (string_of_rtype fun_type)
                    )
                 )
     );
     let calcul_t =
       Calcul_t (List.map snd funbody.args, fun_type) in 
     type_prog tl (Venv.add fun_name calcul_t venv) 
     
let typecheck (p : program) : unit =
  type_prog p empty_venv 
       
      
