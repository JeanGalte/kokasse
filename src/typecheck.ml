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
let check_multiple_type_err
      (el : expr list) 
      (expect_types : o_type list)
      (actual_types : o_type list)
  =
  let cl = List.combine el (List.combine expect_types actual_types) in
  List.iter (fun (ex, (et, at)) -> (check_type_err ex et at)) cl  


let wrap_mcheck
      (el : expr list) 
      (expect_types : o_type list)
      (actual_types : o_type list)
      (msg : string)
  =
  try check_multiple_type_err el expect_types actual_types with
    Type_err s ->
    raise (Type_err (s^" : "^msg))
    

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
     wrap_mcheck
       [e1 ; e2]
       [t1; t1]
       [t1; t2]
       " dans une expression conditionnelle, les deux \
        possibilités doivent avoir le même type"  ;
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
     wrap_mcheck
       [e1; e2]
       [Typename "int"; Typename "int"]
       [t1; t2]
       (Printf.sprintf "L'opérateur %s prend en argument deux entiers"
                       (string_of_binop b)
       );
     Typename "int"
  | Lt | Gt | Slt | Sgt ->
     wrap_mcheck
       [e1; e2]
       [Typename "int"; Typename "int"]
       [t1; t2]
       (Printf.sprintf "L'opérateur %s prend en argument deux entiers"
                       (string_of_binop b)
       );
     Typename "bool"
  | And | Or ->
     wrap_mcheck
       [e1; e2]
       [Typename "bool"; Typename "bool"]
       [t1; t2]
       (Printf.sprintf "L'opérateur %s prend en argument deux booléens"
                       (string_of_binop b)
       );
     Typename "bool"
  | Eq | Neq ->
     wrap_mcheck
       [e1; e2]
       [Typename "bool"; Typename "bool"]
       [t1; t2]
       (Printf.sprintf "L'opérateur %s prend en argument deux expressions de même type"
                       (string_of_binop b)
       );
     Typename "bool"    
  | Concat ->
     (* Les deux derniers cas sont un peu bricolés pour ne pas se faire crier dessus par le typechecker *)
     match (t1, t2) with
     | (Typename "string", Typename "string") -> Typename "string"
     | (Composed_type ("list", elt1), Composed_type ("list", elt2))
          when elt1 = elt2 -> Composed_type ("list", elt1)
     | (Composed_type ("list", elt1), Composed_type ("list", elt2)) ->
        wrap_mcheck
          [e1; e2]
          [Composed_type ("list", elt1); Composed_type ("list", elt1)]
          [t1; t2]
          (Printf.sprintf
             "Les éléments de la première liste sont de type %s\
              alors que les éléments de la seconde liste sont de type %s\
              or on ne peut concaténer que deux listes d'éléments de même type"
             (string_of_type elt1)
             (string_of_type elt2)
          );
        Composed_type ("list", elt1)
     | _ ->
        raise (Type_err
                 (Printf.sprintf
                    "L'opérateur ++ prend en argument deux listes d'éléments\
                     de même type, ou bien deux strings. Or le type de %s \
                     est %s et celui de %s est %s"
                    (string_of_expr e1)
                    (string_of_type t1)
                    (string_of_expr e2)
                    (string_of_type t2)
                 )
              )
        
        
let type_fun (f : funbody) (fun_name : string) (venv : venv)  : result_type =
  ignore fun_name; 
  let args_map = Venv.of_list f.args in
  let env_w_args = Venv.merge (fun _ x _ -> x) args_map venv in
  let effects : effect list = [] in (* À faire *)
  let rtype = (type_expr f.body env_w_args, effects) in
  let expect_rtype = f.ret_type in
  (match expect_rtype with
  | None -> ()
  | Some rt ->
     if rt <> rtype then
       (raise (Type_err
             (
               Printf.sprintf
                 "La fonction %s est censée avoir un type retour %s,
                  mais elle a un type retour %s"
                 fun_name
                 (string_of_rtype rt)
                 (string_of_rtype rtype)
             ))); ()
  );
  rtype
  
  
let rec type_prog (p : program) (venv : venv) : unit =
  match p with
  | [] -> ()
  | (fun_name, funbody) :: tl ->
     let ret_type = type_fun funbody fun_name venv in
     let calcul_type = Calcul_t (List.map snd funbody.args, ret_type) in 
     type_prog tl (Venv.add fun_name calcul_type venv) 
     
let typecheck (p : program) : unit =
  type_prog p empty_venv 
       
      
