open Ast

module Venv = Map.Make(String)

type venv = o_type Venv.t

type funenv = result_type Venv.t 

let empty_venv : venv =
  Venv.of_list
          [
            ("True", Typename "bool");
            ("False", Typename "bool");
          ]

let empty_funenv : funenv = Venv.empty
  
let type_expr (e : expr) (venv : venv) (funenv : funenv) : o_type =
  ignore venv; ignore funenv; 
  match e with
  | Unit -> Typename "unit"
  | Int _ -> Typename "int"
  | String _ -> Typename "string"
  | _ -> print_string "pas implémenté"; exit 2
  

let type_fun (f : expr) (venv : venv) (funenv : funenv) (fun_name : string) : result_type =
  ignore fun_name; 
  type_expr f venv funenv, None
  

let rec type_prog (p : program) (venv : venv) (funenv : funenv) : unit =
  match p with
  | [] -> ()
  | (fun_name, funbody) :: tl ->
     let args_map = Venv.of_list funbody.args in
     let fun_type =
       type_fun
         funbody.body
         (Venv.merge (fun _ x _ -> x) args_map venv)
         funenv
         fun_name        
     in
     (match funbody.ret_type with
      | None -> ()
      | Some t -> if t <> fun_type then print_string "ERREUR ICI\n"; exit 2
     );
     type_prog tl venv (Venv.add fun_name fun_type funenv) 
     
let typecheck (p : program) : unit =
  type_prog p empty_venv empty_funenv
       
      
