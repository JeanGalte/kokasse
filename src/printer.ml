open Ast


let rec string_of_type (t  : o_type) : string =
  match t with
  | Typename ident -> ident
  | Composed_type (ident, t) -> "<"^ident^">"^(string_of_type t)
  | Calcul_t (tl, rt) ->
     List.fold_left
       (fun t_name t_suiv -> t_name ^ "->" ^ (string_of_type t_suiv))
       (string_of_rtype rt)
       tl
and
  string_of_rtype (r : result_type) : string =
  let finaltype, effects = fst r, snd r in
  let effects = (match effects with | None -> [] | Some l -> l) in
  List.fold_left
    (fun eff1 eff2 -> eff1 ^ " " ^ eff2)
    (string_of_type finaltype)
    effects  



(* À priori pas utile, on affichera plutôt l'expression dans le fichier directmeent *)
(* let rec string_of_expr (e : expr) : unit = *)
(*   match e with *)
(*   | Id i -> i *)
(*   | Unit -> "()" *)
(*   | String s -> ("\""^s^"\"") *)
(*   | Int i -> string_of_int i *)
(*   | Elist l -> *)
(*      List.fold_left ^ "]" ("[" ::  (List.map string_of_expr l)) *)
(*   | Return e -> "return " ^ (string_of_expr e) *)
(*   | Fcall (e, l) -> *)
(*      (string_of_expr e) ^ "(" ^ *)
(*        List.fold_left *)
(*          (fun s1 s2 -> s1 ^ " " ^ s2) *)
(*          ")" *)
(*          (List.map string_of_expr l) *)
(*   | Fn f -> *)
(*      "fun "^ *)
(*        List.fold_left *)
(*   and *)
(*     string_of_params (pl : param list) : string = *)
(*     List.fold *)
