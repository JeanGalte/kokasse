open Ast
    
exception Non_implemente

let eval (p : program) : unit =
  match p with
  | [x] when fst x = "main" ->
    let func = snd x in 
    (match func.body with
     | Atom (Int i) -> print_string ("Le main renvoie"^(string_of_int i))
     | _ -> raise Non_implemente
    )
 
  | _ -> raise Non_implemente
