let () =
  if Array.length Sys.argv <> 2 then
    Printf.eprintf "Le format à utiliser est : %s <nom_de_fichier.koka>\n" Sys.argv.(0)
  else
    let filename = Sys.argv.(1) in
    let lexbuf = Lexing.from_channel (open_in filename) in
    let ast = Parser.prog Lexer.token lexbuf in 
    Interp.eval ast  
    
