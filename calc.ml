let _ =
  try 
    let filename = Sys.argv.(1) in
    Printf.printf "Ouverture du fichier : %s\n" filename; (* Débogage *)
    let f = open_in filename in
    let lexbuf = Lexing.from_channel f in
    Printf.printf "Fichier ouvert avec succès\n"; (* Débogage *)
    let ast = Parser.calc Lexer.token lexbuf in
    Printf.printf "AST généré avec succès\n"; (* Débogage *)
    Eval.eval_program ast;
    Printf.printf "Évaluation terminée\n" (* Débogage *)
  with  
  | Lexer.EOF -> Printf.printf "Fin du fichier atteinte\n"; exit 0
  (* | Lexer.Eof -> Printf.printf "Fin du fichier atteinte\n"; () *)
  | Sys_error msg -> Printf.eprintf "Erreur système : %s\n" msg; exit 1
  | Invalid_argument _ -> Printf.eprintf "Erreur : Aucun fichier spécifié\n"; exit 1