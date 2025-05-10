let counter()=
reg (fun c -> c+1) init 0;;

let main (bouton : bool) =
 let cy = counter () in
 let(v, rdy) = exec
          load_code(); (* charger le bytecode *)

          print_string "start execution at " ; print_int cy;
          print_string "!"; print_newline ();

          run_interp() ; (* exécuter le bytecode *)

          print_string "execution is finished at "; print_int cy;
          print_string " !"; print_newline();

          42 (*retourner 42*)

          default 0 (*valeur par défaut*)
 in
 let green_led = not(rdy) in
 green_led;; (* allume la LED verte tant que lecalcul n’est pas terminé*)