open Ast

module StringMap = Map.Make(String)
type env = int StringMap.t

let rec eval_expr e =
  match e with
  | Add(left, right) -> eval_expr left + eval_expr right
  | Mul(left, right) -> eval_expr left * eval_expr right
  | Sub(left, right) -> eval_expr left - eval_expr right
  | Div(left, right) -> eval_expr left / eval_expr right
  | Neg(x) -> - eval_expr x
  | Pos(x) -> eval_expr x
  | Integer(x) ->  x
  | And(left, right) -> if eval_expr left <> 0 && eval_expr right <> 0 then 1 else 0
  | Or(left, right) -> if eval_expr left <> 0 || eval_expr right <> 0 then 1 else 0
  | Not(x) -> if eval_expr x = 0 then 1 else 0
  | Eq(left, right) -> if eval_expr left = eval_expr right then 1 else 0
  | Lt(left, right) -> if eval_expr left < eval_expr right then 1 else 0
  | Gt(left, right) -> if eval_expr left > eval_expr right then 1 else 0

(*let env = Hashtbl.create 10   Table pour stocker les variables *)

let rec eval_instr program_map i =
  match i with
  | Print(l) -> 
      List.iter (fun x -> print_int (eval_expr x); print_string " " ) l; 
      print_newline()
  | Rem(_) -> 
      print_newline()
  | Let(id, e) -> 
      let value = eval_expr e in 
      Printf.printf "%s = %d\n" id value
  | Input(id) -> 
      Printf.printf "Veuillez saisir une valeur pour %s: " id; 
      let value = read_int() in 
      Printf.printf "%s = %d\n" id value
  | Goto(line_number) -> 
      Printf.printf "Goto %d\n" line_number;
      (* Exécute l'instruction de la ligne cible sans modifier l'ordre d'exécution *)
      (match Hashtbl.find_opt program_map line_number with
       | Some instr -> 
           Printf.printf "Exécution de la ligne %d (Goto):\n" line_number;
           eval_instr program_map instr
       | None -> 
           Printf.printf "Ligne %d inexistante, aucune instruction exécutée.\n" line_number)
  | End -> 
      Printf.printf "Program terminated.\n"; 
      exit 0



let eval_program program =
  (* Crée une table de hachage pour accéder aux instructions par numéro de ligne *)
  let program_map = Hashtbl.create (List.length program) in
  List.iter (fun (line_num, instr) -> Hashtbl.add program_map line_num instr) program;
        
          let rec exec pc =
            match Hashtbl.find_opt program_map pc with
            | None -> Printf.printf "Fin du programme.\n"
            | Some instr ->
              Printf.printf "Exécution de la ligne %d :\n" pc;
              eval_instr program_map instr;
              exec (pc + 1)
          in
          exec 1  (* Commence à la ligne 1 *)
          let eval_program program =
            (* Crée une table de hachage pour accéder aux instructions par numéro de ligne *)
            let program_map = Hashtbl.create (List.length program) in
            List.iter (fun (line_num, instr) -> Hashtbl.add program_map line_num instr) program;
          
            let rec exec pc =
              match Hashtbl.find_opt program_map pc with
              | None -> Printf.printf "Fin du programme.\n"
              | Some instr ->
                Printf.printf "Exécution de la ligne %d :\n" pc;
                eval_instr program_map instr;
                exec (pc + 1)
            in
            exec 1  (* Commence à la ligne 1 *)
                