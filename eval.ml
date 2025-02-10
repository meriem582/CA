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

let rec eval_instr s i =
  match i with
  | Print(l) -> 
      List.iter (fun x -> print_int (eval_expr x); print_string " " ) l; 
      print_newline();
      s + 1  (* Continue à la ligne suivante *)
  | Rem(_) -> 
      print_newline(); 
      s + 1
  | Let(id, e) -> 
      let value = eval_expr e in 
      Printf.printf "%s = %d\n" id value;
      s + 1
  | Input(id) -> 
      Printf.printf "Veuillez saisir une valeur pour %s: " id; 
      let value = read_int() in 
      Printf.printf "%s = %d\n" id value;
      s + 1
  | Goto(line_number) -> 
      Printf.printf "Goto %d\n" line_number; 
      line_number  
  | If(e, line_number) -> 
    if eval_expr e = 1 then (
      Printf.printf "Goto %d\n" line_number; 
      line_number 
    ) else (
      s + 1 
    )
    
    
  | End -> 
      Printf.printf "Program terminated.\n"; 
      exit 0 




      let eval_program program =
        let program_map = List.to_seq program |> Hashtbl.of_seq in
      
        let rec exec pc =
          match Hashtbl.find_opt program_map pc with
          | None -> Printf.printf "Ligne %d inexistante, fin du programme.\n" pc
          | Some instr ->
            Printf.printf "Exécution de la ligne %d :\n" pc;
            let next_pc = eval_instr pc instr in
            exec next_pc
        in
        exec 1  (* On commence à la ligne 1 *)
      
