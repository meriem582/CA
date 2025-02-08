open Ast

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
  | Print(l) -> List.iter (fun x -> print_int (eval_expr x); print_string " " ) l; print_newline()
  | Rem(_) -> (); print_newline()
  | Let(id,e) -> let value = eval_expr e in Printf.printf "%s = %d\n" id value
  | Input(id) -> Printf.printf "veuillez saisir une valeur à %s: " id; let value = read_int() in Printf.printf "%s = %d\n" id value
  | Goto(i) -> Printf.printf "goto %d\n" i
  (*| End -> Printf.printf "End\n"; exec 0*)
  (* | End ->
    Printf.printf "Program terminated.\n";
    exit 0 *)
  (*| Goto (label) ->
    (* Rechercher l'instruction avec le label correspondant *)
    let target_pc = List.findi (fun i stmt ->
      match stmt with
      | Goto lbl -> lbl = label
      | _ -> false
    ) program in
    match target_pc with
    | Some pc -> execute_program program env pc
    | None -> failwith ("Label " ^ string_of_int label ^ " not found") *)

let eval_program program =
  let rec exec pc =
    match List.nth_opt program pc with
    | None -> ()
    | Some (s, instr) ->
      eval_instr s instr;
      exec (pc + 1)
  in
  exec 0