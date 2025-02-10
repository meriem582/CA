open Ast

module StringMap = Map.Make(String)
type env = int StringMap.t

type value =
  | VInt of int
  | VFloat of float
  | VString of string

let string_of_value = function
  | VInt(i) -> string_of_int i
  | VFloat(f) -> string_of_float f
  | VString(s) -> s

let rec eval_expr e =
  match e with
  | Integer(x) -> VInt(x)
  | Float(f) -> VFloat(f)
  | String(s) -> VString(s)
  | Add(left, right) -> (
      match eval_expr left, eval_expr right with
      | VInt(l), VInt(r) -> VInt(l + r)
      | VFloat(l), VFloat(r) -> VFloat(l +. r)
      | VString(l), VString(r) -> VString(l ^ r)
      | _ -> failwith "Type mismatch in Add"
    )
  | Sub(left, right) -> (
      match eval_expr left, eval_expr right with
      | VInt(l), VInt(r) -> VInt(l - r)
      | VFloat(l), VFloat(r) -> VFloat(l -. r)
      | _ -> failwith "Type mismatch in Sub"
    )
  | Mul(left, right) -> (
      match eval_expr left, eval_expr right with
      | VInt(l), VInt(r) -> VInt(l * r)
      | VFloat(l), VFloat(r) -> VFloat(l *. r)
      | _ -> failwith "Type mismatch in Mul"
    )
  | Div(left, right) -> (
      match eval_expr left, eval_expr right with
      | VInt(l), VInt(r) -> VInt(l / r)
      | VFloat(l), VFloat(r) -> VFloat(l /. r)
      | _ -> failwith "Type mismatch in Div"
    )
  | Neg(x) -> (
      match eval_expr x with
      | VInt(v) -> VInt(-v)
      | VFloat(v) -> VFloat(-.v)
      | _ -> failwith "Type mismatch in Neg"
    )
  | Pos(x) -> eval_expr x
  | Eq(left, right) -> (
      match eval_expr left, eval_expr right with
      | VInt(l), VInt(r) -> VInt(if l = r then 1 else 0)
      | VFloat(l), VFloat(r) -> VInt(if l = r then 1 else 0)
      | VString(l), VString(r) -> VInt(if l = r then 1 else 0)
      | _ -> failwith "Type mismatch in Eq"
    )
  | And(left, right) -> (
    match (eval_expr left, eval_expr right) with
    | (VInt(l), VInt(r)) -> if l <> 0 && r <> 0 then VInt(1) else VInt(0)
    | (VFloat(l), VFloat(r)) -> if l <> 0.0 && r <> 0.0 then VInt(1) else VInt(0)
    | _ -> failwith "Erreur : AND n'est pas défini pour ces types"
  )
| Or(left, right) -> (
    match (eval_expr left, eval_expr right) with
    | (VInt(l), VInt(r)) -> if l <> 0 || r <> 0 then VInt(1) else VInt(0)
    | (VFloat(l), VFloat(r)) -> if l <> 0.0 || r <> 0.0 then VInt(1) else VInt(0)
    | _ -> failwith "Erreur : OR n'est pas défini pour ces types"
  )
| Not(x) -> (
    match eval_expr x with
    | VInt(v) -> if v = 0 then VInt(1) else VInt(0)
    | VFloat(v) -> if v = 0.0 then VInt(1) else VInt(0)
    | _ -> failwith "Erreur : NOT n'est pas défini pour ce type"
  )
| Lt(left, right) -> (
    match (eval_expr left, eval_expr right) with
    | (VInt(l), VInt(r)) -> if l < r then VInt(1) else VInt(0)
    | (VFloat(l), VFloat(r)) -> if l < r then VInt(1) else VInt(0)
    | (VString(l), VString(r)) -> if l < r then VInt(1) else VInt(0)
    | _ -> failwith "Erreur : LT n'est pas défini pour ces types"
  )
| Gt(left, right) -> (
    match (eval_expr left, eval_expr right) with
    | (VInt(l), VInt(r)) -> if l > r then VInt(1) else VInt(0)
    | (VFloat(l), VFloat(r)) -> if l > r then VInt(1) else VInt(0)
    | (VString(l), VString(r)) -> if l > r then VInt(1) else VInt(0)
    | _ -> failwith "Erreur : GT n'est pas défini pour ces types"
  )


(* Ajout de la pile pour gérer les retours des sous-programmes *)
let rec eval_instr s stack i =
  match i with
  | Print(l) -> 
      List.iter (fun x -> print_string (string_of_value (eval_expr x)) ; print_string " ") l;
      print_newline();
      (s + 1, stack)
  | Rem(_) -> 
      print_newline(); 
      (s + 1, stack)
  | Let(id, e) -> 
      let value = eval_expr e in 
      Printf.printf "%s = %s\n" id (string_of_value value);
      (s + 1, stack)
  | Input(id) -> 
    Printf.printf "Veuillez saisir une valeur pour %s: " id; 
    let input = read_line () in
    let value =
      try VInt (int_of_string input)  (* Essayer de convertir en entier *)
      with Failure _ ->
        try VFloat (float_of_string input)  (* Sinon, essayer en flottant *)
        with Failure _ -> VString input  (* Sinon, traiter comme chaîne *)
    in
    Printf.printf "%s = %s\n" id (string_of_value value);
    (s + 1, stack)
  | Goto(line_number) -> 
      Printf.printf "Goto %d\n" line_number; 
      (line_number, stack)
  | If(e, line_number) -> 
    (match eval_expr e with
    | VInt(1) -> 
        Printf.printf "Goto %d\n" line_number; 
        (line_number, stack)
    | VInt(_) -> 
        (s + 1, stack)
    | _ -> 
        failwith "Erreur : IF n'est pas défini pour ce type")

  | Gosub(line_number) -> 
      Printf.printf "Gosub %d (retournera à %d)\n" line_number (s + 1);
      (line_number, (s + 1) :: stack)  (* Empiler la ligne de retour *)
  | Return -> 
      (match stack with
      | [] -> 
          Printf.printf "Erreur : Return sans Gosub.\n";
          (s + 1, stack)
      | return_address :: rest -> 
          Printf.printf "Retour à la ligne %d\n" return_address;
          (return_address, rest))
  | End -> 
      Printf.printf "Program terminated.\n"; 
      exit 0

(* Fonction principale pour évaluer le programme *)
let eval_program program =
  let program_map = List.to_seq program |> Hashtbl.of_seq in

  let rec exec pc stack =
    match Hashtbl.find_opt program_map pc with
    | None -> Printf.printf "Ligne %d inexistante, fin du programme.\n" pc
    | Some instr ->
        Printf.printf "Exécution de la ligne %d :\n" pc;
        let (next_pc, new_stack) = eval_instr pc stack instr in
        exec next_pc new_stack
  in
  exec 1 []  (* Commence à la ligne 1 avec une pile vide *)
