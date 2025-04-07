type program = coms
and coms = com list
and com =
  | Quote of value
  | Op of operator
  | Car
  | Cdr
  | Cons
  | Push
  | Swap
  | App
  | Rplac
  | Cur of coms
  | Branch of coms * coms

and value =
  | Int of int
  | Bool of bool
  | NullValue
  | Pair of value * value
  | Closure of coms * env
  
and operator = Add | Sub | Mult | Div | Lt | Gt | Eq | Leq | Geq | EqEq 

and stack = value list
and env = value list
and dump = (coms * env) list
type state = coms * stack * env * dump


(* ===== Évaluation des opérateurs ===== *)

let eval_op op v1 v2 =
  match (op, v1, v2) with
  | (Add, Int a, Int b) -> Int (a + b)
  | (Sub, Int a, Int b) -> Int (a - b)
  | (Mult, Int a, Int b) -> Int (a * b)
  | (Div, Int a, Int b) -> Int (a / b)
  | (Lt, Int a, Int b) -> Bool (a < b)
  | (Gt, Int a, Int b) -> Bool (a > b)
  | (Eq, Int a, Int b) -> Bool (a = b)
  | (Leq, Int a, Int b) -> Bool (a <= b)
  | (Geq, Int a, Int b) -> Bool (a >= b)
  | (EqEq, v1, v2) -> Bool (v1 = v2)
  | _ -> failwith "Invalid operands for operator"


(* ===== Exécution d'une étape CAM ===== *)

let rec step (c, s, e, d) : state =
  match c with
  | [] -> ([], s, e, d)
  | instr :: rest ->
    match instr with
    | Quote v -> (rest, v :: s, e, d)

    | Op op ->
      begin match s with
      | v2 :: v1 :: s' ->
        let result = eval_op op v1 v2 in
        (rest, result :: s', e, d)
      | _ -> failwith "Stack underflow on Op"
      end

    | Car ->
      begin match s with
      | Pair (a, _) :: s' -> (rest, a :: s', e, d)
      | _ -> failwith "Expected pair for Car"
      end

    | Cdr ->
      begin match s with
      | Pair (_, b) :: s' -> (rest, b :: s', e, d)
      | _ -> failwith "Expected pair for Cdr"
      end

    | Cons ->
      begin match s with
      | v2 :: v1 :: s' -> (rest, Pair (v1, v2) :: s', e, d)
      | _ -> failwith "Stack underflow on Cons"
      end

    | Push -> (rest, s, e, (rest, e) :: d)

    | Swap ->
      begin match s with
      | a :: b :: s' -> (rest, b :: a :: s', e, d)
      | _ -> failwith "Stack underflow on Swap"
      end

    | Cur body -> (rest, Closure (body, e) :: s, e, d)

    | App ->
      begin match s with
      | arg :: Closure (body, clo_env) :: s' ->
        (* push return context *)
        (body, [], arg :: clo_env, (rest, e) :: d)
      | _ -> failwith "Invalid App application"
      end

    | Rplac ->
      begin match s with
      | v :: Pair (_, b) :: s' -> (rest, Pair (v, b) :: s', e, d)
      | _ -> failwith "Invalid Rplac"
      end

    | Branch (ct, cf) ->
      begin match s with
      | Bool true :: s' -> (ct @ rest, s', e, d)
      | Bool false :: s' -> (cf @ rest, s', e, d)
      | _ -> failwith "Expected boolean for Branch"
      end


(* ===== Boucle principale d'exécution ===== *)

let rec run (state : state) : value =
  match state with
  | ([], v :: _, _, _) -> v  (* Résultat final *)
  | ([], [], _, (c', e') :: d') -> run (c', [], e', d')  (* Retour de fonction *)
  | state' -> run (step state')


(* ===== Interface : exécution d’un programme CAM ===== *)

let eval (program : coms) : value =
  run (program, [], [], [])


(* ===== Exemple d’utilisation ===== *)

let example_program = [Push; Quote (Int 0);
Cons;
Push;
Cur (Push; 
Push; Cdr; 
Swap; Quote (Int 0);
Cons; Op EqEq;
Branch (Quote (Int 1),
Push; Cdr; 
Swap; Push; Car; Cdr; 
Swap; Push; Cdr; 
Swap; Quote (Int 1);
Cons; Op Sub; 
Cons; App;
Cons; Op Mult));
Swap; Rplac;
Push; Cdr;
Swap; Quote (Int 4);
Cons; App]


let () =
  match eval example_program with
  | Int n -> Printf.printf "Résultat: %d\n" n
  | Bool b -> Printf.printf "Résultat booléen: %b\n" b
  | NullValue -> print_endline "Résultat: Null"
  | _ -> print_endline "Résultat inattendu"
