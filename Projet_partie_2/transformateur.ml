open Ast

(* Compilation Mini-ML -> CAM *)

let rec access id = function
  | Nullpat -> failwith "nullpat"
  | Identpat x -> if x = id then [] else failwith "not found"
  | Pairpat (p1, p2) ->
    (try Cdr :: access id p2 with Failure _ -> Car :: access id p1)

let rec compile pat = function
  | Number n -> [Quote (Int n)]
  | Bool b -> [Quote (Bool b)]
  | Ident v -> access v pat
  | If (e1, e2, e3) ->
    [Push] @ compile pat e1 @
    [Branch (compile pat e2 , compile pat e3)]
  | MLpair (e1, e2) ->
    [Push] @ compile pat e1 @ [Swap] @ compile pat e2 @ [Cons]
  | MLin (Let (p, e1), e2) ->
    let newpat = Pairpat (pat, p) in
    [Push] @ compile pat e1 @ [Cons] @ compile newpat e2
  | MLin (Letrec (p, e1), e2) ->
    let newpat = Pairpat (pat, p) in
    [Push; Quote Nullvalue; Cons; Push] @ compile newpat e1 @
    [Swap; Rplac] @ compile newpat e2
  | Lambda (p, e) ->
    let newpat = Pairpat (pat, p) in
    [Cur (compile newpat e )]
  | Apply (e1, e2) ->
    if is_constant e1 then compile pat e2 @ trans_constant e1
    else [Push] @ compile pat e1 @ [Swap] @ compile pat e2 @ [Cons; App]
  | e when is_constant e -> [Cur (Cdr :: trans_constant e)]
  | _ -> failwith "compile"

and is_constant = function
  | Op MLadd 
  | Op MLsub
  | Op MLmult
  | Op MLdiv
  | Op MLlt
  | Op MLgt
  | Op MLeq
  | Op MLleq
  | Op MLeqeq
  | Op MLgeq
  | MLfst 
  | MLsnd -> true
  | _ -> false

and trans_constant = function
  | Op MLadd -> [Opc Add]
  | Op MLsub -> [Opc Sub]
  | Op MLmult -> [Opc Mult]
  | Op MLdiv -> [Opc Div]
  | Op MLlt -> [Opc Lt]
  | Op MLgt -> [Opc Gt]
  | Op MLeq -> [Opc Eq]
  | Op MLeqeq -> [Opc Eqeq]
  | Op MLleq -> [Opc Leq]
  | Op MLgeq ->  [Opc Geq]
  | MLfst -> [Car]
  | MLsnd -> [Cdr]
  | _ -> failwith "trans_constant"

let run e =
  let cam_code = compile Nullpat e in
  print_endline "=== Code CAM généré ===";
  let rec string_of_com = function
    | Quote (Int n) -> Printf.sprintf "Quote %d" n
    | Quote (Bool b) -> Printf.sprintf "Quote %b" b
    | Quote Nullvalue -> "Quote Null"
    | Quote _ -> "Quote <complex>"
    | Opc Add -> "Add"
    | Opc Eq -> "Eq"
    | Opc Eqeq -> "Eqeq"
    | Opc Sub -> "Sub"
    | Opc Mult -> "Mult"
    | Opc Div -> "Div"
    | Opc Lt -> "Lt"
    | Opc Gt -> "Gt"
    | Opc Leq -> "Leq"
    | Opc Geq -> "Geq"
    | Cdr -> "Cdr"
    | Car -> "Car"
    | Cons -> "Cons"
    | Rplac -> "Rplac"
    | Push -> "Push"
    | Swap -> "Swap"
    | App -> "App"
    | Cur cs -> "Cur [" ^ (String.concat "; " (List.map string_of_com cs)) ^ "]"
    | Branch (c1, c2) ->
      "Branch [" ^ (String.concat "; " (List.map string_of_com c1)) ^ "] [" ^
      (String.concat "; " (List.map string_of_com c2)) ^ "]"
  in
  let code_str = "[" ^ String.concat "; " (List.map string_of_com cam_code) ^ "]" in
  print_endline code_str;
  cam_code

(* Affichage *)
let rec string_of_value = function
  | Nullvalue -> "null"
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | Pair (v1, v2) -> "(" ^ string_of_value v1 ^ ", " ^ string_of_value v2 ^ ")"
  | Closure _ -> "<closure>"

  