(* AST Mini_ml *)
type ident = string

type pat =
  | Pairpat of pat * pat
  | Identpat of ident
  | Nullpat

type expr =
  | Ident of ident
  | Number of int
  | Bool of bool
  | If of expr * expr * expr
  | Apply of expr * expr
  | MLpair of expr * expr
  | Lambda of pat * expr
  | MLin of dec * expr
  | Op of operatorML
  | MLfst 
  | MLsnd

and operatorML =
  | MLadd 
  | MLsub
  | MLmult
  | MLdiv
  | MLlt
  | MLgt
  | MLeq
  | MLleq
  | MLgeq

and dec =
  | Let of pat * expr
  | Letrec of pat * expr

(* AST CAM *)
type com =
  | Quote of value
  | Opc of operator
  | Cdr
  | Car
  | Cons
  | Push
  | Swap
  | Return
  | App
  | Rplac
  | Cur of coms
  | Branch of coms * coms

and operator = Add | Sub | Mult | Div | Lt | Gt | Eq | Leq | Geq  


and value =
  | Int of int
  | Bool of bool
  | Nullvalue
  | Pair of value * value
  | Closure of coms * value

and coms = com list


type stackelem = Val of value | Code of coms
type stack = stackelem list
type config = value ref * coms * stack
(* VM CAM *)
let rec exec : config -> config = function
  | ({ contents = Pair(x, y) }, Cdr :: c, d) -> exec (ref x, c, d)
  | ({ contents = Pair(x, y) }, Car :: c, d) -> exec (ref y, c, d)
  | (x, Cons :: c, Val y :: d) -> exec (ref (Pair (y, !x)), c, d)
  | (x, Rplac :: c, Val (Pair (y, z) as u) :: d) -> ref z := !x; exec (ref u, c, d)
  | ({ contents = x }, Push :: c, d) -> exec (ref x, c, Val x :: d)
  | ({ contents = x }, Swap :: c, Val y :: d) -> exec (ref y, c, Val x :: d)
  | (t, Quote v :: c, d) -> exec (ref v, c, d)
  | ({ contents = Pair (Closure (x, y), z) }, App :: c, d) -> exec (ref (Pair (y, z)), x, Code c :: d)
  | ({ contents = Bool b }, Branch (c1, c2) :: c, Val x :: d) -> exec (ref x, (if b then c1 else c2), Code c :: d)
  | ({ contents = Pair (Int m, Int n) }, Opc Add :: c, d) -> exec (ref (Int (m + n)), c, d)
  | ({ contents = Pair (Int m, Int n) }, Opc Eq :: c, d) -> exec (ref (Bool (m = n)), c, d)
  | ({ contents = x }, Cur c1 :: c, d) -> exec (ref (Closure (c1, x)), c, d)
  | ({ contents = x }, Return :: c, Code c' :: d) -> exec (ref x, c', d)
  | config -> config

let exec_code cs = exec (ref Nullvalue, cs, [])

(* Compilation Mini-ML -> CAM *)

let rec access id = function
  | Nullpat -> failwith "nullpat"
  | Identpat x -> if x = id then [] else failwith "not found"
  | Pairpat (p1, p2) ->
    (try Car :: access id p2 with Failure _ -> Cdr :: access id p1)

let rec compile pat = function
  | Number n -> [Quote (Int n)]
  | Bool b -> [Quote (Bool b)]
  | Ident v -> access v pat
  | If (e1, e2, e3) ->
    [Push] @ compile pat e1 @
    [Branch (compile pat e2 @ [Return], compile pat e3 @ [Return])]
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
    [Cur (compile newpat e @ [Return])]
  | Apply (e1, e2) ->
    if is_constant e1 then compile pat e2 @ trans_constant e1
    else [Push] @ compile pat e1 @ [Swap] @ compile pat e2 @ [Cons; App]
  | e when is_constant e -> [Cur (Car :: trans_constant e)]
  | _ -> failwith "compile"

and is_constant = function
  | Op MLadd | Op MLeq
  | Op MLgeq
   | MLfst | MLsnd -> true
  | _ -> false

and trans_constant = function
  | Op MLadd -> [Opc Add]
  | Op MLsub -> [Opc Sub]
  | Op MLmult -> [Opc Mult]
  | Op MLdiv -> [Opc Div]
  | Op MLlt -> [Opc Lt]
  | Op MLgt -> [Opc Gt]
  | Op MLeq -> [Opc Eq]
  | Op MLleq -> [Opc Leq]
  | Op MLgeq ->  [Opc Geq]
  | MLfst -> [Cdr]
  | MLsnd -> [Car]
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
    | Return -> "Return"
    | App -> "App"
    | Cur cs -> "Cur [" ^ (String.concat "; " (List.map string_of_com cs)) ^ "]"
    | Branch (c1, c2) ->
      "Branch [" ^ (String.concat "; " (List.map string_of_com c1)) ^ "] [" ^
      (String.concat "; " (List.map string_of_com c2)) ^ "]"
  in
  let code_str = "[" ^ String.concat "; " (List.map string_of_com cam_code) ^ "]" in
  print_endline code_str;
  print_endline "=== Résultat ===";
  exec_code cam_code

(* Affichage *)
let rec string_of_value = function
  | Nullvalue -> "null"
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | Pair (v1, v2) -> "(" ^ string_of_value v1 ^ ", " ^ string_of_value v2 ^ ")"
  | Closure _ -> "<closure>"

(* Exemple Mini-ML à tester *)
let example =
  MLin(Let(Identpat "x", Number 5),
       MLin(Let(Identpat "z",
                Lambda(Identpat "y",
                       Apply(Op MLadd, MLpair(Ident "y", Ident "x")))),
            MLin(Let(Identpat "x", Number 1),
                 MLin(Let(Identpat "r", Apply(Ident "z", Ident "x")),
                      Apply(Op MLadd, MLpair(Ident "r", Ident "r"))))))

let () =
  let (v, _, _) = run example in
  Printf.printf "Résultat final : %s\n" (string_of_value !v)
