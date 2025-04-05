(* from a 1986 paper: https://www.cs.tufts.edu/~nr/cs257/archive/dominique-clement/applicative.pdf *)

(* figure 1 page 14 (Abstract Syntax of Mini-ML) *)
type ident = string

type pat =
| Pairpat of pat * pat
| IdentPat of ident
| NullPat

type expr =
  | Ident of ident
  | Number of int
  | False
  | True
  | Apply of expr * expr
  | Mlpair of expr * expr
  | Lambda of pat * expr
  | Let of pat * expr * expr
  | LetRec of pat * expr * expr
  | If of expr * expr * expr


(* figure 7 page 21 (Abstract syntax of CAM code) *)

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
  

and operator = Add | Sub | Mult | Div | Lt | Gt | Eq | Leq | Geq | EqEq 
let rec string_of_expr = function
  | Ident s -> s
  | Number n -> string_of_int n
  | False -> "false"
  | True -> "true"
  | Apply (e1, e2) -> "(" ^ string_of_expr e1 ^ " " ^ string_of_expr e2 ^ ")"
  | Mlpair (e1, e2) -> "(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | Lambda (p, e) -> "(fun " ^ string_of_pat p ^ " -> " ^ string_of_expr e ^ ")"
  | Let (p, e1, e2) -> "(let " ^ string_of_pat p ^ " = " ^ string_of_expr e1 ^ " in " ^ string_of_expr e2 ^ ")"
  | LetRec (p, e1, e2) -> "(let rec " ^ string_of_pat p ^ " = " ^ string_of_expr e1 ^ " in " ^ string_of_expr e2 ^ ")"
  | If (e1, e2, e3) -> "(if " ^ string_of_expr e1 ^ " then " ^ string_of_expr e2 ^ " else " ^ string_of_expr e3 ^ ")"

and string_of_pat = function
  | IdentPat s -> s
  | NullPat -> "()"
  | Pairpat (p1, p2) -> "(" ^ string_of_pat p1 ^ ", " ^ string_of_pat p2 ^ ")"

let rec string_of_com = function
  | Quote v -> "Quote " ^ (match v with 
      | Int n -> string_of_int n 
      | Bool b -> string_of_bool b 
      | NullValue -> "null")
  | Op op -> "Op " ^ (match op with Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/" | Lt -> "<" | Gt -> ">" | Eq -> "=" | Leq -> "<=" | Geq -> ">=" | EqEq -> "==")
  | Car -> "Car"
  | Cdr -> "Cdr"
  | Cons -> "Cons"
  | Push -> "Push"
  | Swap -> "Swap"
  | App -> "App"
  | Rplac -> "Rplac"
  | Cur coms -> "Cur (" ^ String.concat "; " (List.map string_of_com coms) ^ ")"
  | Branch (c1, c2) -> "Branch (" ^ String.concat "; " (List.map string_of_com c1) ^ ", " ^ String.concat "; " (List.map string_of_com c2) ^ ")"
