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

and operatorML = MLadd| MLsub| MLmult| MLdiv| MLlt| MLgt| MLeq| MLleq| MLgeq 

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

(* VM CAM *)
type stackelem = Val of value | Code of coms
type stack = stackelem list
type config = value ref * coms * stack


(* Type des commandes CAM aplaties *)
type flat_com =
  | FQuote of value
  | FOpc of operator
  | FCdr
  | FCar
  | FCons
  | FPush
  | FSwap
  | FApp
  | FRplac
  | FCur of int
  | FBranch of int * int
