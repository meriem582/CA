type expr =
  | Add of expr * expr
  | Mul of expr * expr
  | Sub of expr * expr
  | Div of expr * expr 
  | Integer of int
  | Neg of expr
  | Pos of expr
  | And of expr * expr
  | Or of expr * expr
  | Not of expr
  | Lt of expr * expr
  | Gt of expr * expr
  | Eq of expr * expr

type instr  =
  | Print of expr list
  | Rem of string
  | Let of string * expr
  | Input of string
  | Goto of int
  | End
  | If of expr * int
  | Gosub of int
  | Return

type line = int * instr

type program = line list