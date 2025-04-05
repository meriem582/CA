open Ast

type env = value list
type stack = value list

let rec eval (prog : coms) (stack : stack) (env : env) : value =
  match prog with
  | [] ->
      (match stack with
      | v :: _ -> v
      | _ -> failwith "Empty stack at end of evaluation")

  | Quote v :: rest ->
      eval rest (v :: stack) env

  | Op op :: rest ->
      (match stack with
      | Int n2 :: Int n1 :: st ->
          let result =
            match op with
            | Add -> Int (n1 + n2)
            | Sub -> Int (n1 - n2)
            | Mult -> Int (n1 * n2)
            | Div -> Int (n1 / n2)
            | Lt -> Bool (n1 < n2)
            | Gt -> Bool (n1 > n2)
            | Eq -> Bool (n1 = n2)
            | Leq -> Bool (n1 <= n2)
            | Geq -> Bool (n1 >= n2)
            | EqEq -> Bool (n1 = n2)
          in
          eval rest (result :: st) env
      | _ -> failwith "Operator expects two integers")

  | Car :: rest ->
      (match stack with
      | (Mlpair (v1, _)) :: st -> eval rest (v1 :: st) env
      | _ -> failwith "Car expects a pair")

  | Cdr :: rest ->
      (match stack with
      | (Mlpair (_, v2)) :: st -> eval rest (v2 :: st) env
      | _ -> failwith "Cdr expects a pair")

  | Cons :: rest ->
      (match stack with
      | v2 :: v1 :: st -> eval rest (Mlpair (v1, v2) :: st) env
      | _ -> failwith "Cons expects two values")

  | Push :: rest ->
      eval rest (NullValue :: stack) env

  | Swap :: rest ->
      (match stack with
      | a :: b :: st -> eval rest (b :: a :: st) env
      | _ -> failwith "Swap needs two elements")

  | Cur code :: rest ->
      eval rest (Closure (code, env) :: stack) env

  | App :: rest ->
      (match stack with
      | arg :: Closure (code, env') :: st ->
          eval code (arg :: st) (arg :: env') (* env' could be different *)
      | _ -> failwith "App expects closure and argument")

  | Branch (cthen, celse) :: rest ->
      (match stack with
      | Bool true :: st -> eval (cthen @ rest) st env
      | Bool false :: st -> eval (celse @ rest) st env
      | _ -> failwith "Branch expects a boolean condition")

  | Rplac :: rest ->
      (match stack with
      | newval :: _ :: st -> eval rest (newval :: st) env
      | _ -> failwith "Rplac needs two elements")