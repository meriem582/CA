open Ast

let rec compile (env : ident list) (e : expr) : coms =
  match e with
  | Number n -> [Quote (Int n)]     (* Règle (2) *)
  | True -> [Quote (Bool true)]     (* Règle (3) *)
  | False -> [Quote (Bool false)]   (* Règle (4) *)
  | Ident x -> trans_const x env    (* Règle (5) et (11) *)
  | If (e1, e2, e3) ->              (* Règle (6) *)
      let c1 = compile env e1 in
      let c2 = compile env e2 in
      let c3 = compile env e3 in
      [Push] @ c1 @ [Branch (c2, c3)]
  | Mlpair (e1, e2) ->              (* Règle (7) *)
      let c1 = compile env e1 in
      let c2 = compile env e2 in
      [Push] @ c1 @ [Swap] @ c2 @ [Cons]
  | Let (p, e1, e2) ->              (* Règle (8) *)
      let c1 = compile env e1 in
      let c2 = compile (extend_env p env) e2 in
      [Push] @ c1 @ [Cons] @ c2
  | LetRec (p, e1, e2) ->           (* Règle (9) *)
      let env' = extend_env p env in
      let c1 = compile env' e1 in
      let c2 = compile env' e2 in
      (match p with
        | IdentPat x -> [Push; Quote (Int (access_path env' x)); Cons; Push] @ c1 @ [Swap; Rplac] @ c2
        | _ -> failwith "LetRec only supports IdentPat")
  | Lambda (p, e) ->                (* Règle (10) *)
      let env' = extend_env p env in
      let c = compile env' e in
      [Cur c]
  | Apply (e1, e2) ->               (* Règle (12) *)
      let c1 = compile env e1 in
      let c2 = compile env e2 in
      [Push] @ c1 @ [Swap] @ c2 @ [Cons; App]

and trans_const (x : ident) (env : ident list) : coms =
  match x with
  | "fst" -> [Car]    (* projection première composante *)
  | "snd" -> [Cdr]    (* projection deuxième composante *)
  | "+" -> [Op Add]
  | "-" -> [Op Sub]
  | "*" -> [Op Mult]
  | "/" -> [Op Div]
  | "<" -> [Op Lt]
  | ">" -> [Op Gt]
  | "=" -> [Op Eq]
  | "<=" -> [Op Leq]
  | ">=" -> [Op Geq]
  | "==" -> [Op EqEq]
  | _ -> access env x

and access (env : ident list) (x : ident) : coms =
  match env with
  | [] -> failwith ("Unbound identifier: " ^ x)
  | y :: ys -> if x = y then [] else [Car] @ access ys x

and extend_env (p : pat) (env : ident list) : ident list =
  match p with
  | IdentPat x -> x :: env
  | Pairpat (p1, p2) -> extend_env p1 (extend_env p2 env)
  | NullPat -> env

and access_path (env : ident list) (x : ident) : int =
  let rec aux env x n =
    match env with
    | [] -> failwith ("Unbound identifier: " ^ x)
    | y :: ys -> if x = y then n else aux ys x (n + 1)
  in aux env x 0
