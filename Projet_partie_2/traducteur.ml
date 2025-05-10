open Ast


let flatten (code : com list) : flat_com list =
  let result = ref [] in
  let label = ref 0 in

  let add (com : flat_com) =
    result := !result @ [com];
    incr label
  in

  let rec flatten_coms coms =
    List.iter flatten_com coms

  and flatten_com = function
    | Quote v -> add (FQuote v)
    | Opc op -> add (FOpc op)
    | Car -> add FCar
    | Cdr -> add FCdr
    | Cons -> add FCons
    | Push -> add FPush
    | Swap -> add FSwap
    | App -> add FApp
    | Rplac -> add FRplac

    | Cur body ->
        let cur_index = !label in
        (* On réserve la place de `FCur i` *)
        add (FCur (-1)); (* placeholder *)
        let body_index = !label in
        flatten_coms body;
        (* Remplace le placeholder par l'indice réel *)
        let updated =
          List.mapi (fun i instr ->
            if i = cur_index then FCur body_index else instr
          ) !result
        in
        result := updated;
        label := List.length updated

    | Branch (b1, b2) ->
        let branch_index = !label in
        add (FBranch (-1, -1)); (* placeholder *)
        let i1 = !label in
        flatten_coms b1;
        let i2 = !label in
        flatten_coms b2;
        let updated =
          List.mapi (fun i instr ->
            if i = branch_index then FBranch (i1, i2) else instr
          ) !result
        in
        result := updated;
        label := List.length updated
  in

  flatten_coms code;
  !result

let string_of_flat_com = function
  | FQuote (Int n) -> Printf.sprintf "quote_int %d" n
  | FQuote (Bool b) -> Printf.sprintf "quote_bool %b" b
  | FQuote Nullvalue -> "quote_null()"
  | FQuote _ -> "quote <complex>"
  | FOpc Add -> "add()"
  | FOpc Sub -> "sub()"
  | FOpc Mult -> "mult()"
  | FOpc Div -> "div()"
  | FOpc Eq -> "eq()"
  | FOpc Lt -> "lt()"
  | FOpc Gt -> "gt()"
  | FOpc Leq -> "leq()"
  | FOpc Geq -> "geq()"
  | FCdr -> "cdr()"
  | FCar -> "car()"
  | FCons -> "cons()"
  | FPush -> "push()"
  | FSwap -> "swap()"
  | FApp -> "app()"
  | FRplac -> "rplac()"
  | FCur i -> Printf.sprintf "cur %d" i
  | FBranch (i1, i2) -> Printf.sprintf "branch %d %d" i1 i2
