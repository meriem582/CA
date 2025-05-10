(* run_interp.ecl *)

(* Définition des types de valeurs *)
type value =
   Int of int
  | Bool of bool
  | Null
  | Closure of int * value array<1024>

(* Définition des instructions *)
type instr =
   Push
  | Quote of value
  | Cons
  | Car
  | Cdr
  | Swap
  | Rplac
  | Cur of int
  | Branch of int * int
  | App
  | Add
  | Sub
  | Mult
  | Div
  | Eq
  | Lt
  | Gt
  | Leq
  | Geq

(* Pile d'exécution *)
let stack = ref []

(* Environnement *)
let env = ref []

(* Pile de retour *)
let dump = ref []

(* Fonction pour exécuter l'interpréteur *)
let rec run_interp pc =
  match get(code, pc) with
  | Push ->
      env := !stack :: !env;
      run_interp (pc + 1)
  | Quote v ->
      stack := v :: !stack;
      run_interp (pc + 1)
  | Cons ->
      let v1 = List.hd !stack in
      stack := List.tl !stack;
      let v2 = List.hd !stack in
      stack := List.tl !stack;
      stack := (v1, v2) :: !stack;
      run_interp (pc + 1)
  | Car ->
      let (v1, _) = List.hd !stack in
      stack := List.tl !stack;
      stack := v1 :: !stack;
      run_interp (pc + 1)
  | Cdr ->
      let (_, v2) = List.hd !stack in
      stack := List.tl !stack;
      stack := v2 :: !stack;
      run_interp (pc + 1)
  | Swap ->
      let v1 = List.hd !stack in
      stack := List.tl !stack;
      let v2 = List.hd !stack in
      stack := List.tl !stack;
      stack := v1 :: v2 :: !stack;
      run_interp (pc + 1)
  | Rplac ->
      let v1 = List.hd !stack in
      stack := List.tl !stack;
      let v2 = List.hd !stack in
      stack := List.tl !stack;
      (* Remplacer la tête de v2 par v1 *)
      stack := (v1, snd v2) :: !stack;
      run_interp (pc + 1)
  | Cur i ->
      let closure = Closure (i, !env) in
      stack := closure :: !stack;
      run_interp (pc + 1)
  | Branch (i1, i2) ->
      let v = List.hd !stack in
      stack := List.tl !stack;
      (match v with
       | Bool true -> run_interp i1
       | Bool false -> run_interp i2
       | _ -> failwith "Invalid boolean value")
  | App ->
      let Closure (i, e) = List.hd !stack in
      stack := List.tl !stack;
      let arg = List.hd !stack in
      stack := List.tl !stack;
      dump := (pc + 1, !env, !stack) :: !dump;
      env := arg :: e;
      run_interp i
  | Add ->
      let Int v1 = List.hd !stack in
      stack := List.tl !stack;
      let Int v2 = List.hd !stack in
      stack := List.tl !stack;
      stack := Int (v1 + v2) :: !stack;
      run_interp (pc + 1)
  | Sub ->
      let Int v1 = List.hd !stack in
      stack := List.tl !stack;
      let Int v2 = List.hd !stack in
      stack := List.tl !stack;
      stack := Int (v1 - v2) :: !stack;
      run_interp (pc + 1)
  | Mult ->
      let Int v1 = List.hd !stack in
      stack := List.tl !stack;
      let Int v2 = List.hd !stack in
      stack := List.tl !stack;
      stack := Int (v1 * v2) :: !stack;
      run_interp (pc + 1)
  | Div ->
      let Int v1 = List.hd !stack in
      stack := List.tl !stack;
      let Int v2 = List.hd !stack in
      stack := List.tl !stack;
      stack := Int (v1 / v2) :: !stack;
      run_interp (pc + 1)
  | Eq ->
      let Int v1 = List.hd !stack in
      stack := List.tl !stack;
      let Int v2 = List.hd !stack in
      stack := List.tl !stack;
      stack := Bool (v1 = v2) :: !stack;
      run_interp (pc + 1)
  | Lt ->
      let Int v1 = List.hd !stack in
      stack := List.tl !stack;
      let Int v2 = List.hd !stack in
      stack := List.tl !stack;
      stack := Bool (v1 < v2) :: !stack;
      run_interp (pc + 1)
  | Gt ->
      let Int v1 = List.hd !stack in
      stack := List.tl !stack;
      let Int v2 = List.hd !stack in
      stack := List.tl !stack;
      stack := Bool (v1 > v2) :: !stack;
      run_interp (pc + 1)
  | Leq ->
      let Int v1 = List.hd !stack in
      stack := List.tl !stack;
      let Int v2 = List.hd !stack in
      stack := List.tl !stack;
      stack := Bool (v1 <= v2) :: !stack;
      run_interp (pc + 1)
  | Geq ->
      let Int v1 = List.hd !stack in
      stack := List.tl !stack;
      let Int v2 = List.hd !stack in
      stack := List.tl !stack;
      stack := Bool (v1 >= v2) :: !stack;
      run_interp (pc + 1)
