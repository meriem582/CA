exception LuaBytecodeExpected
open Bytes
open Printf
open Int64

(* Définition du type énuméré InstructionType *)
type instruction_type =
  | ABC
  | ABx
  | AsBx

(* Fonction pour convertir un entier en instruction *)
let int_to_instruction = function
  | 0 -> ABC
  | 1 -> ABx
  | 2 -> AsBx
  | _ -> invalid_arg "Instruction inconnue"

(* Définition du type énuméré opcode *)
type opcode =
  | MOVE | LOADK | LOADBOOL | LOADNIL | GETUPVAL | GETGLOBAL | GETTABLE
  | SETGLOBAL | SETUPVAL | SETTABLE | NEWTABLE | SELF | ADD | SUB | MUL
  | DIV | MOD | POW | UNM | NOT | LEN | CONCAT | JMP | EQ | LT | LE
  | TEST | TESTSET | CALL | TAILCALL | RETURN | FORLOOP | FORPREP
  | TFORLOOP | SETLIST | CLOSE | CLOSURE | VARARG

(* Fonction pour convertir un entier en opcode *)
let int_to_opcode = function
  | 0  -> MOVE
  | 1  -> LOADK
  | 2  -> LOADBOOL
  | 3  -> LOADNIL
  | 4  -> GETUPVAL
  | 5  -> GETGLOBAL
  | 6  -> GETTABLE
  | 7  -> SETGLOBAL
  | 8  -> SETUPVAL
  | 9  -> SETTABLE
  | 10 -> NEWTABLE
  | 11 -> SELF
  | 12 -> ADD
  | 13 -> SUB
  | 14 -> MUL
  | 15 -> DIV
  | 16 -> MOD
  | 17 -> POW
  | 18 -> UNM
  | 19 -> NOT
  | 20 -> LEN
  | 21 -> CONCAT
  | 22 -> JMP
  | 23 -> EQ
  | 24 -> LT
  | 25 -> LE
  | 26 -> TEST
  | 27 -> TESTSET
  | 28 -> CALL
  | 29 -> TAILCALL
  | 30 -> RETURN
  | 31 -> FORLOOP
  | 32 -> FORPREP
  | 33 -> TFORLOOP
  | 34 -> SETLIST
  | 35 -> CLOSE
  | 36 -> CLOSURE
  | 37 -> VARARG
  | _  -> invalid_arg "Opcode inconnu"

(* Fonction pour afficher un opcode sous forme de chaîne *)
let opcode_to_string = function
  | MOVE      -> "MOVE"
  | LOADK     -> "LOADK"
  | LOADBOOL  -> "LOADBOOL"
  | LOADNIL   -> "LOADNIL"
  | GETUPVAL  -> "GETUPVAL"
  | GETGLOBAL -> "GETGLOBAL"
  | GETTABLE  -> "GETTABLE"
  | SETGLOBAL -> "SETGLOBAL"
  | SETUPVAL  -> "SETUPVAL"
  | SETTABLE  -> "SETTABLE"
  | NEWTABLE  -> "NEWTABLE"
  | SELF      -> "SELF"
  | ADD       -> "ADD"
  | SUB       -> "SUB"
  | MUL       -> "MUL"
  | DIV       -> "DIV"
  | MOD       -> "MOD"
  | POW       -> "POW"
  | UNM       -> "UNM"
  | NOT       -> "NOT"
  | LEN       -> "LEN"
  | CONCAT    -> "CONCAT"
  | JMP       -> "JMP"
  | EQ        -> "EQ"
  | LT        -> "LT"
  | LE        -> "LE"
  | TEST      -> "TEST"
  | TESTSET   -> "TESTSET"
  | CALL      -> "CALL"
  | TAILCALL  -> "TAILCALL"
  | RETURN    -> "RETURN"
  | FORLOOP   -> "FORLOOP"
  | FORPREP   -> "FORPREP"
  | TFORLOOP  -> "TFORLOOP"
  | SETLIST   -> "SETLIST"
  | CLOSE     -> "CLOSE"
  | CLOSURE   -> "CLOSURE"
  | VARARG    -> "VARARG"

(* Définition du type énuméré ConstType *)
type const_type =
  | NIL
  | BOOL
  | NUMBER
  | STRING

(* Fonction pour convertir un entier en ConstType *)
let int_to_const_type = function
  | 0 -> NIL
  | 1 -> BOOL
  | 3 -> NUMBER
  | 4 -> STRING
  | _ -> invalid_arg "ConstType inconnu"

(* Fonction pour convertir un ConstType en chaîne de caractères *)
let const_type_to_string = function
  | NIL -> "NIL"
  | BOOL -> "BOOL"
  | NUMBER -> "NUMBER"
  | STRING -> "STRING"

(* Définition des listes de types d'opcodes spécifiques *)
let _RKBCInstr = 
  [9; 12; 13; 14; 15; 16; 17; 23; 24]  (* Correspond à SETTABLE, ADD, SUB, MUL, DIV, MOD, POW, EQ, LT *)
let _RKCInstr = 
  [6; 11]  (* Correspond à GETTABLE, SELF *)
let _KBx = 
  [1; 5; 7]  (* Correspond à LOADK, GETGLOBAL, SETGLOBAL *)

(*_LUAMAGIC est la signature de l'entête du bytecode Lua*)
let _LUAMAGIC = 
  "\x1bLua" 

(* Définition de la fonction whichRK qui vérifie si un bit est un registre ou une constante *)
let which_rk (rk: int) : bool =
  (rk land (1 lsl 8)) > 0

(* Définition de la fonction readRKasK qui lit un registre ou une constante *)
let read_rk_as_k (rk: int) : int =
  rk land (lnot (1 lsl 8))  

(* Définition du type instruction *)
type instruction = {
  instr_type : instruction_type;  (* Type d'instruction : ABC, ABx, AsBx *)
  name_instr : string;                  (* Nom de l'instruction *)
  mutable opcode : int option;    (* Opcode associé, mutable car modifiable *)
  mutable a : int option;         (* Champ A (optionnel) *)
  mutable b : int option;         (* Champ B (optionnel) *)
  mutable c : int option;         (* Champ C (optionnel) *)
}

(* Constructeur pour créer une nouvelle instruction (vu comme constructeur) *)
let create_instruction t name_instr = {
  instr_type = t;
  name_instr = name_instr;
  opcode = None; 
  a = None;
  b = None;
  c = None;
}

(* Formater RK : registre ou constante *)
let format_rk rk =
  if which_rk rk then Printf.sprintf "K[%d]" (read_rk_as_k rk)
  else Printf.sprintf "R[%d]" rk

(* Fonction pour afficher une instruction *)
let print_instruction instr =
  let instr_name = Printf.sprintf "%10s" instr.name_instr in
  let regs =
    match instr.instr_type with
    | ABC ->
        let a = match instr.a with Some v ->  v | None -> 0 in
        let b = match instr.b with Some v ->  v | None -> 0 in
        let c = match instr.c with Some v ->  v | None -> 0 in

        (* Traitement spécifique pour les opcodes dans _RKBCInstr ou _RKCInstr *) 
        let a, b, c =
          match instr.opcode with
          | Some op when List.mem op _RKBCInstr ->
              (Printf.sprintf "R[%d] " a, format_rk (b), format_rk (c))
          | Some op when List.mem op _RKCInstr ->
              (Printf.sprintf "R[%d]" a, Printf.sprintf "%d" b, format_rk (c))
          | _ -> (Printf.sprintf "%d" a, Printf.sprintf "%d" b, Printf.sprintf "%d" c)
        in
        Printf.sprintf "%6s %6s %6s" a b c

    | ABx | AsBx ->
        let a = match instr.a with Some v -> Printf.sprintf "R[%d]" v | None -> "None" in
        let b = match instr.b with Some v -> string_of_int v | None -> "None" in

        (* Si l'opcode est dans _KBx, formater B en tant que constante *)
        let b =
          match instr.opcode with
          | Some op when List.mem op _KBx -> Printf.sprintf "K[%s]" b
          | _ -> b
        in
        Printf.sprintf "%6s %6s" a b
  in
  Printf.sprintf "%s : %s" instr_name regs

(* Définition du type constant *)
type constant ={
  type_const : const_type;
  data : string;
}

(* Constructeur pour créer une nouvelle constante *)
let create_constant t data = {
  type_const = t;
  data = data;
}

(* Fonction pour afficher une constante *)
let toString c =
  Printf.sprintf "[%s] %s" (const_type_to_string c.type_const) c.data

(* Fonction pour convertir une constante en code Lua *)
let toCode c =
  match c.type_const with
  | STRING -> "\"" ^ c.data ^ "\""
  | BOOL -> if c.data = "true" then "true" else "false"
  | NUMBER -> Printf.sprintf "%s" c.data
  | NIL -> "nil"

(* Définition du type local *)
type local = {
  name_local : string;
  start : int;
  end_pc : int;
}

(* Constructeur pour créer une nouvelle variable locale *)
let create_local name_local start end_pc = {
  name_local = name_local;
  start = start;
  end_pc = end_pc;
}

(*Définition de type chunk "Block" *)
type chunk = {
  mutable constants : constant list;
  mutable instructions : instruction list;
  mutable protos : chunk list;

  mutable name_chunk : string;
  mutable frst_line : int;
  mutable last_line : int;
  mutable numUpvals : int;
  mutable numParams : int;
  mutable isVarg : bool;
  mutable maxStack : int;

  mutable upvalues : string list;
  mutable lineNums : int list;
  mutable locals : local list;
  
}

(* Constructeur pour créer un nouveau chunk *)
let create_chunk () = {
  constants = [];
  instructions = [];
  protos = [];

  name_chunk = "Unnamed proto";
  frst_line = 0;
  last_line = 0;
  numUpvals = 0;
  numParams = 0;
  isVarg = false;
  maxStack = 0;

  upvalues = [];
  lineNums = [];
  locals = [];
}

(* Fonction pour ajouter une instruction à un chunk *)
let append_instruction chunk instr =
  chunk.instructions <- chunk.instructions @ [instr]
(* Fonction pour ajouter une constante à un chunk *)
let append_constant chunk const =
  chunk.constants <-  chunk.constants @ [const]

(* Fonction pour ajouter un proto à un chunk *)
let append_proto chunk proto =
  chunk.protos <- chunk.protos @ [proto]

(* Fonction pour ajouter une ligne à un chunk *)
let append_line chunk line =
  chunk.lineNums <- chunk.lineNums @ [line]

(* Fonction pour ajouter une variable locale à un chunk *)
let append_local chunk local =
  chunk.locals <- chunk.locals @ [local]

(* Fonction pour ajouter un upvalue à un chunk *)
let append_upval chunk upval =
  chunk.upvalues <- chunk.upvalues @ [upval]

(* Fonction pour trouver une variable locale à partir d'une position *)
let find_local chunk pc =
  let rec find_local_aux locals pc =
    match locals with
    | [] -> None
    | l :: _ when l.start <= pc && l.end_pc >= pc -> Some l
    | _ :: rest -> find_local_aux rest pc
  in
  find_local_aux chunk.locals pc

(* Fonction pour trouver un upvalue à partir d'une position *)
let get_constant chunk indx =
    List.nth chunk.constants indx

(* Fonction pour obtenir une annotation pour une instruction *) 
let getAnnotation instr chunk =
  match instr.opcode with
  | Some 0 -> Printf.sprintf "move R[%d] into R[%d]" (match instr.b with Some v -> v | None -> 0) (match instr.a with Some v -> v | None -> 0)
  | Some 1 -> Printf.sprintf "load %s into R[%d]" (toCode (get_constant chunk (match instr.b with Some v -> v | None -> 0))) (match instr.a with Some v -> v | None -> 0)
  | Some 5 -> Printf.sprintf "move _G[%s] into R[%d]" (toCode (get_constant chunk (match instr.b with Some v -> v | None -> 0))) (match instr.a with Some v -> v | None -> 0)
  | Some 12 -> Printf.sprintf "add %s to %s, place into R[%d]" (format_rk (match instr.c with Some v -> v | None -> 0)) (format_rk (match instr.b with Some v -> v | None -> 0)) (match instr.a with Some v -> v | None -> 0)
  | Some 13 -> Printf.sprintf "sub %s from %s, place into R[%d]" (format_rk (match instr.c with Some v -> v | None -> 0)) (format_rk (match instr.b with Some v -> v | None -> 0)) (match instr.a with Some v -> v | None -> 0)
  | Some 14 -> Printf.sprintf "mul %s to %s, place into R[%d]" (format_rk (match instr.c with Some v -> v | None -> 0)) (format_rk (match instr.b with Some v -> v | None -> 0)) (match instr.a with Some v -> v | None -> 0)
  | Some 15 -> Printf.sprintf "div %s from %s, place into R[%d]" (format_rk (match instr.c with Some v -> v | None -> 0)) (format_rk (match instr.b with Some v -> v | None -> 0)) (match instr.a with Some v -> v | None -> 0)
  | Some 16 -> Printf.sprintf "mod %s from %s, place into R[%d]" (format_rk (match instr.c with Some v -> v | None -> 0)) (format_rk (match instr.b with Some v -> v | None -> 0)) (match instr.a with Some v -> v | None -> 0)
  | Some 17 -> Printf.sprintf "pow %s to %s, place into R[%d]" (format_rk (match instr.c with Some v -> v | None -> 0)) (format_rk (match instr.b with Some v -> v | None -> 0)) (match instr.a with Some v -> v | None -> 0)
  | Some 18 -> Printf.sprintf "negate %s, place into R[%d]" (format_rk (match instr.b with Some v -> v | None -> 0)) (match instr.a with Some v -> v | None -> 0)
  | Some 19 -> Printf.sprintf "not %s, place into R[%d]" (format_rk (match instr.b with Some v -> v | None -> 0)) (match instr.a with Some v -> v | None -> 0)
  | Some 20 -> Printf.sprintf "len %s, place into R[%d]" (format_rk (match instr.b with Some v -> v | None -> 0)) (match instr.a with Some v -> v | None -> 0)
  | Some 21 -> let count = (match instr.c with Some v -> v | None -> 0) - (match instr.b with Some v -> v | None -> 0) + 1 in
               Printf.sprintf "concat %d values from R[%d] to R[%d], store into R[%d]" count (match instr.b with Some v -> v | None -> 0) (match instr.c with Some v -> v | None -> 0) (match instr.a with Some v -> v | None -> 0)
  | Some 22 -> Printf.sprintf "jump to %d" (match instr.b with Some v -> v | None -> 0)
  | Some 23 -> Printf.sprintf "if R[%s] == R[%s] then pc++" (format_rk (match instr.b with Some v -> v | None -> 0)) (format_rk (match instr.c with Some v -> v | None -> 0))
  | Some 24 -> Printf.sprintf "if R[%s] < R[%s] then pc++" (format_rk (match instr.b with Some v -> v | None -> 0)) (format_rk (match instr.c with Some v -> v | None -> 0))
  | Some 25 -> Printf.sprintf "if R[%s] <= R[%s] then pc++" (format_rk (match instr.b with Some v -> v | None -> 0)) (format_rk (match instr.c with Some v -> v | None -> 0))
  | Some 26 -> Printf.sprintf "if not R[%d] <=> %d pc++" (match instr.a with Some v -> v | None -> 0) (match instr.c with Some v -> v | None -> 0)
  | Some 27 -> Printf.sprintf "if (R[%d] <=> %d) then R[%d] := R[%d] else pc++" (match instr.b with Some v -> v | None -> 0) (match instr.c with Some v -> v | None -> 0) (match instr.a with Some v -> v | None -> 0) (match instr.b with Some v -> v | None -> 0)
  | _ -> ""
  
(* Fonction pour afficher un block *)
let rec print_chunk chunk =
  Printf.printf "\n===========+ Chunk: %s +===========\n" chunk.name_chunk;
  Printf.printf "\n==== [[ constants ]] ====\n\n";
  List.iteri (fun i c ->
    Printf.printf "[%d]: %s\n" i (toString c)
  ) chunk.constants;

  Printf.printf "\n==== [[ locals ]] ====\n\n";
  List.iteri (fun i l -> Printf.printf "R[%d]: %s\n" i l.name_local ) chunk.locals;
  
  Printf.printf "\n==== [[ Instructions ]] ====\n\n" ;
  List.iteri (fun i instr ->
    Printf.printf "[%d] %-40s; %s\n" i (print_instruction instr) (getAnnotation instr chunk);
  ) chunk.instructions;

  if List.length chunk.protos > 0 then
    Printf.printf "\n==== [[ protos ]] ====\n\n";
    List.iter (fun z -> print_chunk z) chunk.protos

(* ensemble de création de couple (type, nom) pour les instructions *)
let instr_lookup_tbl = [
  create_instruction ABC "MOVE"; create_instruction ABx "LOADK"; create_instruction ABC "LOADBOOL";
  create_instruction ABC "LOADNIL"; create_instruction ABC "GETUPVAL"; create_instruction ABx "GETGLOBAL";
  create_instruction ABC "GETTABLE"; create_instruction ABx "SETGLOBAL"; create_instruction ABC "SETUPVAL";
  create_instruction ABC "SETTABLE"; create_instruction ABC "NEWTABLE"; create_instruction ABC "SELF";
  create_instruction ABC "ADD"; create_instruction ABC "SUB"; create_instruction ABC "MUL";
  create_instruction ABC "DIV"; create_instruction ABC "MOD"; create_instruction ABC "POW";
  create_instruction ABC "UNM"; create_instruction ABC "NOT"; create_instruction ABC "LEN";
  create_instruction ABC "CONCAT"; create_instruction AsBx "JMP"; create_instruction ABC "EQ";
  create_instruction ABC "LT"; create_instruction ABC "LE"; create_instruction ABC "TEST";
  create_instruction ABC "TESTSET"; create_instruction ABC "CALL"; create_instruction ABC "TAILCALL";
  create_instruction ABC "RETURN"; create_instruction AsBx "FORLOOP"; create_instruction AsBx "FORPREP";
  create_instruction ABC "TFORLOOP"; create_instruction ABC "SETLIST"; create_instruction ABC "CLOSE";
  create_instruction ABx "CLOSURE"; create_instruction ABC "VARARG"
] 

(* Fonction pour définir des bits dans un entier *)
let set_bits num data p s =
  (num land (lnot ((lnot ((lnot 0) lsl s)) lsl p))) lor
  ((data lsl p) land ((lnot ((lnot 0) lsl s)) lsl p))

(* fonction pour obtenir des bits dans un entier selon une plage donnée *)
let get_bits num p s = 
  (num lsr p) land (lnot ((lnot 0) lsl s))

(* Fonction pour décoder une instruction *)
let decode_instr data : instruction =
    let opcode = get_bits data 0 6 in
    let template = List.nth instr_lookup_tbl opcode in
    let instr = {
      instr_type = template.instr_type;
      name_instr = template.name_instr;
      opcode = Some opcode;
      a = Some (get_bits data 6 8);
      b = (match template.instr_type with
          | ABC -> Some (get_bits data 23 9)
          | ABx -> Some (get_bits data 14 18)
          | AsBx -> Some ((get_bits data 14 18) - 131071));
      c = (match template.instr_type with
          | ABC -> Some (get_bits data 14 9)
          | _ -> None)
    } in
    instr

(* Définition du type lua_undump *)
type lua_undump = {
  mutable bytecode : bytes;
  mutable index : int;
  mutable vm_version : int;
  mutable bytecode_format : int;
  mutable big_endian : bool;
  mutable int_size : int;
  mutable size_t : int;
  mutable instr_size : int;
  mutable l_number_size : int;
  mutable integral_flag : int;
  mutable root_chunk : chunk;
}

(* Constructeur pour créer un nouveau lua_undump *)
let create_lua_undump bytecode = {
  bytecode;
  index = 4;
  vm_version = 0;
  bytecode_format = 0;
  big_endian = false;
  int_size = 0;
  size_t = 0;
  instr_size = 0;
  l_number_size = 0;
  integral_flag = 0;
  root_chunk = create_chunk ();
}

let _loadBlock lua_undump sz =
  if lua_undump.index + sz > Bytes.length lua_undump.bytecode then
    failwith "Malformed bytecode!"
  else
    let temp = Bytes.sub lua_undump.bytecode lua_undump.index sz in
    lua_undump.index <- lua_undump.index + sz;
    temp

let _get_byte lua_undump =
  let byte = Bytes.get lua_undump.bytecode lua_undump.index in
  lua_undump.index <- lua_undump.index + 1;
  Char.code byte

let _get_uint32 (lua_undump : lua_undump) : int =
  let b0 = Char.code (Bytes.get lua_undump.bytecode lua_undump.index) in
  let b1 = Char.code (Bytes.get lua_undump.bytecode (lua_undump.index + 1)) in
  let b2 = Char.code (Bytes.get lua_undump.bytecode (lua_undump.index + 2)) in
  let b3 = Char.code (Bytes.get lua_undump.bytecode (lua_undump.index + 3)) in
  
  lua_undump.index <- lua_undump.index + 4;
  (*On récupere selon ordre de bytecode "big_endian" or "little endian"*)
  if lua_undump.big_endian then
    (b0 lsl 24) lor (b1 lsl 16) lor (b2 lsl 8) lor b3
  else
    (b3 lsl 24) lor (b2 lsl 16) lor (b1 lsl 8) lor b0

let _get_uint lua_undump : int =
  let block = Array.init lua_undump.int_size (fun i ->
    Char.code (Bytes.get lua_undump.bytecode (lua_undump.index + i))
  ) in
  lua_undump.index <- lua_undump.index + lua_undump.int_size;
  let result = ref 0 in
  if lua_undump.big_endian then
    Array.iter (fun b -> result := (!result lsl 8) lor b) block
  else
    Array.iteri (fun i b -> result := !result lor (b lsl (8 * i))) block;
  !result

(* Fonction pour récupérer un double *)
let _get_double lua_undump : float =
  let block = Array.init lua_undump.l_number_size (fun i ->
    Char.code (Bytes.get lua_undump.bytecode (lua_undump.index + i))
  ) in
  lua_undump.index <- lua_undump.index + lua_undump.l_number_size;
  let i =
    if lua_undump.big_endian then
      Array.fold_left (fun acc byte -> logor (shift_left acc 8) (of_int byte)) zero block
    else
      Array.fold_left (fun acc (i, byte) -> logor acc (shift_left (of_int byte) (8 * i)))
        zero (Array.mapi (fun i byte -> (i, byte)) block)
  in
  Int64.float_of_bits i

let _get_size_t lua_undump : int =
  let block = Array.init lua_undump.size_t (fun i ->
    let byte = Char.code (Bytes.get lua_undump.bytecode (lua_undump.index + i)) in
    byte
  ) in

  lua_undump.index <- lua_undump.index + lua_undump.size_t;
  if lua_undump.big_endian then
    Array.fold_left (fun acc byte -> (acc lsl 8) lor byte) 0 block
  else
    Array.fold_left (fun acc (i, byte) -> acc lor (byte lsl (8 * i))) 0
      (Array.mapi (fun i byte -> (i, byte)) block)

(* Fonction pour extraire une chaîne de caractères d'un bloc donné *)
let _get_string lua_undump size : string =
  if size <= 0 then
    ""  (* Retourne une chaîne vide si la taille est 0 ou négative *)
  else
    let block = _loadBlock lua_undump size in
    let str = Bytes.to_string block in
    (* On supprime le dernier caractère (null terminator) si la chaîne n'est pas vide *)
    if String.length str > 0 then
      String.sub str 0 (String.length str - 1)
    else
      ""

let rec decode_chunk lua_undump =
    let chunk = create_chunk () in
  
    (* Informations du chunk *)
    let size = _get_size_t lua_undump in
    chunk.name_chunk <- _get_string lua_undump size;
    chunk.frst_line <- _get_uint lua_undump;
    chunk.last_line <- _get_uint lua_undump;
    chunk.numUpvals <- _get_byte lua_undump;
    chunk.numParams <- _get_byte lua_undump;
    chunk.isVarg <- (_get_byte lua_undump) <> 0;
    chunk.maxStack <- _get_byte lua_undump;
  
    (* Instructions *)
    let num_instr = _get_uint lua_undump in
    for _ = 1 to num_instr do
      let instr = decode_instr (_get_uint32 lua_undump) in
      append_instruction chunk instr
    done;
  
    (* Constantes *)
    let num_consts = _get_uint lua_undump in
    for _ = 1 to num_consts do
      let ctype = _get_byte lua_undump in
      let constant = match ctype with
        | 0 -> { type_const = NIL; data = "" }
        | 1 -> { type_const = BOOL; data = string_of_bool ((_get_byte lua_undump) <> 0) }
        | 3 -> { type_const = NUMBER; data = string_of_float (_get_double lua_undump) }
        | 4 -> let size = _get_size_t lua_undump in { type_const = STRING; data = _get_string lua_undump size }
        | _ -> failwith (Printf.sprintf "Unknown Datatype! [%d]" ctype)
      in
      append_constant chunk constant
    done;
  
    (* Prototypes *)
    let num_protos = _get_uint lua_undump in
    for _ = 1 to num_protos do
      let proto = decode_chunk lua_undump in
      append_proto chunk proto
    done;
  
    (* Consommation des informations de débogage *)
  
    (* Numéros de ligne *)
    let num_lines = _get_uint lua_undump in
    chunk.lineNums <- List.init num_lines (fun _ -> _get_uint lua_undump);
  
    (* Locaux *)
    let num_locals = _get_uint lua_undump in
    for _ = 1 to num_locals do
      let local_name = _get_string lua_undump (_get_size_t lua_undump) in  
      let start_pc = _get_uint lua_undump in
      let end_pc = _get_uint lua_undump in
      let local = create_local local_name start_pc end_pc in
      append_local chunk local
    done;
  
    (* Upvalues *)
    let num_upvals = _get_uint lua_undump in
    for _ = 1 to num_upvals do
      let upval_name = _get_string lua_undump (_get_size_t lua_undump) in
      append_upval chunk upval_name
    done;
  
    chunk
  
let decode_bytecode bytecode =
  let lua_undump = create_lua_undump bytecode in

  (* Extraction des informations du bytecode *)
  lua_undump.vm_version <- _get_byte lua_undump;
  lua_undump.bytecode_format <- _get_byte lua_undump;
  lua_undump.big_endian <- (_get_byte lua_undump = 0);
  lua_undump.int_size <- _get_byte lua_undump;
  lua_undump.size_t <- _get_byte lua_undump;
  lua_undump.instr_size <- _get_byte lua_undump;
  lua_undump.l_number_size <- _get_byte lua_undump;
  lua_undump.integral_flag <- _get_byte lua_undump;
  (* Décodage du chunk principal *)
  lua_undump.root_chunk <- decode_chunk lua_undump;

  Printf.printf "\n===========+ HEADER +===========\n";
  Printf.printf "VM Version        : %d\n" lua_undump.vm_version;
  Printf.printf "Bytecode Format   : %d\n" lua_undump.bytecode_format;
  Printf.printf "index             : %d\n" lua_undump.index;
  Printf.printf "Big Endian        : %b\n" lua_undump.big_endian;
  Printf.printf "Int Size          : %d\n" lua_undump.int_size;
  Printf.printf "Size_t Size       : %d\n" lua_undump.size_t;
  Printf.printf "Instruction Size  : %d\n" lua_undump.instr_size;
  Printf.printf "Lua Number Size   : %d\n" lua_undump.l_number_size;
  Printf.printf "Integral Flag     : %d (%s)\n"
    lua_undump.integral_flag (if lua_undump.integral_flag = 0 then "F" else "I");
  Printf.printf "==================================\n";
  lua_undump.root_chunk

(* Fonction pour décoder un bytecode brut *)
let decode_rawbytecode lua_undump rawbytecode =
  (* Vérification de la taille avant de manipuler la chaîne *)
  if String.length rawbytecode < 4 then
    failwith "Raw bytecode too short"
  else if not (String.sub rawbytecode 0 4 = _LUAMAGIC) then
    raise LuaBytecodeExpected
  else
    let bytecode = Bytes.init (String.length rawbytecode) (fun i -> rawbytecode.[i]) in
    decode_bytecode bytecode
(* Fonction pour charger un fichier bytecode Lua *)

let load_file luaCFile =
  let ic = open_in_bin luaCFile in
  let bytecode = really_input_string ic (in_channel_length ic) in
  close_in ic;
  let bytecode_bytes = Bytes.of_string bytecode in
  decode_bytecode bytecode_bytes

(*LuaDump*)
let _encode_instr instr =
  match instr.opcode, instr.a with
  | Some opcode, Some a ->
      let data = 0 in
      let data = set_bits data opcode 0 6 in
      let data = set_bits data a 6 8 in
      let data =
        match instr.instr_type with
        | ABC  -> 
            let b = match instr.b with Some b -> b | None -> 0 in
            let c = match instr.c with Some c -> c | None -> 0 in
            let data = set_bits data b 23 9 in
            let data = set_bits data c 14 9 in
            data
        | ABx  -> 
            let b = match instr.b with Some b -> b | None -> 0 in
            let data = set_bits data b 14 18 in
            data
        | AsBx -> 
            let b = match instr.b with Some b -> b | None -> 0 in
            let data = set_bits data (b + 131071) 14 18 in
            data
      in
      Some data
      | _ -> 
        None

type endian = BigEndian | LittleEndian

type lua_dump = {
  mutable root_chunk : chunk;
  mutable bytecode : bytes;
  mutable vm_version : int;
  mutable bytecode_format : int;
  mutable big_endian : bool;
  mutable int_size : int;
  mutable size_t : int;
  mutable instr_size : int;
  mutable l_number_size : int;
  mutable integral_flag : bool;
}

let create_dump root_chunk = {
  root_chunk;
  bytecode = Bytes.create 0;
  vm_version = 0x51;
  bytecode_format = 0x00;
  big_endian = false;
  int_size = 4;
  size_t = 8;
  instr_size = 4;
  l_number_size = 8;
  integral_flag = false;
}

let write_block dump data =
  dump.bytecode <- Bytes.cat dump.bytecode data

let set_byte dump b =
  write_block dump (Bytes.make 1 (char_of_int b))

let set_uint32 dump i =
  let order = if dump.big_endian then BigEndian else LittleEndian in
  let data = Bytes.create 4 in
  for j = 0 to 3 do
    let shift = if order = BigEndian then (3 - j) * 8 else j * 8 in
    Bytes.set data j (char_of_int ((i lsr shift) land 0xFF))
  done;
  write_block dump data

let set_uint dump i size =
  let order = if dump.big_endian then BigEndian else LittleEndian in
  let data = Bytes.create size in
  for j = 0 to size - 1 do
    let shift = if order = BigEndian then (size - 1 - j) * 8 else j * 8 in
    Bytes.set data j (char_of_int ((i lsr shift) land 0xFF))
  done;
  write_block dump data

let set_size_t dump i = set_uint dump i dump.size_t

let set_double dump fval =
  let f = Int64.bits_of_float fval in
  let bytes = Bytes.create 8 in 
  if dump.big_endian then
    for n = 0 to 7 do
      Bytes.set bytes n (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right f (8 * (7 - n))) 0xFFL)))
    done
  else
    for n = 0 to 7 do
      Bytes.set bytes n (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right f (8 * n)) 0xFFL)))
    done;
  write_block dump bytes

let set_string dump str =
  set_size_t dump (String.length str + 1);
  write_block dump (Bytes.of_string str);
  set_byte dump 0x00

let rec dump_chunk dump chunk =
  set_string dump chunk.name_chunk;
  set_uint dump chunk.frst_line dump.int_size;
  set_uint dump chunk.last_line dump.int_size;
  set_byte dump chunk.numUpvals;
  set_byte dump chunk.numParams;
  set_byte dump (if chunk.isVarg then 1 else 0);
  set_byte dump chunk.maxStack;

  (* Instructions *)
  set_uint dump (List.length chunk.instructions) dump.int_size;
  List.iter (fun instr -> match _encode_instr instr with
    | Some encoded -> set_uint32 dump encoded
    | None -> ()) chunk.instructions;

  (* Constantes *)
  set_uint dump (List.length chunk.constants) dump.int_size;
  List.iter (fun constant ->
    match constant.type_const with
    | NIL -> set_byte dump 0
    | BOOL ->
        set_byte dump 1;
        set_byte dump (if constant.data = "true" then 1 else 0)
    | NUMBER ->
        set_byte dump 3;
        set_double dump (float_of_string constant.data)
    | STRING ->
        set_byte dump 4;
        set_string dump constant.data
  ) chunk.constants;

  (* Prototypes *)
  set_uint dump (List.length chunk.protos) dump.int_size;
  List.iter (dump_chunk dump) chunk.protos;

  (* Numéros de ligne *)
  set_uint dump (List.length chunk.lineNums) dump.int_size;
  List.iter (fun ln -> set_uint dump ln dump.int_size) chunk.lineNums;

  (* Locaux *)
  set_uint dump (List.length chunk.locals) dump.int_size;
  List.iter (fun local ->
    set_string dump local.name_local;
    set_uint dump local.start dump.int_size;
    set_uint dump local.end_pc dump.int_size
  ) chunk.locals;

  (* Upvalues *)
  set_uint dump (List.length chunk.upvalues) dump.int_size;
  List.iter (set_string dump) chunk.upvalues

let dump_header dump =
  write_block dump (Bytes.of_string _LUAMAGIC);
  set_byte dump dump.vm_version;
  set_byte dump dump.bytecode_format;
  set_byte dump (if dump.big_endian then 0 else 1);
  set_byte dump dump.int_size;
  set_byte dump dump.size_t;
  set_byte dump dump.instr_size;
  set_byte dump dump.l_number_size;
  set_byte dump (if dump.integral_flag then 1 else 0)

let dump dump =
  dump_header dump;
  dump_chunk dump dump.root_chunk;
  dump.bytecode

let save_to_file filename bytecode =
  let oc = open_out_bin filename in
  output_bytes oc bytecode;
  close_out oc;
  Printf.printf "Bytecode écrit dans : %s\n" filename

(* Définition des types *)
type table = (string, constant) Hashtbl.t

and func = { 
  chunk : chunk;
  global_function : (string, value) Hashtbl.t;
  }

and value =
  | Const of constant
  | Table of table
  | Func of func

type vm_state = {
  mutable stack : value array;  (* Pile d'exécution *)
  mutable globals : (string, value) Hashtbl.t; (* Variables globales *)
  mutable pc : int; (* Compteur de programme *)
  mutable chunk : chunk; (* Chunk contenant le code et les constantes *)
}




let execute_iterator vm args =
  match args with
  | [Const { type_const = STRING; data = start }; Const { type_const = NUMBER; data = idx }] -> 
      let next_idx = string_of_int ((int_of_string idx) + 1) in
      if next_idx = "10" then [Const { type_const = NIL; data = "" }]  (* Simule la fin de l'itération *)
      else [Const { type_const = NUMBER; data = next_idx }]  (* Retourne la nouvelle valeur *)
  | _ -> failwith "execute_iterator : Arguments invalides"

  let rec execute_instruction vm instr =
  if vm.pc >= List.length vm.chunk.instructions then
    failwith "Erreur : dépassement du bytecode";
  
  let instr = List.nth vm.chunk.instructions vm.pc in
  Printf.printf "Debug : pc = %d, instruction exécutée : %s, sBx = %s\n"
    vm.pc instr.name_instr
    (match instr.b with Some sbx -> string_of_int sbx | None -> "None");

  match instr.name_instr with
  | "MOVE" -> (
    match instr.a, instr.b with
    | Some a, Some b ->
        vm.stack.(a) <- vm.stack.(b);  (* R(A) := R(B) *)
        vm.pc <- vm.pc + 1
    | _ -> failwith "MOVE : paramètres invalides"
  )
  | "LOADK" -> (
      match instr.a, instr.b with
      | Some a, Some bx ->
          let constant_value = List.nth vm.chunk.constants bx in
          vm.stack.(a) <- Const constant_value;
          vm.pc <- vm.pc + 1
      | _ -> failwith "LOADK : paramètres invalides"
    )
  | "LOADBOOL" -> (
    match instr.a, instr.b, instr.c with
    | Some a, Some b, Some c ->
        let bool_value = if b = 1 then "true" else "false" in
        vm.stack.(a) <- Const { type_const = BOOL; data = bool_value };
        if c <> 0 then vm.pc <- vm.pc + 1;
        vm.pc <- vm.pc + 1
    | _ -> failwith "LOADBOOL : paramètres invalides"
  )
  | "LOADNIL" -> (
    match instr.a, instr.b with
    | Some a, Some b ->
        for i = a to b do
          vm.stack.(i) <- Const { type_const = NIL; data = "" }
        done;
        vm.pc <- vm.pc + 1
    | _ -> failwith "LOADNIL : paramètre invalide"
  )
  | "GETUPVAL" -> (
    match instr.a, instr.b with
      | Some a, Some b ->
          let upvalue = List.nth vm.chunk.upvalues b in
          vm.stack.(a) <- Const { type_const = STRING; data = upvalue };
          vm.pc <- vm.pc + 1
      | _ -> failwith "GETUPVAL : paramètres invalides"
    )
  | "GETGLOBAL" -> (
    match instr.a, instr.b with
    | Some a, Some bx -> (
      let cons = get_constant vm.chunk bx in
      let key = match cons.type_const with
        | STRING -> cons.data
        | _ -> failwith "Type error in GETGLOBAL"
      in
      vm.stack.(a) <- (match Hashtbl.find_opt vm.globals key with
                        | Some v -> v
                        | None -> Const { type_const = NIL; data = "" });
      vm.pc <- vm.pc + 1  
      )
    | _ -> failwith "GETGLOBAL : paramètres invalides"
  )
  | "GETTABLE" -> (
      match instr.a, instr.b, instr.c with
      | Some a, Some b, Some c -> (
          match vm.stack.(b) with
          | Table tbl -> 
              let get_rk x =
                if x < 256 then (
                  if x < Array.length vm.stack then vm.stack.(x)
                  else failwith ("GETTABLE : Index pile hors limites -> " ^ string_of_int x)
                ) else (
                  let k_index = x - 256 in
                  if k_index < List.length vm.chunk.constants then
                    Const (List.nth vm.chunk.constants k_index)
                  else failwith ("GETTABLE : Index constante hors limites -> " ^ string_of_int k_index)
                ) in
              
              (* Récupération de RK(C) *)
              let rk_c = get_rk c in
              
              (* Extraction et normalisation de la clé *)
              let key = match rk_c with
                | Const { data } -> (
                    try 
                      (* 🔥 Si c'est un nombre, on le normalise pour SETLIST 🔥 *)
                      string_of_int (int_of_float (float_of_string data))
                    with Failure _ ->
                      (* 🔥 Sinon, on le laisse tel quel pour les chaînes 🔥 *)
                      data
                  )
                | _ -> failwith "GETTABLE : Clé invalide, doit être une constante"
              in

              (* Debugging: Afficher la clé recherchée *)
              Printf.printf "Debug: GETTABLE - Recherche tbl[%s]\n" key;

              (* Accès à la table avec la clé *)
              let value = match Hashtbl.find_opt tbl key with
                | Some v -> v  (* R(A) := R(B)[RK(C)] *)
                | None -> 
                  Printf.printf "Debug: GETTABLE - Clé introuvable %s, retour nil\n" key;
                  { type_const = NIL; data = "" }  (* Si la clé n'existe pas, R(A) = nil *)
              in

              (* Debugging: Afficher la valeur trouvée *)
              Printf.printf "Debug: GETTABLE - tbl[%s] -> %s\n" key 
                (match value with { data } -> data );

              (* Stocker la valeur obtenue dans R(A) *)
              vm.stack.(a) <- Const value;

              (* Avancer le compteur de programme *)
              vm.pc <- vm.pc + 1
          | _ -> failwith "GETTABLE : R(B) n'est pas une table"
        )
      | _ -> failwith "GETTABLE : paramètres invalides"
    )
  | "SETGLOBAL" -> (
    match instr.a, instr.b with
    | Some a, Some bx -> (
        let key = List.nth vm.chunk.constants bx in  (* Kst(Bx) *)
        let value = vm.stack.(a) in  (* R(A) *)
        Hashtbl.replace vm.globals key.data value;  (* Stocke directement `value` *)
        vm.pc <- vm.pc + 1
      )
    | _ -> failwith "SETGLOBAL : paramètres invalides"
  )
  | "SETUPVAL" -> (
    match instr.a, instr.b with
    | Some a, Some b -> (
        if b >= List.length vm.chunk.upvalues then
          failwith ("SETUPVAL : Index d'UpValue invalide -> " ^ string_of_int b)
        else (
          let value = vm.stack.(a) in  (* R(A) *)
          let upvalue_name = List.nth vm.chunk.upvalues b in  (* UpValue[B] *)
          Hashtbl.replace vm.globals upvalue_name value;  (* Simule l'affectation d'UpValue *)
          vm.pc <- vm.pc + 1
        )
      )
    | _ -> failwith "SETUPVAL : paramètres invalides"
  )
  | "SETTABLE" -> (
    match instr.a, instr.b, instr.c with
    | Some a, Some b, Some c when a < Array.length vm.stack -> (
        match vm.stack.(a) with
        | Table tbl -> (
          let get_rk x =
            if x < 256 then (
              if x < Array.length vm.stack then vm.stack.(x)
              else failwith ("EQ : Index pile hors limites -> " ^ string_of_int x)
            ) else (
              let k_index = x - 256 in
              if k_index < List.length vm.chunk.constants then
                Const (List.nth vm.chunk.constants k_index)
              else failwith ("EQ : Index constante hors limites -> " ^ string_of_int k_index)
            )
          in
          let rk_b = get_rk b in
          let rk_c = get_rk c in

            (* Vérifier que RK(B) est une clé STRING *)
            let key = match rk_b with
              | Const { data } -> data
              | _ -> failwith "SETTABLE : RK(B) doit être une clé STRING"
            in

            (* Ajouter RK(C) dans la table *)
            (match rk_c with
            | Const constant -> Hashtbl.replace tbl key constant
            | _ -> failwith "SETTABLE : RK(C) must be a constant");
            vm.pc <- vm.pc + 1
          )
        | _ -> failwith "SETTABLE : R(A) doit être une table"
      )
    | _ -> failwith "SETTABLE : Paramètres invalides ou hors limites"
  )
  | "NEWTABLE" -> (
    match instr.a, instr.b, instr.c with
    | Some a, Some b, Some c ->
      let size = Int64.to_int (Int64.max (Int64.of_int b) (Int64.of_int c)) in
        let new_table = Hashtbl.create size in  (* Crée une table vide avec taille estimée *)
        vm.stack.(a) <- Table new_table;  (* Stocke la table dans R(A) *)
        vm.pc <- vm.pc + 1
    | _ -> failwith "NEWTABLE : paramètres invalides"
  )
  | "SELF" -> (
    match instr.a, instr.b, instr.c with
    | Some a, Some b, Some c -> (
        match vm.stack.(b) with
        | Table tbl -> (
            let key = match vm.stack.(c) with
              | Const { data } -> data  (* RK(C) est une constante ou une valeur dans la pile *)
              | _ -> failwith "SELF : Clé invalide"
            in
            let value = Hashtbl.find_opt tbl key in
            vm.stack.(a + 1) <- Table tbl;  (* R(A+1) := R(B) *)
            vm.stack.(a) <- (match value with 
              | Some v -> Const v  (* R(A) := R(B)[RK(C)] *)
              | None -> Const { type_const = NIL; data = "" }  (* Valeur absente -> nil *)
            );
            vm.pc <- vm.pc + 1
        )
        | _ -> failwith "SELF : R(B) n'est pas une table"
      )
    | _ -> failwith "SELF : paramètres invalides"
  )
  | "ADD" -> (
    match instr.a, instr.b, instr.c with
    | Some a, Some b, Some c -> (
        (* Récupération des valeurs RK(B) et RK(C) *)
        let op1 =
          if b < List.length vm.chunk.constants then
            List.nth vm.chunk.constants b  (* RK(B) est une constante *)
          else
            (match vm.stack.(b) with
            | Const constant -> constant
            | _ -> failwith "ADD : Expected a constant value")  (* RK(B) est un registre *)
        in
        let op2 =
          if c < List.length vm.chunk.constants then
            List.nth vm.chunk.constants c  (* RK(C) est une constante *)
          else
            (match vm.stack.(c) with
            | Const constant -> constant
            | _ -> failwith "ADD : Expected a constant value")  (* RK(C) est un registre *)
        in

        (* Vérification des types et addition *)
        match op1, op2 with
        | { type_const = NUMBER; data = x }, { type_const = NUMBER; data = y } ->
            let result = { type_const = NUMBER; data = string_of_float ((float_of_string x) +. (float_of_string y)) } in
            vm.stack.(a) <- Const result;
            vm.pc <- vm.pc + 1
        | _ -> failwith "ADD : Opérandes non numériques"
      )
    | _ -> failwith "ADD : paramètres invalides"
  )
  | "SUB" -> (
    match instr.a, instr.b, instr.c with
    | Some a, Some b, Some c -> (
        (* Récupération des opérandes *)
        let op1 =
          if b < List.length vm.chunk.constants then
            List.nth vm.chunk.constants b  (* RK(B) est une constante *)
          else
            (match vm.stack.(b) with
            | Const constant -> constant
            | _ -> failwith "SUB : Expected a constant value")  (* RK(B) est un registre *)
        in
        let op2 =
          if c < List.length vm.chunk.constants then
            List.nth vm.chunk.constants c  (* RK(C) est une constante *)
          else
            (match vm.stack.(c) with
            | Const constant -> constant
            | _ -> failwith "SUB : Expected a constant value")  (* RK(C) est un registre *)
        in

        (* Vérification des types et soustraction *)
        match op1, op2 with
        | { type_const = NUMBER; data = x }, { type_const = NUMBER; data = y } ->
            let result = { type_const = NUMBER; data = string_of_float ((float_of_string x) -. (float_of_string y)) } in
            vm.stack.(a) <- Const result;  (* Stocker le résultat dans R(A) *)
            vm.pc <- vm.pc + 1
        | _ -> failwith "SUB : Opérandes non numériques"
      )
    | _ -> failwith "SUB : paramètres invalides"
  )
  | "MUL" -> (
    match instr.a, instr.b, instr.c with
    | Some a, Some b, Some c -> (
        (* Récupération des valeurs RK(B) et RK(C) *)
        let op1 =
          if b < List.length vm.chunk.constants then
            List.nth vm.chunk.constants b  (* RK(B) est une constante *)
          else
            (match vm.stack.(b) with
            | Const constant -> constant  (* RK(B) est un registre *)
            | _ -> failwith "MUL : RK(B) n'est pas un nombre")
        in
        let op2 =
          if c < List.length vm.chunk.constants then
            List.nth vm.chunk.constants c  (* RK(C) est une constante *)
          else
            (match vm.stack.(c) with
            | Const constant -> constant  (* RK(C) est un registre *)
            | _ -> failwith "MUL : RK(C) n'est pas un nombre")
        in

        (* Vérification des types et multiplication *)
        match op1, op2 with
        | { type_const = NUMBER; data = x }, { type_const = NUMBER; data = y } ->
            let result = { type_const = NUMBER; data = string_of_float ((float_of_string x) *. (float_of_string y)) } in
            vm.stack.(a) <- Const result;  (* R(A) := RK(B) * RK(C) *)
            vm.pc <- vm.pc + 1
        | _ -> failwith "MUL : Opérandes non numériques"
      )
    | _ -> failwith "MUL : paramètres invalides"
  )
  | "DIV" -> (
    match instr.a, instr.b, instr.c with
    | Some a, Some b, Some c -> (
        (* Récupération des valeurs RK(B) et RK(C) *)
        let op1 =
          if b < List.length vm.chunk.constants then
            List.nth vm.chunk.constants b  (* RK(B) est une constante *)
          else
            (match vm.stack.(b) with
            | Const constant -> constant
            | _ -> failwith "DIV : Expected a constant value")  (* RK(B) est un registre *)
        in
        let op2 =
          if c < List.length vm.chunk.constants then
            List.nth vm.chunk.constants c  (* RK(C) est une constante *)
          else
            (match vm.stack.(c) with
            | Const constant -> constant
            | _ -> failwith "DIV : Expected a constant value")  (* RK(C) est un registre *)
        in

        (* Vérification des types et division *)
        match op1, op2 with
        | { type_const = NUMBER; data = x }, { type_const = NUMBER; data = y } ->
            if float_of_string y = 0.0 then
              failwith "DIV : Division par zéro"
            else
              let result = { type_const = NUMBER; data = string_of_float ((float_of_string x) /. (float_of_string y)) } in
              vm.stack.(a) <- Const result;
              vm.pc <- vm.pc + 1
        | _ -> failwith "DIV : Opérandes non numériques"
      )
    | _ -> failwith "DIV : paramètres invalides"
  )
  | "MOD" -> (
    match instr.a, instr.b, instr.c with
    | Some a, Some b, Some c -> (
        (* Récupération des valeurs RK(B) et RK(C) *)
        let op1 =
          if b < List.length vm.chunk.constants then
            List.nth vm.chunk.constants b  (* RK(B) est une constante *)
          else
            (match vm.stack.(b) with
            | Const constant -> constant
            | _ -> failwith "MOD : Expected a constant value")  (* RK(B) est un registre *)
        in
        let op2 =
          if c < List.length vm.chunk.constants then
            List.nth vm.chunk.constants c  (* RK(C) est une constante *)
          else
            (match vm.stack.(c) with
            | Const constant -> constant
            | _ -> failwith "MOD : Expected a constant value")  (* RK(C) est un registre *)
        in

        (* Vérification des types et calcul du modulo *)
        match op1, op2 with
        | { type_const = NUMBER; data = x }, { type_const = NUMBER; data = y } ->
            if float_of_string y = 0.0 then
              failwith "MOD : Division par zéro"
            else
              let result = { type_const = NUMBER; data = string_of_float (mod_float (float_of_string x) (float_of_string y)) } in
              vm.stack.(a) <- Const result;
              vm.pc <- vm.pc + 1
        | _ -> failwith "MOD : Opérandes non numériques"
      )
    | _ -> failwith "MOD : paramètres invalides"
  )
  | "POW" -> (
    match instr.a, instr.b, instr.c with
    | Some a, Some b, Some c -> (
        (* Récupération des valeurs RK(B) et RK(C) *)
        let op1 =
          if b < List.length vm.chunk.constants then
            List.nth vm.chunk.constants b  (* RK(B) est une constante *)
          else
            (match vm.stack.(b) with
            | Const constant -> constant
            | _ -> failwith "POW : Expected a constant value")  (* RK(B) est un registre *)
        in
        let op2 =
          if c < List.length vm.chunk.constants then
            List.nth vm.chunk.constants c  (* RK(C) est une constante *)
          else
            (match vm.stack.(c) with
            | Const constant -> constant
            | _ -> failwith "POW : Expected a constant value")  (* RK(C) est un registre *)
        in

        (* Vérification des types et calcul de la puissance *)
        match op1, op2 with
        | { type_const = NUMBER; data = x }, { type_const = NUMBER; data = y } ->
            let result = { type_const = NUMBER; data = string_of_float ((float_of_string x) ** (float_of_string y)) } in
            vm.stack.(a) <- Const result;
            vm.pc <- vm.pc + 1
        | _ -> failwith "POW : Opérandes non numériques"
      )
    | _ -> failwith "POW : paramètres invalides"
  )
  | "UNM" -> (
    match instr.a, instr.b with
    | Some a, Some b -> (
        (* Récupération de la valeur R(B) *)
        let op =
          if b < List.length vm.chunk.constants then
            List.nth vm.chunk.constants b  (* R(B) est une constante *)
          else
            (match vm.stack.(b) with
            | Const constant -> constant
            | _ -> failwith "UNM : Expected a constant value")  (* R(B) est un registre *)
        in

        (* Vérification du type et négation *)
        match op with
        | { type_const = NUMBER; data = x } ->
            let result = { type_const = NUMBER; data = string_of_float (-. (float_of_string x)) } in
            vm.stack.(a) <- Const result;
            vm.pc <- vm.pc + 1
        | _ -> failwith "UNM : Opérande non numérique"
      )
    | _ -> failwith "UNM : paramètres invalides"
  )
  | "NOT" -> (
    match instr.a, instr.b with
    | Some a, Some b -> (
        (* Récupération de la valeur R(B) *)
        let op = vm.stack.(b) in

        (* Application de l'opération logique NOT *)
        let result =
          match op with
          | Const { type_const = BOOL; data = "true" } -> Const { type_const = BOOL; data = "false" }
          | Const { type_const = BOOL; data = "false" } -> Const { type_const = BOOL; data = "true" }
          | Const { type_const = NIL; _ } -> Const { type_const = BOOL; data = "true" } (* Lua : `not nil` est `true` *)
          | _ -> Const { type_const = BOOL; data = "false" } (* Lua : `not` de tout autre type est `false` *)
        in

        (* Stockage du résultat dans R(A) *)
        vm.stack.(a) <- result;
        vm.pc <- vm.pc + 1
      )
    | _ -> failwith "NOT : paramètres invalides"
  )
  | "LEN" -> (
    match instr.a, instr.b with
    | Some a, Some b -> (
        match vm.stack.(b) with
        | Const { type_const = STRING; data } ->  
            let result = { type_const = NUMBER; data = string_of_int (String.length data) } in
            vm.stack.(a) <- Const result  (* R(A) := longueur de la chaîne *)
        | Table tbl ->  
            let result = { type_const = NUMBER; data = string_of_int (Hashtbl.length tbl) } in
            vm.stack.(a) <- Const result  (* R(A) := nombre d'éléments dans la table *)
        | _ -> failwith "LEN : Opérande invalide, doit être une chaîne ou une table"
      );
      vm.pc <- vm.pc + 1
    | _ -> failwith "LEN : paramètres invalides"
  ) 
  | "CONCAT" -> (
    match instr.a, instr.b, instr.c with
    | Some a, Some b, Some c -> (
        if b > c then failwith "CONCAT : B doit être inférieur ou égal à C"
        else
          let buffer = Buffer.create 16 in
          for i = b to c do
            match vm.stack.(i) with
            | Const { type_const = STRING; data } -> Buffer.add_string buffer data
            | Const { type_const = NUMBER; data } -> Buffer.add_string buffer data
            | _ -> failwith "CONCAT : Opérande non concaténable"
          done;
          vm.stack.(a) <- Const { type_const = STRING; data = Buffer.contents buffer }; (* R(A) := R(B).. ... ..R(C) *)
          vm.pc <- vm.pc + 1
      )
    | _ -> failwith "CONCAT : paramètres invalides"
  )
  | "JMP" -> (
    match instr.b with
    | Some sbx -> 
        vm.pc <- vm.pc + sbx;
    | _ -> failwith "JMP : paramètre invalide"
  )
  | "EQ" -> (
    match instr.a, instr.b, instr.c with
    | Some a, Some b, Some c -> (
        let get_rk x =
          if x < 256 then (
            if x < Array.length vm.stack then vm.stack.(x)
            else failwith ("EQ : Index pile hors limites -> " ^ string_of_int x)
          ) else (
            let k_index = x - 256 in
            if k_index < List.length vm.chunk.constants then
              Const (List.nth vm.chunk.constants k_index)
            else failwith ("EQ : Index constante hors limites -> " ^ string_of_int k_index)
          )
        in
        let rk_b = get_rk b in
        let rk_c = get_rk c in
        (* Comparaison *)
        let is_equal = match rk_b, rk_c with
          | Const { data = x }, Const { data = y } -> x = y
          | _ -> false
        in
        if is_equal <> (a <> 1) then (
          Printf.printf "EQ : On saute 3 instructions\n";
          vm.pc <- vm.pc + 2);
          vm.pc <- vm.pc + 1
      )
    | _ -> failwith "EQ : paramètres invalides"
  ) 
  | "LT" -> (
    match instr.a, instr.b, instr.c with
    | Some a, Some b, Some c -> (
        let get_rk x =
          if x < 256 then (
            if x < Array.length vm.stack then vm.stack.(x)
            else failwith ("LT : Index pile hors limites -> " ^ string_of_int x)
          ) else (
            let k_index = x - 256 in
            if k_index < List.length vm.chunk.constants then
              Const (List.nth vm.chunk.constants k_index)
            else failwith ("LT : Index constante hors limites -> " ^ string_of_int k_index)
          )
        in

        let rk_b = get_rk b in
        let rk_c = get_rk c in

        (* Extraction des valeurs *)
        let get_number = function
          | Const { type_const = NUMBER; data } -> float_of_string data
          | _ -> failwith "LT : Opérandes non numériques"
        in
        let val_b = get_number rk_b in
        let val_c = get_number rk_c in

        (* Debug *)
        Printf.printf "Debug: LT sur b = %f et c = %f\n" val_b val_c;

        (* Comparaison *)
        let is_less = val_b < val_c in
        Printf.printf "Résultat de LT: %b (A=%d)\n" is_less a;

        (* Gestion du saut *)
        if is_less <> (a <> 1) then (
          Printf.printf "LT : On saute 2 instructions\n";
          vm.pc <- vm.pc + 2
        );

        vm.pc <- vm.pc + 1
      )
    | _ -> failwith "LT : paramètres invalides"
  )
  | "LE" -> (
    match instr.a, instr.b, instr.c with
    | Some a, Some b, Some c -> (
        let get_rk x =
          if x < 256 then (
            if x < Array.length vm.stack then vm.stack.(x)
            else failwith ("LE : Index pile hors limites -> " ^ string_of_int x)
          ) else (
            let k_index = x - 256 in
            if k_index < List.length vm.chunk.constants then
              Const (List.nth vm.chunk.constants k_index)
            else failwith ("LE : Index constante hors limites -> " ^ string_of_int k_index)
          )
        in
        let rk_b = get_rk b in
        let rk_c = get_rk c in
        let is_lessAndEqual = match rk_b, rk_c with
          | Const { data = x }, Const { data = y } -> x <= y
          | _ -> false
        in
        Printf.printf "Résultat de LE: %b (A=%d)\n" is_lessAndEqual a;
        if is_lessAndEqual <> (a <> 1) then (
          Printf.printf "EQ : On saute 3 instructions\n";
          vm.pc <- vm.pc + 2);
          vm.pc <- vm.pc + 1
      )
    | _ -> failwith "LE : paramètres invalides"
  )
  | "TEST" -> (
    match instr.a, instr.c with
    | Some a, Some c -> (
        (* Récupération de la valeur R(A) *)
        let op = vm.stack.(a) in

        (* Évaluation de la condition *)
        let condition =
          match op with
          | Const { type_const = BOOL; data = "true" } -> true  (* BOOL stocke "true" ou "false" sous forme de STRING *)
          | Const { type_const = BOOL; data = "false" } -> false
          | Const { type_const = NIL; _ } -> false  (* NIL est toujours considéré comme faux *)
          | _ -> true  (* Tout autre type est considéré comme vrai en Lua *)
        in

        (* Appliquer l'opération logique <=> avec C *)
        let expected = (c = 1) in  (* Si C = 1, on teste si condition est vraie, sinon on teste si elle est fausse *)
        let result = (condition = expected) in

        (* Si le test échoue, on saute l'instruction suivante *)
        if not result then vm.pc <- vm.pc + 1;
        vm.pc <- vm.pc + 1
      )
    | _ -> failwith "TEST : paramètres invalides"
  )
  | "TESTSET" -> (
    match instr.a, instr.b, instr.c with
    | Some a, Some b, Some c -> (
        (* Récupération de la valeur R(B) *)
        let op = vm.stack.(b) in

        (* Évaluation de la condition *)
        let condition =
          match op with
          | Const { type_const = BOOL; data = "true" } -> true  (* BOOL stocke "true" ou "false" sous forme de STRING *)
          | Const { type_const = BOOL; data = "false" } -> false
          | Const { type_const = NIL; _ } -> false  (* NIL est toujours considéré comme faux *)
          | _ -> true  (* Tout autre type est considéré comme vrai en Lua *)
        in

        (* Appliquer l'opération logique <=> avec C *)
        let expected = (c = 1) in  (* Si C = 1, on teste si condition est vraie, sinon on teste si elle est fausse *)
        let result = (condition = expected) in

        if result then
          (* R(A) := R(B) *)
          vm.stack.(a) <- vm.stack.(b)
        else
          (* Si le test échoue, on saute l'instruction suivante *)
          vm.pc <- vm.pc + 1;

        (* Passer à l'instruction suivante *)
        vm.pc <- vm.pc + 1
      )
    | _ -> failwith "TESTSET : paramètres invalides"
  )
  (*call*)
  | "CALL" -> (
    match instr.a, instr.b, instr.c with
    | Some a, Some b, Some c -> (
        match vm.stack.(a) with
        | Const { type_const = STRING; data = "print" } -> (
            match vm.stack.(a + 1) with
            | Const { type_const = NUMBER; data } | Const { type_const = STRING; data } ->
                Printf.printf "Résultat: %s\n" data;
                vm.pc <- vm.pc + 1
            | Const { type_const = NIL; _ } -> print_endline ""; vm.pc <- vm.pc + 1
            | Const { type_const = BOOL; data } -> print_endline (if data = "true" then "true" else "false"); vm.pc <- vm.pc + 1
            | Table tbl -> (
                Printf.printf "Table: %d éléments\n" (Hashtbl.length tbl);
                Hashtbl.iter (fun key value -> Printf.printf "[%s] = %s\n" key (match Const value with Const { data } -> data | _ -> "unknown")) tbl;
                vm.pc <- vm.pc + 1
              )
            | _ -> failwith "CALL : Type non supporté par print"
          )
        | Const { type_const = STRING; data } -> failwith ("CALL : Fonction inconnue (non-print) -> " ^ data)
        | Func func -> (
            let new_vm = { 
                stack = Array.make 10 (Const { type_const = NIL; data = "" });
                pc = 0;
                globals = func.global_function;
                chunk = func.chunk;
            } in

            (* Copier les arguments vers la nouvelle pile *)
            for i = 1 to (b - 1) do
                new_vm.stack.(i - 1) <- vm.stack.(a + i)
            done;
            run_vm new_vm new_vm.chunk;
            if c > 0 then
                vm.stack.(a) <- new_vm.stack.(0);

            ()
        )

        | _ -> 
            Printf.printf "Erreur : la valeur sur la pile n'est pas une fonction (type : %s)\n%!" 
                (match vm.stack.(a) with
                | Const { type_const = NIL; _ } -> "Nil"
                | Const { type_const = BOOL; _ } -> "Bool"
                | Const { type_const = NUMBER; _ } -> "Number"
                | Const { type_const = STRING; _ } -> "String"
                | Table _ -> "Table"
                | Func _ -> "Function");
        vm.pc <- vm.pc + 1
      )
    | _ -> failwith "CALL : paramètres invalides"
  )

  
  (*tailcall*)
  | "TAILCALL" -> (
    match instr.a, instr.b, instr.c with
    | Some a, Some b, Some c -> (
        match vm.stack.(a) with
        | Func func -> (
            for i = 1 to b do
              vm.stack.(i - 1) <- vm.stack.(a + i)
            done;
            vm.pc <- 0; 
            vm.chunk <- func.chunk;
            run_vm vm func.chunk;
            if c > 0 then
              vm.stack.(a) <- vm.stack.(0);
            vm.pc <- List.length vm.chunk.instructions
        )

        | Const { type_const = STRING; data = "print" } -> (
            let rec print_args i =
              if i < Array.length vm.stack && vm.stack.(i) <> Const { type_const = NIL; data = "" } then (
                print_string (match vm.stack.(i) with
                  | Const { data } -> data
                  | _ -> failwith "TAILCALL : Type non supporté par print");
                print_string " ";
                print_args (i + 1)
              )
            in
            print_args (a + 1);
            print_newline ();
            for i = a + 1 to Array.length vm.stack - 1 do
              vm.stack.(i) <- Const { type_const = NIL; data = "" }
            done;

            vm.stack.(0) <- Const { type_const = NIL; data = "" };
            vm.pc <- List.length vm.chunk.instructions
        )

        | _ -> 
            Printf.printf "Erreur : la valeur sur la pile n'est pas une fonction (type : %s)\n%!" 
              (match vm.stack.(a) with
               | Const { type_const = NIL; _ } -> "Nil"
               | Const { type_const = BOOL; _ } -> "Bool"
               | Const { type_const = NUMBER; _ } -> "Number"
               | Const { type_const = STRING; _ } -> "String"
               | Table _ -> "Table"
               | Func _ -> "Function");
            failwith "Type error in TAILCALL"
      )
    | _ -> failwith "TAILCALL : paramètres invalides"
  )


  | "RETURN" -> (
    match instr.a, instr.b with
    | Some a, Some b -> (
        let n =
          if b = 0 then Array.length vm.stack - a
          else b - 1
        in
        for i = 0 to n - 1 do
          vm.stack.(i) <- vm.stack.(a + i)
        done;
        vm.pc <- List.length vm.chunk.instructions  (* Arrêter l'exécution *)
      )
    | _ -> failwith "RETURN : paramètres invalides"
  )
(*
  | "FORPREP" -> (
      match instr.a, instr.b with
      | Some a, Some sbx -> (
          Printf.printf "Debug : FORPREP saute de %d instructions depuis pc = %d\n" sbx vm.pc;
  
          (* Vérifie que le saut pointe bien vers FORLOOP *)
          let target_pc = vm.pc + sbx in
          if target_pc < List.length vm.chunk.instructions then
            let target_instr = List.nth vm.chunk.instructions target_pc in
            if target_instr.name_instr = "FORLOOP" then (
              Printf.printf "✅ FORPREP saute bien vers FORLOOP à pc = %d\n" target_pc;
              vm.pc <- target_pc
            ) else (
              failwith (Printf.sprintf "FORPREP : Mauvaise cible (pc = %d, attendu FORLOOP, trouvé %s)"
                target_pc target_instr.name_instr)
            )
          else
            failwith "FORPREP : sBx invalide, dépasse le bytecode"
        )
      | _ -> failwith "FORPREP : paramètres invalides"
    )

  | "FORLOOP" -> (
      match instr.a, instr.b with
      | Some a, Some sbx -> (
          Printf.printf "Debug : FORLOOP sur R(%d), sBx = %d\n" a sbx;
          
          (* Récupération de R(A) (compteur), R(A+1) (limite) et R(A+2) (pas) *)
          match vm.stack.(a), vm.stack.(a+1), vm.stack.(a+2) with
          | Const { type_const = NUMBER; data = counter },
            Const { type_const = NUMBER; data = limit },
            Const { type_const = NUMBER; data = step } ->
  
              (* Incrémentation : R(A) += R(A+2) *)
              let new_value = (float_of_string counter) +. (float_of_string step) in
              vm.stack.(a) <- Const { type_const = NUMBER; data = string_of_float new_value };
  
              (* Vérification de la condition de boucle *)
              if (float_of_string step) > 0.0 then
                  (* Cas croissant : continue si R(A) <= R(A+1) *)
                  if new_value <= float_of_string limit then (
                      vm.stack.(a+3) <- Const { type_const = NUMBER; data = string_of_float new_value };
                      vm.pc <- vm.pc + sbx  (* Retourne au début du bloc for *)
                  ) else (
                      vm.pc <- vm.pc + 1  (* Fin de la boucle *)
                  )
              else
                  (* Cas décroissant : continue si R(A) >= R(A+1) *)
                  if new_value >= float_of_string limit then (
                      vm.stack.(a+3) <- Const { type_const = NUMBER; data = string_of_float new_value };
                      vm.pc <- vm.pc + sbx  (* Retourne au début du bloc for *)
                  ) else (
                      vm.pc <- vm.pc + 1  (* Fin de la boucle *)
                  )
          | _ -> failwith "FORLOOP : Types incompatibles pour la boucle"
        )
      | _ -> failwith "FORLOOP : paramètres invalides"
    )
  
  | "TFORLOOP" -> (
    match instr.a, instr.c with
    | Some a, Some c -> (
        (* R(A) est la fonction itérative *)
        match vm.stack.(a) with
        | Const { type_const = STRING; data = "iterator" } -> (
            (* Supposons que l'itérateur Lua est stocké comme une fonction spéciale *)
            let args = [ vm.stack.(a + 1); vm.stack.(a + 2) ] in
            let results = execute_iterator args in  (* Exécute la fonction itérative *)
            
            (* Stocker les résultats dans R(A+3) à R(A+2+C) *)
            let rec store_results i res_list = 
              match res_list with
              | [] -> ()
              | r :: rs -> 
                  if i <= c then (
                    vm.stack.(a + 2 + i) <- r;
                    store_results (i + 1) rs
                  )
            in
            store_results 1 results;
            
            (* Vérifier si R(A+3) est nil *)
            match vm.stack.(a + 3) with
            | Const { type_const = NIL; _ } -> vm.pc <- vm.pc + 1  (* Passe à l'instruction suivante *)
            | _ -> vm.stack.(a + 2) <- vm.stack.(a + 3)  (* R(A+2) := R(A+3) *)
          )
        | _ -> failwith "TFORLOOP : R(A) doit être un itérateur"
      );
      vm.pc <- vm.pc + 1
    | _ -> failwith "TFORLOOP : paramètres invalides"
  )
  *)
  | "SETLIST" -> (
    match instr.a, instr.b, instr.c with
    | Some a, Some b, Some c -> (
        match vm.stack.(a) with
        | Table tbl ->  
            let fpf = 50 in  (* Fields Per Flush, généralement 50 *)
            let base_index = (c - 1) * fpf in
            Printf.printf "Debug: SETLIST - Insère %d valeurs à partir de l'index %d\n" b base_index;
            
            for i = 1 to b do
                let key = string_of_int (base_index + i) in
                match vm.stack.(a + i) with
                | Const value -> 
                    Printf.printf "Debug: SETLIST - tbl[%s] := %s\n" key value.data;
                    Hashtbl.replace tbl key value
                | Table t -> 
                    Printf.printf "Debug: SETLIST - tbl[%s] := <table>\n" key;
                    Hashtbl.replace tbl key { type_const = STRING; data = "<table>" }
                | _ -> failwith "SETLIST : Valeur non supportée"
                  done;

            vm.pc <- vm.pc + 1
        | _ -> failwith "SETLIST : R(A) doit être une table"
      )
    | _ -> failwith "SETLIST : paramètres invalides"
  )

  | "CLOSE" -> (
    match instr.a with
    | Some a ->
        (* Ferme toutes les variables dans la pile jusqu'à R(A) *)
        for i = a to Array.length vm.stack - 1 do
            vm.stack.(i) <- Const { type_const = NIL; data = "" } (* Remplace par NIL *)
        done;
        vm.pc <- vm.pc + 1
    | _ -> failwith "CLOSE : paramètre invalide"
  )
  | "CLOSURE" -> (
    match instr.a, instr.b with
    | Some a, Some bx -> (
        (* Récupération du prototype de fonction *)
        let proto = List.nth vm.chunk.protos bx in
        
        (* Création de la fermeture et stockage dans R(A) *)
        vm.stack.(a) <- Func { chunk = proto; global_function = Hashtbl.copy vm.globals };
        
        (* Passage à l'instruction suivante *)
        vm.pc <- vm.pc + 1
      )
    | _ -> failwith "CLOSURE : paramètres invalides"
  )

  | "VARARG" -> (
    match instr.a, instr.b with
    | Some a, Some b ->
        (* Vérifier que des arguments variadiques existent *)
        if List.length vm.chunk.upvalues < b then
          failwith "VARARG : Pas assez d'arguments variadiques"
        else
          (* Stocker les B arguments variadiques dans R(A) à R(A+B-1) *)
          for i = 0 to b - 1 do
            let vararg_value = List.nth vm.chunk.upvalues i in
            vm.stack.(a + i) <- Const { type_const = STRING; data = vararg_value }
          done;
        vm.pc <- vm.pc + 1
    | _ -> failwith "VARARG : paramètres invalides"
  )
  | _ -> failwith ("Opcode inconnu : " ^ instr.name_instr)
and run_vm vm chunk =
  vm.chunk <- chunk;
  vm.pc <- 0;
  while vm.pc < List.length chunk.instructions do
    let instr = List.nth chunk.instructions vm.pc in
    execute_instruction vm instr;
  done

let init_vm =
  let vm = {
        stack = Array.make 10 (Const { type_const = NIL; data = "" });
        globals = Hashtbl.create 10;
        pc = 0;
        chunk = create_chunk();
      } in
      Hashtbl.add vm.globals "print" (Const { type_const = STRING; data = "print" });
      vm

let () =
  let test_dir = "../test" in  (* Chemin vers le répertoire contenant les fichiers .luac *)
  let output_dir = "../resultat_dump" in  (* Répertoire de destination *)

  (* Vérifier si le répertoire de sortie existe, sinon le créer *)
  if not (Sys.file_exists output_dir) then Sys.mkdir output_dir 0o755;

  (* Vérifier si le répertoire test existe bien *)
  if Sys.file_exists test_dir && Sys.is_directory test_dir then
    let files = Sys.readdir test_dir in
    Array.iter (fun file ->
      if Filename.check_suffix file ".luac" then
        let input_file = Filename.concat test_dir file in
        let chunk = load_file input_file in
        print_chunk chunk;
        
        (* Création de l'objet lua_dump *)
        let lua_dump = create_dump chunk in
        
        (* Génération du bytecode *)
        let bytecode = dump lua_dump in
        
        (* Définition du chemin du fichier de sortie *)
        let output_file = Filename.concat output_dir ("processed_" ^ file) in
        save_to_file output_file bytecode;
        
        Printf.printf "Fichier traité : %s -> %s\n" input_file output_file;
      
      
        let vm=init_vm in
        run_vm vm chunk;
        Printf.printf "Execution terminée\n";
  
    ) files
  else
    Printf.printf "Erreur : Le répertoire %s n'existe pas ou n'est pas un dossier.\n" test_dir
