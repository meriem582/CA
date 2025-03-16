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

  Printf.printf "\n==== [[%s's constants]] ====\n\n" chunk.name_chunk;
  List.iteri (fun i c ->
    Printf.printf "%d: %s\n" i (toString c)
  ) chunk.constants;

  Printf.printf "\n==== [[%s's locals]] ====\n\n" chunk.name_chunk;
  List.iteri (fun i l -> Printf.printf "R[%d]: %s\n" i l.name_local ) chunk.locals;
  
  Printf.printf "\n==== [[%s's dissassembly]] ====\n\n" chunk.name_chunk;
  List.iteri (fun i instr ->
    Printf.printf "[%3d] %-40s; %s\n" i (print_instruction instr) (getAnnotation instr chunk);
  ) chunk.instructions;

  if List.length chunk.protos > 0 then
    Printf.printf "\n==== [[%s's protos]] ====\n\n" chunk.name_chunk;
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
  let block = _loadBlock lua_undump size in
  let str = Bytes.to_string block in
  (* On supprime le dernier caractère avec String.sub *)
  String.sub str 0 (String.length str - 1)

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
  lua_undump.root_chunk

(* Fonction pour afficher le désassemblage du bytecode *)
let print_disassembly lua_undump =
  print_chunk lua_undump.root_chunk

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
  Printf.printf "numUpvals: %d\n" chunk.numUpvals;
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

let () =
  if Array.length Sys.argv <> 2 then
    Printf.printf "Utilisation : %s <nom_fichier>\n" Sys.argv.(0)
  else
    let filename = Sys.argv.(1) in
    let chunk = load_file filename  in
    print_chunk chunk;
    (* Création de l'objet lua_dump *)
    let lua_dump = create_dump chunk in

    (* Génération du bytecode *)
    let bytecode = dump lua_dump in

    (* Sauvegarde du bytecode dans un fichier *)
    save_to_file "output.luac" bytecode