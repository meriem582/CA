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
  Printf.printf "%s : %s\n" instr_name regs

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
  chunk.instructions <- instr :: chunk.instructions

(* Fonction pour ajouter une constante à un chunk *)
let append_constant chunk const =
  chunk.constants <- const :: chunk.constants

(* Fonction pour ajouter un proto à un chunk *)
let append_proto chunk proto =
  chunk.protos <- proto :: chunk.protos

(* Fonction pour ajouter une ligne à un chunk *)
let append_line chunk line =
  chunk.lineNums <- line :: chunk.lineNums

(* Fonction pour ajouter une variable locale à un chunk *)
let append_local chunk local =
  chunk.locals <- local :: chunk.locals

(* Fonction pour ajouter un upvalue à un chunk *)
let append_upval chunk upval =
  chunk.upvalues <- upval :: chunk.upvalues

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
  | Some 21 -> let count = (match instr.c with Some v -> v | None -> 0) - (match instr.b with Some v -> v | None -> 0) + 1 in
               Printf.sprintf "concat %d values from R[%d] to R[%d], store into R[%d]" count (match instr.b with Some v -> v | None -> 0) (match instr.c with Some v -> v | None -> 0) (match instr.a with Some v -> v | None -> 0)
  | _ -> ""
  
(* Fonction pour afficher un block *)
let rec print_chunk chunk =
  Printf.printf "\n==== [[%s's constants]] ====\n" chunk.name_chunk;
  List.iteri (fun i c ->
    Printf.printf "%d: %s\n" i (toString c)
  ) chunk.constants;

  Printf.printf "\n==== [[%s's locals]] ====\n" chunk.name_chunk;
  List.iteri (fun i l -> Printf.printf "R[%d]: %s\n" i l.name_local ) chunk.locals;
  
  Printf.printf "\n==== [[%s's dissassembly]] ====\n" chunk.name_chunk;
  List.iteri (fun i instr ->
    Printf.printf "[%3d] %-40s; %s\n" i instr.name_instr (getAnnotation instr chunk);
  ) chunk.instructions;

  if List.length chunk.protos > 0 then
    Printf.printf "\n==== [[%s's protos]] ====\n" chunk.name_chunk;
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

(* Fonction pour encoder une instruction *)
let _encode_instr instr =
  let data = 0 in
  let data = set_bits data (Option.get instr.opcode) 0 6 in
  let data = set_bits data (Option.get instr.a) 6 8 in
  let data = match instr.instr_type with
    | ABC ->
        let data = set_bits data (Option.get instr.b) 23 9 in
        set_bits data (Option.get instr.c) 14 9
    | ABx ->
        set_bits data (Option.get instr.b) 14 18
    | AsBx ->
        set_bits data (Option.get instr.b + 131071) 14 18
  in
  data

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
    for _ = 1 to num_lines do
      ignore (_get_uint lua_undump)
    done;
  
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
    
let () =
  if Array.length Sys.argv <> 2 then
    Printf.printf "Utilisation : %s <nom_fichier>\n" Sys.argv.(0)
  else
    let filename = Sys.argv.(1) in
    let chunk = load_file filename  in
    print_chunk chunk