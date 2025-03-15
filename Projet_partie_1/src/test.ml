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
let _RKBCInstr = [9; 12; 13; 14; 15; 16; 17; 23; 24]  (* Correspond à SETTABLE, ADD, SUB, MUL, DIV, MOD, POW, EQ, LT *)
let _RKCInstr = [6; 11]  (* Correspond à GETTABLE, SELF *)
let _KBx = [1; 5; 7]  (* Correspond à LOADK, GETGLOBAL, SETGLOBAL *)

let _LUAMAGIC = "\x1bLua"

(* Définition de la fonction whichRK *)
let which_rk (rk: int) : bool =
  (rk land (1 lsl 8)) > 0

(* Définition de la fonction readRKasK *)
let read_rk_as_k (rk: int) : int =
  rk land (lnot (1 lsl 8))  (* Utilisation de l'opérateur lnot pour inverser le bit *)

(* Définition du type instruction *)
type instruction = {
  instr_type : instruction_type;  (* Type d'instruction : ABC, ABx, AsBx *)
  name_instr : string;                  (* Nom de l'instruction *)
  mutable opcode : int option;    (* Opcode associé, mutable car modifiable *)
  mutable a : int option;         (* Champ A (optionnel) *)
  mutable b : int option;         (* Champ B (optionnel) *)
  mutable c : int option;         (* Champ C (optionnel) *)
}

(* Constructeur pour créer une nouvelle instruction *)
let create_instruction t name_instr = {
  instr_type = t;
  name_instr = name_instr;
  opcode = None;  (* Initialisé à None, équivalent de "None" en Python *)
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
  if indx >= 0 && indx < List.length chunk.constants then
    List.nth chunk.constants indx
  else
    failwith (Printf.sprintf "Index %d out of bounds" indx)

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
  
(* Fonction pour afficher un chunk *)
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

let get_bits num p s = 
  (num lsr p) land (lnot ((lnot 0) lsl s))

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

type lua_undump = {
  mutable bytecode : int array;
  mutable index : int;
  mutable vm_version : int;
  mutable bytecode_format : int;
  mutable big_endian : bool;
  mutable int_size : int;
  mutable size_t : int;
  mutable instr_size : int;
  mutable l_number_size : int;
  mutable integral_flag : int;
  mutable root_chunk : string; (* Placeholder pour le chunk décodé *)
}

let create_lua_undump bytecode = {
  bytecode;
  index = 4; (* Alignement de l'index, saute l'en-tête *)
  vm_version = 0;
  bytecode_format = 0;
  big_endian = false;
  int_size = 0;
  size_t = 0;
  instr_size = 0;
  l_number_size = 0;
  integral_flag = 0;
  root_chunk = "";
}

let load_block lua_undump sz =
  if lua_undump.index + sz > Array.length lua_undump.bytecode then
    failwith "Malformed bytecode!"
  else
    let temp = Array.sub lua_undump.bytecode lua_undump.index sz in
    lua_undump.index <- lua_undump.index + sz;
    temp

(* Fonction sécurisée pour récupérer un octet et avancer l'index *)
let get_byte lua_undump =
  if lua_undump.index >= Array.length lua_undump.bytecode then
    failwith "Unexpected end of bytecode"
  else
    let byte = lua_undump.bytecode.(lua_undump.index) in
    lua_undump.index <- lua_undump.index + 1;
    byte

let decode_bytecode bytecode =
  if Array.length bytecode < 9 then
    failwith "Bytecode too short to be valid"
  else
    let lua_undump = create_lua_undump bytecode in

    (* Extraction des informations du bytecode *)
    lua_undump.vm_version <- get_byte lua_undump;
    lua_undump.bytecode_format <- get_byte lua_undump;
    lua_undump.big_endian <- (get_byte lua_undump = 0);
    lua_undump.int_size <- get_byte lua_undump;
    lua_undump.size_t <- get_byte lua_undump;
    lua_undump.instr_size <- get_byte lua_undump;
    lua_undump.l_number_size <- get_byte lua_undump;
    lua_undump.integral_flag <- get_byte lua_undump;

    (* Décodage du chunk principal *)
    lua_undump.root_chunk <- "chunk_decoded"; (* Placeholder pour le vrai décodage *)

    (* Affichage des informations *)
    Printf.printf "\n---- Decoded Bytecode Info ----\n";
    Printf.printf "VM Version        : %d\n" lua_undump.vm_version;
    Printf.printf "Bytecode Format   : %d\n" lua_undump.bytecode_format;
    Printf.printf "Big Endian        : %b\n" lua_undump.big_endian;
    Printf.printf "Int Size          : %d bytes\n" lua_undump.int_size;
    Printf.printf "Size_t Size       : %d bytes\n" lua_undump.size_t;
    Printf.printf "Instruction Size  : %d bytes\n" lua_undump.instr_size;
    Printf.printf "Lua Number Size   : %d bytes\n" lua_undump.l_number_size;
    Printf.printf "Integral Flag     : %d (%s)\n" 
      lua_undump.integral_flag (if lua_undump.integral_flag = 0 then "Floating point" else "Integer-based");
    Printf.printf "Root Chunk        : %s\n" lua_undump.root_chunk;
    Printf.printf "--------------------------------\n\n";

    lua_undump.root_chunk

(* Fonction pour charger un fichier bytecode Lua *)
let load_file luaCFile =
  let ic = open_in_bin luaCFile in
  let bytecode = really_input_string ic (in_channel_length ic) in
  close_in ic;
  
  if String.length bytecode < 9 then
    failwith "Lua bytecode file too short"
  else
    let bytecode_array = Array.init (String.length bytecode) (fun i -> Char.code bytecode.[i]) in
    decode_bytecode bytecode_array

(* Fonction pour afficher le désassemblage du bytecode *)
let print_disassembly lua_undump =
  print_endline lua_undump.root_chunk

(* Fonction pour décoder un bytecode brut *)
let decode_rawbytecode rawbytecode =
  (* Vérification de la taille avant de manipuler la chaîne *)
  if String.length rawbytecode < 4 then
    failwith "Raw bytecode too short"
  else if not (String.sub rawbytecode 0 4 = _LUAMAGIC) then
    raise LuaBytecodeExpected
  else
    let bytecode = Array.init (String.length rawbytecode) (fun i -> Char.code rawbytecode.[i]) in
    decode_bytecode bytecode

let get_uint32 lua_undump =
  (* Charger 4 octets depuis le bytecode *)
  let block = load_block lua_undump 4 in

  (* Convertir les 4 octets en un entier non signé *)
  let result =
    if lua_undump.big_endian then
      (* Big-endian : MSB en premier *)
      Int32.logor (Int32.shift_left (Int32.of_int block.(0)) 24)
        (Int32.logor (Int32.shift_left (Int32.of_int block.(1)) 16)
          (Int32.logor (Int32.shift_left (Int32.of_int block.(2)) 8)
            (Int32.of_int block.(3))))
    else
      (* Little-endian : LSB en premier *)
      Int32.logor (Int32.shift_left (Int32.of_int block.(3)) 24)
        (Int32.logor (Int32.shift_left (Int32.of_int block.(2)) 16)
          (Int32.logor (Int32.shift_left (Int32.of_int block.(1)) 8)
            (Int32.of_int block.(0))))
  in

  (* Retourner le résultat en tant qu'entier *)
  Int32.to_int result
  
let test_get_uint32 () =
  let lua_undump = {
    bytecode = [|0x12; 0x34; 0x56; 0x78; 0x9A; 0xBC|];
    index = 0;
    vm_version = 0;
    bytecode_format = 0;
    big_endian = true;
    int_size = 4;
    size_t = 4;
    instr_size = 4;
    l_number_size = 4;
    integral_flag = 0;
    root_chunk = "";
  } in

  let result = get_uint32 lua_undump in
  Printf.printf "Résultat (big-endian) : 0x%08X\n" result;

  lua_undump.index <- 0;
  lua_undump.big_endian <- false;

  let result_le = get_uint32 lua_undump in
  Printf.printf "Résultat (little-endian) : 0x%08X\n" result_le;()
  
(* Fonction pour convertir un bloc d'octets en entier non signé *)
type endian = BigEndian | LittleEndian

let _get_uint self =
  let order = if self.big_endian then BigEndian else LittleEndian in
  let bytes = load_block self self.int_size in
  match order with
  | BigEndian -> 
      List.fold_left (fun acc byte -> (acc lsl 8) lor byte) 0 (Array.to_list bytes)
  | LittleEndian ->
      List.fold_right (fun byte acc -> (acc lsl 8) lor byte) (Array.to_list bytes) 0

  
(* Fonction pour lire un entier non signé de taille size_t *)
let _get_size_t self =
  let order = if self.big_endian then BigEndian else LittleEndian in
  let bytes = load_block self self.size_t in
  match order with
  | BigEndian -> 
      List.fold_left (fun acc byte -> (acc lsl 8) lor byte) 0 (Array.to_list bytes)
  | LittleEndian ->
      List.fold_right (fun byte acc -> (acc lsl 8) lor byte) (Array.to_list bytes) 0

(* Convertit un tableau d'entiers (0-255) en une chaîne de caractères *)
let array_to_string arr =
  String.init (Array.length arr) (fun i -> Char.chr arr.(i))

(* Convertit un bloc binaire en float64 (IEEE 754) *)
let bytes_to_float64 big_endian bytes =
  if String.length bytes <> 8 then invalid_arg "Invalid block size";
  let b = Bytes.of_string bytes in
  let i64 =
    if big_endian then
      Int64.logor
        (shift_left (of_int (Char.code (Bytes.get b 0))) 56)
        (shift_left (of_int (Char.code (Bytes.get b 1))) 48)
      |> logor (shift_left (of_int (Char.code (Bytes.get b 2))) 40)
      |> logor (shift_left (of_int (Char.code (Bytes.get b 3))) 32)
      |> logor (shift_left (of_int (Char.code (Bytes.get b 4))) 24)
      |> logor (shift_left (of_int (Char.code (Bytes.get b 5))) 16)
      |> logor (shift_left (of_int (Char.code (Bytes.get b 6))) 8)
      |> logor (of_int (Char.code (Bytes.get b 7)))
    else
      Int64.logor
        (of_int (Char.code (Bytes.get b 0)))
        (shift_left (of_int (Char.code (Bytes.get b 1))) 8)
      |> logor (shift_left (of_int (Char.code (Bytes.get b 2))) 16)
      |> logor (shift_left (of_int (Char.code (Bytes.get b 3))) 24)
      |> logor (shift_left (of_int (Char.code (Bytes.get b 4))) 32)
      |> logor (shift_left (of_int (Char.code (Bytes.get b 5))) 40)
      |> logor (shift_left (of_int (Char.code (Bytes.get b 6))) 48)
      |> logor (shift_left (of_int (Char.code (Bytes.get b 7))) 56)
  in
  Int64.float_of_bits i64

(* Fonction principale _get_double utilisant load_block *)
let _get_double lua_undump : float =
  let block = load_block lua_undump 8 in
  let data = array_to_string block in
  bytes_to_float64 lua_undump.big_endian data

(* Fonction pour extraire une chaîne de caractères d'un bloc donné *)
let _get_string lua_undump size : string =
  let block = load_block lua_undump size in
  let str = array_to_string block in
  (* On supprime le dernier caractère avec String.sub *)
  String.sub str 0 (String.length str - 1)







let rec decode_chunk lua_undump =
    let chunk = create_chunk () in
  
    (* Informations du chunk *)
    let size = lua_undump.size_t + 2 in
    Printf.printf "size_t: %d\n" size;
    chunk.name_chunk <- _get_string lua_undump size;
    Printf.printf "Chunk name: %s\n" chunk.name_chunk;

    chunk.frst_line <- _get_uint lua_undump;
    Printf.printf "First line: %d\n" chunk.frst_line;

    chunk.last_line <- _get_uint lua_undump;
    Printf.printf "Last line: %d\n" chunk.last_line;
    chunk.numUpvals <- get_byte lua_undump;
    Printf.printf "Number of upvalues: %d\n" chunk.numUpvals;
    chunk.numParams <- get_byte lua_undump;
    Printf.printf "Number of parameters: %d\n" chunk.numParams;
    chunk.isVarg <- (get_byte lua_undump) <> 0;
    Printf.printf "Is vararg: %b\n" chunk.isVarg;
    chunk.maxStack <- get_byte lua_undump;
    Printf.printf "Max stack size: %d\n" chunk.maxStack;
  
    (* Instructions *)
    let num_instr = _get_uint lua_undump in
    for _ = 1 to num_instr do
      let instr = decode_instr (get_uint32 lua_undump) in
      append_instruction chunk instr
    done;
  
    (* Constantes *)
    let num_consts = _get_uint lua_undump in
    for _ = 1 to num_consts do
      let ctype = get_byte lua_undump in
      let constant = match ctype with
        | 0 -> { type_const = NIL; data = "" }
        | 1 -> { type_const = BOOL; data = string_of_bool ((get_byte lua_undump) <> 0) }
        | 3 -> { type_const = NUMBER; data = string_of_float (_get_double lua_undump) }
        | 4 -> let size = _get_uint lua_undump in { type_const = STRING; data = _get_string lua_undump size }
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
  
let test_decode_chunk () =
  let lua_undump = {
    bytecode = [|
  27; 76; 117; 97; 1; 0; 0; 0; 81; 0; 1; 4; 4; 4; 8; 0; 
  25; 147; 13; 10; 26; 10; 0; 0; 0; 0; 0; 0; 0; 2; 0; 0; 
  0; 0; 0; 0; 0; 0; 1; 4; 0; 0; 0; 6; 64; 0; 0; 0; 
  0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0; 37; 64; 0; 0
|];
    index = 0;
    vm_version = 0;
    bytecode_format = 0;
    big_endian = false;
    int_size = 4;
    size_t = 4;
    instr_size = 4;
    l_number_size = 8;
    integral_flag = 0;
    root_chunk = "";
  } in
  let chunk = decode_chunk lua_undump in
  Printf.printf "Chunk name: %s\n" chunk.name_chunk;
  Printf.printf "First line: %d\n" chunk.frst_line;
  Printf.printf "Last line: %d\n" chunk.last_line;
  Printf.printf "Number of upvalues: %d\n" chunk.numUpvals;
  Printf.printf "Number of parameters: %d\n" chunk.numParams;
  Printf.printf "Is vararg: %b\n" chunk.isVarg;
  Printf.printf "Max stack size: %d\n" chunk.maxStack;
  Printf.printf "Number of instructions: %d\n" (List.length chunk.instructions);
  Printf.printf "Number of constants: %d\n" (List.length chunk.constants);
  Printf.printf "Number of prototypes: %d\n" (List.length chunk.protos);
  Printf.printf "Number of locals: %d\n" (List.length chunk.locals);
  Printf.printf "Number of upvalues: %d\n" (List.length chunk.upvalues)



























  


(*LuaDump*)
let set_uint32 big_endian i =
  let bytes = Bytes.create 4 in
  if big_endian then
    Bytes.set_int32_be bytes 0 (Int32.of_int i)
  else
    Bytes.set_int32_le bytes 0 (Int32.of_int i);
  bytes


let set_float64 big_endian f =
  let bytes = Bytes.create 8 in
  let bits = Int64.bits_of_float f in
  if big_endian then
    Bytes.set_int64_be bytes 0 bits
  else
    Bytes.set_int64_le bytes 0 bits;
  bytes

let write_null_terminated_string write_block str =
  write_block (Bytes.of_string str);
  write_block (Bytes.make 1 (Char.chr 0))

let rec dump_chunk ~big_endian ~write_block ~encode_instr chunk =
  write_null_terminated_string write_block chunk.name_chunk;
  write_block (set_uint32 big_endian chunk.frst_line);
  write_block (set_uint32 big_endian chunk.last_line);
  write_block (Bytes.make 1 (Char.chr chunk.numUpvals));
  write_block (Bytes.make 1 (Char.chr chunk.numParams));
  write_block (Bytes.make 1 (Char.chr (if chunk.isVarg then 1 else 0)));
  write_block (Bytes.make 1 (Char.chr chunk.maxStack));

  write_block (set_uint32 big_endian (List.length chunk.instructions));
  List.iter (fun instr ->
    write_block (set_uint32 big_endian (encode_instr instr))
  ) chunk.instructions;
  
  write_block (set_uint32 big_endian (List.length chunk.constants));
  List.iter (fun c ->
    match c.type_const with
    | NIL -> write_block (Bytes.make 1 (Char.chr 0))
    | BOOL ->
        write_block (Bytes.make 1 (Char.chr 1));
        write_block (Bytes.make 1 (Char.chr (if c.data = "true" then 1 else 0)))
    | NUMBER ->
        write_block (Bytes.make 1 (Char.chr 3));
        write_block (set_float64 big_endian (float_of_string c.data))
    | STRING ->
        write_block (Bytes.make 1 (Char.chr 4));
        write_null_terminated_string write_block c.data
  ) chunk.constants;

  write_block (set_uint32 big_endian (List.length chunk.protos));
  List.iter (dump_chunk ~big_endian ~write_block ~encode_instr) chunk.protos;

  write_block (set_uint32 big_endian (List.length chunk.lineNums));
  List.iter (fun l -> write_block (set_uint32 big_endian l)) chunk.lineNums;

  write_block (set_uint32 big_endian (List.length chunk.locals));
  List.iter (fun local ->
    write_null_terminated_string write_block local.name_local;
    write_block (set_uint32 big_endian local.start);
    write_block (set_uint32 big_endian local.end_pc)
  ) chunk.locals;

  write_block (set_uint32 big_endian (List.length chunk.upvalues));
  List.iter (write_null_terminated_string write_block) chunk.upvalues

let dump_header ~big_endian ~write_block ~vm_version ~bytecode_format ~int_size ~size_t ~instr_size ~l_number_size ~integral_flag =
  let lua_magic = "\x1bLua" in
  write_block (Bytes.of_string lua_magic);
  write_block (Bytes.make 1 (Char.chr vm_version));
  write_block (Bytes.make 1 (Char.chr bytecode_format));
  write_block (Bytes.make 1 (Char.chr (if big_endian then 0 else 1)));
  write_block (Bytes.make 1 (Char.chr int_size));
  write_block (Bytes.make 1 (Char.chr size_t));
  write_block (Bytes.make 1 (Char.chr instr_size));
  write_block (Bytes.make 1 (Char.chr l_number_size));
  write_block (Bytes.make 1 (Char.chr integral_flag))

let dump ~big_endian ~write_block ~encode_instr root_chunk =
  dump_header ~big_endian ~write_block ~vm_version:0x51 ~bytecode_format:0 ~int_size:4 ~size_t:8 ~instr_size:4 ~l_number_size:8 ~integral_flag:0;
  dump_chunk ~big_endian ~write_block ~encode_instr root_chunk

let test_write_block buffer data =
  Buffer.add_bytes buffer data

let test_encode_instr instr = match instr.opcode with
  | Some opcode -> opcode
  | None -> 0

let hex_of_bytes bytes =
  Bytes.iter (fun c -> Printf.printf "%02x " (Char.code c)) bytes;
  print_newline ()
  
let save_to_file filename buffer =
  let oc = open_out_bin filename in
  output_bytes oc (Buffer.to_bytes buffer);
  close_out oc;
  Printf.printf "Bytecode écrit dans : %s\n" filename

let () =
  let buffer = Buffer.create 1024 in
  let chunk = {
    name_chunk = "main";
    frst_line = 1;
    last_line = 10;
    numUpvals = 0;
    numParams = 2;
    isVarg = false;
    maxStack = 5;
    instructions = [
      {instr_type = ABC; name_instr = "LOADK"; opcode = Some 1; a = Some 0; b = Some 1; c = None};
      {instr_type = ABC; name_instr = "ADD"; opcode = Some 2; a = Some 0; b = Some 1; c = Some 2};
      {instr_type = ABC; name_instr = "RETURN"; opcode = Some 3; a = Some 0; b = None; c = None}
    ];
    constants = [{type_const = NUMBER; data = "3.14"}];
    protos = [];
    lineNums = [1; 2; 3];
    locals = [{name_local = "x"; start = 0; end_pc = 10}];
    upvalues = ["env"];
  } in
  dump ~big_endian:true ~write_block:(test_write_block buffer) ~encode_instr:test_encode_instr chunk;
  let output_file = "output.luac" in
  let oc = open_out_bin output_file in
  output_bytes oc (Bytes.of_string (Buffer.contents buffer));
  close_out oc;
  Printf.printf "Bytecode écrit dans : %s\n" output_file;