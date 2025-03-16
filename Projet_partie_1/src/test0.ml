exception LuaBytecodeExpected
open Bigarray
open Bytes


type instruction_type =
  | ABC
  | ABx
  | AsBx

  type opcode =
  | MOVE        (* 0 *)
  | LOADK       (* 1 *)
  | LOADBOOL    (* 2 *)
  | LOADNIL     (* 3 *)
  | GETUPVAL    (* 4 *)
  | GETGLOBAL   (* 5 *)
  | GETTABLE    (* 6 *)
  | SETGLOBAL   (* 7 *)
  | SETUPVAL    (* 8 *)
  | SETTABLE    (* 9 *)
  | NEWTABLE    (* 10 *)
  | SELF        (* 11 *)
  | ADD         (* 12 *)
  | SUB         (* 13 *)
  | MUL         (* 14 *)
  | DIV         (* 15 *)
  | MOD         (* 16 *)
  | POW         (* 17 *)
  | UNM         (* 18 *)
  | NOT         (* 19 *)
  | LEN         (* 20 *)
  | CONCAT      (* 21 *)
  | JMP         (* 22 *)
  | EQ          (* 23 *)
  | LT          (* 24 *)
  | LE          (* 25 *)
  | TEST        (* 26 *)
  | TESTSET     (* 27 *)
  | CALL        (* 28 *)
  | TAILCALL    (* 29 *)
  | RETURN      (* 30 *)
  | FORLOOP     (* 31 *)
  | FORPREP     (* 32 *)
  | TFORLOOP    (* 33 *)
  | SETLIST     (* 34 *)
  | CLOSE       (* 35 *)
  | CLOSURE     (* 36 *)
  | VARARG      (* 37 *)

  (*Fonction qui transforme l'opcode en entier*)
let opcode_to_int = function
  | MOVE -> 0 | LOADK -> 1 | LOADBOOL -> 2 | LOADNIL -> 3
  | GETUPVAL -> 4 | GETGLOBAL -> 5 | GETTABLE -> 6 | SETGLOBAL -> 7
  | SETUPVAL -> 8 | SETTABLE -> 9 | NEWTABLE -> 10 | SELF -> 11
  | ADD -> 12 | SUB -> 13 | MUL -> 14 | DIV -> 15
  | MOD -> 16 | POW -> 17 | UNM -> 18 | NOT -> 19
  | LEN -> 20 | CONCAT -> 21 | JMP -> 22 | EQ -> 23
  | LT -> 24 | LE -> 25 | TEST -> 26 | TESTSET -> 27
  | CALL -> 28 | TAILCALL -> 29 | RETURN -> 30 | FORLOOP -> 31
  | FORPREP -> 32 | TFORLOOP -> 33 | SETLIST -> 34 | CLOSE -> 35
  | CLOSURE -> 36 | VARARG -> 37

  (*Fonction qui transforme l'entier en opcode  *)
  let int_to_opcode = function
  | 0 -> MOVE | 1 -> LOADK | 2 -> LOADBOOL | 3 -> LOADNIL
  | 4 -> GETUPVAL | 5 -> GETGLOBAL | 6 -> GETTABLE | 7 -> SETGLOBAL
  | 8 -> SETUPVAL | 9 -> SETTABLE | 10 -> NEWTABLE | 11 -> SELF
  | 12 -> ADD | 13 -> SUB | 14 -> MUL | 15 -> DIV
  | 16 -> MOD | 17 -> POW | 18 -> UNM | 19 -> NOT
  | 20 -> LEN | 21 -> CONCAT | 22 -> JMP | 23 -> EQ
  | 24 -> LT | 25 -> LE | 26 -> TEST | 27 -> TESTSET
  | 28 -> CALL | 29 -> TAILCALL | 30 -> RETURN | 31 -> FORLOOP
  | 32 -> FORPREP | 33 -> TFORLOOP | 34 -> SETLIST | 35 -> CLOSE
  | 36 -> CLOSURE | 37 -> VARARG
  | _ -> failwith "Opcode inconnu"

  (* Fonction pour afficher un opcode sous forme de chaîne *)


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


  type const_type =
  | NIL      (* 0 *)
  | BOOL     (* 1 *)
  | NUMBER   (* 3 *)
  | STRING   (* 4 *)
  let string_of_const_type = function
  | NIL -> "NIL"
  | BOOL -> "BOOL"
  | NUMBER -> "NUMBER"
  | STRING -> "STRING"
  let int_to_const_type = function
  | 0 -> NIL
  | 1 -> BOOL
  | 3 -> NUMBER
  | 4 -> STRING
  | _ -> failwith "Type de constante inconnu"


  (*Toute les instructions qui sont de type ABC *)
  let rkbc_instr = [
    SETTABLE; ADD; SUB; MUL; DIV; MOD; POW; EQ; LT
  ]

  (*Toute les instructions qui sont de type ABx *)
  let rkc_instr = [
    GETTABLE; SELF
  ]
  
  (*Toute les instructions qui sont de type AsBx *)
  let kbx_instr = [
    LOADK; GETGLOBAL; SETGLOBAL
  ]
  
  let luamagic = "\x1bLua"

  (*Cette fonction permet de distinguer les registres des constantes en vérifiant le 9 ème bit.*)
  (* si c'est le 9 ème bit= 0 alors rk est un registre. *)
  (* si c'est le 9 ème bit= 1 alors rk est une constante. *)
  let which_rk rk = (rk land (1 lsl 8)) > 0

  (*Cette fonction permet d'extraire la valeur réel de rk *)
  (* si rk est une constante alors on extrait la valeur de la constante en mettons le 9 ème bit à zéro. *)
  (* Sinon si rk est un registre alors on a déjà la valeur de registre vu qu'on le 9 ème bit est à zéro déjà. *)
  let readRKasK rk = rk land lnot (1 lsl 8)
   

  (* Définition du type instruction *)
  type instruction = {
  instr_type : instruction_type;  (* Type d'instruction : ABC, ABx, AsBx *)
  name_instr : string;                  (* Nom de l'instruction *)
  mutable opcode : int option;    (* Opcode associé, mutable car modifiable *)
  mutable a : int option;         (* Champ A (optionnel) *)
  mutable b : int option;         (* Champ B (optionnel) *)
  mutable c : int option;         (* Champ C (optionnel) *)
}

(* permet d'intialiser une instruction *)
let create_instruction type_instr name_instruction = {
  instr_type = type_instr;
  name_instr = name_instruction;
  opcode = None; 
  a = None;
  b = None;
  c = None;
}

(* Cette fonction permet d'afficher le registre ou la constante selon la fonction  which_rk *)
let formatRK (rk: int) : string =
  if which_rk rk then
    "K[" ^ string_of_int (readRKasK rk) ^ "]"
  else
    "R[" ^ string_of_int rk ^ "]"

    let print_instruction instr =
      (* Formatage du nom de l'instruction *)
      let instr_str = Printf.sprintf "%10s" instr.name_instr in
    
      (* Formatage des registres/constantes en fonction du type d'instruction *)
      let regs =
        match instr.instr_type with
        | ABC ->
            (* On extrait a, b et c avec une valeur par défaut si None *)
            let a_str =
              match instr.a with
              | Some v -> string_of_int v
              | None -> ""
            in
            let op = int_to_opcode (Option.get instr.opcode) in
            begin
              match (instr.b, instr.c) with
              | (Some b_val, Some c_val) -> 
                  if List.mem op rkbc_instr then 
                    Printf.sprintf "%6s %6s %6s"
                      ("R[" ^ a_str ^ "]")
                      (formatRK b_val)
                      (formatRK c_val)
                  else if List.mem op rkc_instr then
                    Printf.sprintf "%6s %6s %6s"
                      ("R[" ^ a_str ^ "]")
                      (string_of_int b_val)
                      (formatRK c_val)
                  else
                    Printf.sprintf "%6s %6s %6s"
                      a_str
                      (string_of_int b_val)
                      (string_of_int c_val)
              | _ -> ""
            end
        | ABx | AsBx ->
            let a_str = Printf.sprintf "R[%d]" (Option.get instr.a) in
            let b_val = Option.get instr.b in
            let op = int_to_opcode (Option.get instr.opcode) in
            let b_str =
              if List.mem op kbx_instr then
                Printf.sprintf "K[%d]" b_val
              else
                string_of_int b_val
            in
            Printf.sprintf "%6s %6s" a_str b_str
      in
    
      Printf.printf "%s : %s\n" instr_str regs



type constant ={
  type_const : const_type;
  data : string;
  }
    
let create_constant t data = {
  type_const = t;
  data = data;
  }
      
let print_constante c =
  Printf.sprintf "[%s] %s" ( string_of_const_type c.type_const) c.data

let toCode c =
  match c.type_const with
  | NIL -> "nil"
  | BOOL -> if c.data = "true" then "true" else "false"
  | NUMBER -> c.data 
  | STRING -> "" ^ c.data ^ ""

type local = {
    name_local : string;
    start : int;
    end_pc : int;
  }
  
let create_local name_local start end_pc = {
    name_local = name_local;
    start = start;
    end_pc = end_pc;
  }


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

let append_instruction chunk instr =
  chunk.instructions <- chunk.instructions @  [instr]
let append_constant chunk const =
   chunk.constants <- chunk.constants @ [const] 
let append_proto chunk proto =
chunk.protos <- chunk.protos @ [proto ] 

let append_upvalue chunk upvalue =
  chunk.upvalues <- chunk.upvalues @ [upvalue] 

let append_line chunk line =
  chunk.lineNums <- chunk.lineNums @ [line] 

let append_local chunk local =
  chunk.locals <- chunk.locals @ [local] 

let find_local chunk pc =
  let rec find_local_aux locals pc =
    match locals with
    | [] -> None
    | l :: q -> if l.start <= pc && l.end_pc >= pc then Some l else find_local_aux q pc
  in
  find_local_aux chunk.locals pc

(* cette fonction permet de chercher une constante dans un bloc selon son index *)
let get_constant chunk indx =
  if indx >= 0 && indx < List.length chunk.constants then
    List.nth chunk.constants indx
    else
    failwith (Printf.sprintf "Index %d out of bounds" indx)

    let getAnnotation instr chunk =
      match instr.opcode with
      | None -> "Invalid instruction (missing opcode)"
      | Some opcode ->
          let a = match instr.a with Some v -> v | None -> 0 in
          let b = match instr.b with Some v -> v | None -> 0 in
          let c = match instr.c with Some v -> v | None -> 0 in
          match opcode with
          | 0 -> (* MOVE *)
              Printf.sprintf "move R[%d] into R[%d]" b a
          | 1 -> (* LOADK *)
              Printf.sprintf "load %s into R[%d]" 
                (toCode (get_constant chunk b)) a
          | 2 -> (* LOADBOOL *)
              Printf.sprintf "load boolean %b into R[%d]" (b <> 0) a
          | 3 -> (* LOADNIL *)
              Printf.sprintf "load nil into R[%d]" a
          | 4 -> (* GETUPVAL *)
              Printf.sprintf "get upvalue %d into R[%d]" b a
          | 5 -> (* GETGLOBAL *)
              Printf.sprintf "get global %s into R[%d]" 
                (toCode (get_constant chunk b)) a
          | 6 -> (* GETTABLE *)
              Printf.sprintf "get table element %s[%s] into R[%d]" 
                (formatRK b) (formatRK c) a
          | 7 -> (* SETGLOBAL *)
              Printf.sprintf "set global %s from R[%d]" 
                (toCode (get_constant chunk b)) a
          | 8 -> (* SETUPVAL *)
              Printf.sprintf "set upvalue %d from R[%d]" b a
          | 9 -> (* SETTABLE *)
              Printf.sprintf "set table element %s[%s] = %s" 
                (formatRK a) (formatRK b) (formatRK c)
          | 10 -> (* NEWTABLE *)
              Printf.sprintf "create new table in R[%d]" a
          | 11 -> (* SELF *)
              Printf.sprintf "prepare method call: R[%d] = R[%d], R[%d] = function" 
                a b (a + 1)
          | 12 -> (* ADD *)
              Printf.sprintf "add %s to %s, store in R[%d]" 
                (formatRK b) (formatRK c) a
          | 13 -> (* SUB *)
              Printf.sprintf "subtract %s from %s, store in R[%d]" 
                (formatRK c) (formatRK b) a
          | 14 -> (* MUL *)
              Printf.sprintf "multiply %s by %s, store in R[%d]" 
                (formatRK b) (formatRK c) a
          | 15 -> (* DIV *)
              Printf.sprintf "divide %s by %s, store in R[%d]" 
                (formatRK b) (formatRK c) a
          | 16 -> (* MOD *)
              Printf.sprintf "modulo %s by %s, store in R[%d]" 
                (formatRK b) (formatRK c) a
          | 17 -> (* POW *)
              Printf.sprintf "raise %s to the power of %s, store in R[%d]" 
                (formatRK b) (formatRK c) a
          | 18 -> (* UNM *)
              Printf.sprintf "negate %s, store in R[%d]" 
                (formatRK b) a
          | 19 -> (* NOT *)
              Printf.sprintf "logical NOT of %s, store in R[%d]" 
                (formatRK b) a
          | 20 -> (* LEN *)
              Printf.sprintf "length of %s, store in R[%d]" 
                (formatRK b) a
          | 21 -> (* CONCAT *)
              Printf.sprintf "concat values from R[%d] to R[%d], store in R[%d]" 
                b c a
          | 22 -> (* JMP *)
              Printf.sprintf "jump by %d instructions" (b - 131071)
          | 23 -> (* EQ *)
              Printf.sprintf "compare %s == %s, jump if %b" 
                (formatRK b) (formatRK c) (a <> 0)
          | 24 -> (* LT *)
              Printf.sprintf "compare %s < %s, jump if %b" 
                (formatRK b) (formatRK c) (a <> 0)
          | 25 -> (* LE *)
              Printf.sprintf "compare %s <= %s, jump if %b" 
                (formatRK b) (formatRK c) (a <> 0)
          | 26 -> (* TEST *)
              Printf.sprintf "test R[%d], jump if %b" a (c <> 0)
          | 27 -> (* TESTSET *)
              Printf.sprintf "test R[%d], jump if %b and move R[%d] into R[%d]" 
                b (c <> 0) b a
          | 28 -> (* CALL *)
              Printf.sprintf "call function in R[%d] with %d args, %d results" 
                a b c
          | 29 -> (* TAILCALL *)
              Printf.sprintf "tail call function in R[%d] with %d args" 
                a b
          | 30 -> (* RETURN *)
              Printf.sprintf "return %d values from R[%d]" b a
          | 31 -> (* FORLOOP *)
              Printf.sprintf "for loop: increment R[%d], jump by %d" 
                a (b - 131071)
          | 32 -> (* FORPREP *)
              Printf.sprintf "for loop preparation: R[%d], jump by %d" 
                a (b - 131071)
          | 33 -> (* TFORLOOP *)
              Printf.sprintf "generic for loop: R[%d], jump by %d" 
                a (c - 131071)
          | 34 -> (* SETLIST *)
              Printf.sprintf "set list elements in R[%d], starting at index %d" 
                a b
          | 35 -> (* CLOSE *)
              Printf.sprintf "close upvalues for R[%d]" a
          | 36 -> (* CLOSURE *)
              Printf.sprintf "create closure from proto %d, store in R[%d]" 
                b a
          | 37 -> (* VARARG *)
              Printf.sprintf "load vararg into R[%d]" a
          | _ -> "Instruction not found"
          
    let rec print_chunk chunk =
      Printf.printf "Chunk %s\n" chunk.name_chunk;
      Printf.printf "First line: %d\n" chunk.frst_line;
      Printf.printf "Last line: %d\n" chunk.last_line;
      Printf.printf "Number of upvalues: %d\n" chunk.numUpvals;
      Printf.printf "Number of parameters: %d\n" chunk.numParams;
      Printf.printf "Is vararg: %b\n" chunk.isVarg;
      Printf.printf "Max stack: %d\n" chunk.maxStack;
      Printf.printf "Upvalues: %s\n" (String.concat ", " chunk.upvalues);
      Printf.printf "Line numbers: %s\n" (String.concat ", " (List.map string_of_int chunk.lineNums));
      Printf.printf "Locals:\n";
      List.iter (fun l -> Printf.printf "  %s [%d, %d]\n" l.name_local l.start l.end_pc) chunk.locals;
      Printf.printf "Constants:\n";
      List.iter (fun c -> Printf.printf "  %s\n" (print_constante c)) chunk.constants;
      Printf.printf "Instructions:\n";
      List.iter (fun i -> Printf.printf "  %s\n" (getAnnotation i chunk)) chunk.instructions;
      Printf.printf "Protos:\n";
      List.iter print_chunk chunk.protos

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


(* cette fonction permet d'extraire les bits de p à p+s de num  *)
let get_bits num p s = (num lsr p) land (lnot ((lnot 0) lsl s)) 


(* cette fonction permet de mettre les bits de p à p+s de num à data *)
let set_bits num data p s =
  (num land (lnot ((lnot ((lnot 0) lsl s)) lsl p))) lor
  ((data lsl p) land ((lnot ((lnot 0) lsl s)) lsl p))

  
  (*Cette fonction permet de décoder une instruction *)
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
let encode_instr instr =
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
            Printf.printf "opcode=%s, a=%d, b=%d, c=%d\n" (opcode_to_string (int_to_opcode opcode)) a b c;
            data
        | ABx  -> 
            let b = match instr.b with Some b -> b | None -> 0 in
            let data = set_bits data b 14 18 in
            Printf.printf "opcode=%s, a=%d, b=%d\n" (opcode_to_string (int_to_opcode opcode)) a b ;
            data
        | AsBx -> 
            let b = match instr.b with Some b -> b | None -> 0 in
            let data = set_bits data (b + 131071) 14 18 in
            Printf.printf "opcode=%s, a=%d, b=%d\n" (opcode_to_string (int_to_opcode opcode)) a b ;
            data
      in
      Some data
      | _ -> 
        let error_msg =
          "Instruction encoding failed: " ^
          (match instr.opcode with Some o -> Printf.sprintf "opcode=%d " o | None -> "opcode=None ") ^
          (match instr.a with Some a -> Printf.sprintf "a=%d " a | None -> "a=None ") ^
          (match instr.b with Some b -> Printf.sprintf "b=%d " b | None -> "b=None ") ^
          (match instr.c with Some c -> Printf.sprintf "c=%d" c | None -> "c=None")
        in
        Printf.printf "%s\n" error_msg;
        None        
        

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
  mutable root_chunk : chunk; (* Placeholder pour le chunk décodé *)
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
  root_chunk = create_chunk ();
}

(* cette fonction permet de charger un bloc de taille sz  *)

let load_block lua_undump sz =
  Printf.printf "Loading block of size %d at index %d\n" sz lua_undump.index;
  if lua_undump.index + sz > Bytes.length lua_undump.bytecode || sz < 0 then
    failwith (Printf.sprintf "Malformed bytecode! Index: %d, Size: %d, Bytecode length: %d" lua_undump.index sz (Bytes.length lua_undump.bytecode))
  else
    let temp = Bytes.sub lua_undump.bytecode lua_undump.index sz in
    Printf.printf "Block content: %s\n" (String.concat "; " (Array.to_list (Array.map (fun c -> string_of_int (int_of_char (Bytes.get temp c))) (Array.init (Bytes.length temp) (fun i -> i)))));
    lua_undump.index <- lua_undump.index + sz;
    temp


    (* cette fonction permet de lire un octet du bytecode *)
    let get_byte (state : lua_undump) : int =
      let b = Bytes.get state.bytecode state.index in
      state.index <- state.index + 1;
      Char.code b  (* Convertit le char en int *)
    

(* Cette fonction permet de lire 4 octet de byte code et faire avancer l'index *)
(* elle convertit ces octets en un entier 32 bits non signé en tenant compte de l'endianness (big-endian ou little-endian). *)
let get_uint32 (lua_undump : lua_undump) : int =
  if lua_undump.index + 4 > Bytes.length lua_undump.bytecode then
    failwith "Unexpected end of bytecode"
  else
    (* Récupération des 4 octets *)
    let b0 = Char.code (Bytes.get lua_undump.bytecode lua_undump.index) in
    let b1 = Char.code (Bytes.get lua_undump.bytecode (lua_undump.index + 1)) in
    let b2 = Char.code (Bytes.get lua_undump.bytecode (lua_undump.index + 2)) in
    let b3 = Char.code (Bytes.get lua_undump.bytecode (lua_undump.index + 3)) in
    
    (* Mise à jour de l'index *)
    lua_undump.index <- lua_undump.index + 4;

    (* Gestion de l'endianess *)
    if lua_undump.big_endian then
      (b0 lsl 24) lor (b1 lsl 16) lor (b2 lsl 8) lor b3
    else
      (b3 lsl 24) lor (b2 lsl 16) lor (b1 lsl 8) lor b0

(* Cette fonction permet de lire un entier non signé de taille state.int_size *)
(* elle extrait un bloc de bytes de taille state.int_size *)
(* elle convertit le bloc en entier non signé en tenant compte de l'endianness *)
let get_uint (state : lua_undump) : int =
  Printf.printf "int_size: %d\n" state.int_size;
  
  if state.int_size <= 0 || state.int_size > 8 then
    failwith "Invalid int_size value"
  else if state.index + state.int_size > Bytes.length state.bytecode then
    failwith "Malformed bytecode!"
  else
    (* Lire les octets un par un en tant qu'entiers *)
    let block = Array.init state.int_size (fun i ->
      Char.code (Bytes.get state.bytecode (state.index + i))
    ) in

    (* Avancer l'index *)
    state.index <- state.index + state.int_size;

    (* Affichage des octets lus *)
    Printf.printf "block: %s\n"
      (String.concat "; " (Array.to_list (Array.map string_of_int block)));

    (* Calcul du résultat *)
    let result = ref 0 in
    if state.big_endian then
      (* Big-endian: l'octet le plus significatif en premier *)
      Array.iter (fun b -> result := (!result lsl 8) lor b) block
    else
      (* Little-endian: l'octet le moins significatif en premier *)
      Array.iteri (fun i b -> result := !result lor (b lsl (8 * i))) block;

    !result


(* Cette fonction permet de lire un entier non signé de taille state.size_t *)
(* elle extrait un bloc de bytes de taille state.size_t *)
(* elle convertit le bloc en entier non signé en tenant compte de l'endianness *)
            

let get_double (state : lua_undump) : float =
  (* Vérification que l_number_size est valide *)
  if state.l_number_size <> 8 then
    failwith "Invalid l_number_size value (expected 8 for double)"
  else if state.index + state.l_number_size > Bytes.length state.bytecode then
    failwith "Malformed bytecode!"
  else
    (* Extraction des 8 octets sous forme d'un tableau d'entiers *)
    let block = Array.init state.l_number_size (fun i ->
      Char.code (Bytes.get state.bytecode (state.index + i))
    ) in

    (* Mise à jour de l'index *)
    state.index <- state.index + state.l_number_size;

    (* Conversion du tableau d'octets en un Int64 *)
    let open Int64 in
    let i =
      if state.big_endian then
        (* Big-endian: l'octet le plus significatif en premier *)
        Array.fold_left (fun acc byte -> logor (shift_left acc 8) (of_int byte)) zero block
      else
        (* Little-endian: l'octet le moins significatif en premier *)
        Array.fold_left (fun acc (i, byte) -> logor acc (shift_left (of_int byte) (8 * i)))
          zero (Array.mapi (fun i byte -> (i, byte)) block)
    in
    (* Conversion de l'Int64 en float *)
    Int64.float_of_bits i


(* Fonction pour extraire une chaîne de caractères d'un bloc donné *)
let get_string lua_undump size : string =
  if size <= 0 then
    ""  (* Retourne une chaîne vide si la taille est 0 ou négative *)
  else
    let block = load_block lua_undump size in
    let str = Bytes.to_string block in
    (* On supprime le dernier caractère (null terminator) si la chaîne n'est pas vide *)
    if String.length str > 0 then
      String.sub str 0 (String.length str - 1)
    else
      ""


  let get_size_t (state: lua_undump) : int =
    Printf.printf "Reading size_t: %d\n" state.size_t;
    (* Vérification que size_t est valide *)
    if state.size_t <= 0 || state.size_t > 8 then
      failwith "Invalid size_t value"
    else if state.index + state.size_t > Bytes.length state.bytecode then
      failwith "Malformed bytecode!"
    else
      (* Extraction des octets sous forme d'entiers *)
      let block = Array.init state.size_t (fun i ->
        let byte = Char.code (Bytes.get state.bytecode (state.index + i)) in
        Printf.printf "Byte %d: %d\n" i byte;
        byte
      ) in
  
      (* Mise à jour de l'index *)
      state.index <- state.index + state.size_t;
  
      (* Conversion du bloc en entier non signé selon l'endianness *)
      if state.big_endian then
        (* Big-endian: l'octet le plus significatif en premier *)
        Array.fold_left (fun acc byte -> (acc lsl 8) lor byte) 0 block
      else
        (* Little-endian: l'octet le moins significatif en premier *)
        Array.fold_left (fun acc (i, byte) -> acc lor (byte lsl (8 * i))) 0
          (Array.mapi (fun i byte -> (i, byte)) block)


(* cette fonction décode un bloc de bytecode Lua en extrayant toutes ses parties (métadonnées, instructions, constantes, sous-protos, informations de débogage, variables locales et upvalues)
   et retourne une structure complète représentant le chunk Lua décodé   *)
   let rec decode_chunk (state: lua_undump) : chunk =
    let chunk = create_chunk () in
  
    (* Métadonnées du chunk *)
    let name_size = get_size_t state in
    chunk.name_chunk <- get_string state name_size;  (* _get_string *)
    chunk.frst_line  <- get_uint state;             (* _get_uint *)
    chunk.last_line  <- get_uint state;             (* _get_uint *)
    chunk.numUpvals  <- get_byte state;             (* _get_byte *)
    chunk.numParams  <- get_byte state;             (* _get_byte *)
    chunk.isVarg     <- (get_byte state <> 0);      (* _get_byte *)
    chunk.maxStack   <- get_byte state;             (* _get_byte *)
  
    (* Décodage des instructions *)
    let num_instr = get_uint state in               (* _get_uint *)
    for _i = 1 to num_instr do
      let instr_data = get_uint32 state in          (* _get_uint32 *)
      let instr = decode_instr instr_data in
      append_instruction chunk instr
    done;
  
    (* Décodage des constantes *)
    let num_consts = get_uint state in              (* _get_uint *)
    for _i = 1 to num_consts do
      let type_code = get_byte state in             (* _get_byte *)
      let constant =
        match type_code with
        | 0 -> create_constant NIL ""               (* nil *)
        | 1 ->                                      (* bool *)
            let b = get_byte state in
            create_constant BOOL (if b <> 0 then "true" else "false")
        | 3 ->                                      (* number *)
            let d = get_double state in
            create_constant NUMBER (string_of_float d)
        | 4 ->                                      (* string *)
            let str_size = get_size_t state in
            create_constant STRING (get_string state str_size)
        | _ -> failwith (Printf.sprintf "Unknown Datatype! [%d]" type_code)
      in
      append_constant chunk constant
    done;
  
    (* Décodage des sous-protos *)
    let num_protos = get_uint state in              (* _get_uint *)
    for _i = 1 to num_protos do
      let proto = decode_chunk state in
      append_proto chunk proto
    done;
  
    (* Consommation des numéros de ligne (debug, non stockés ici) *)
    let num_lines = get_uint state in               (* _get_uint *)
    for _i = 1 to num_lines do
      let _ = get_uint state in ()
    done;
  
    (* Décodage des variables locales *)
    let num_locals = get_uint state in              (* _get_uint *)
    for _i = 1 to num_locals do
      let name_size = get_size_t state in
      let local_name = get_string state name_size in (* _get_string *)
      let start_pc   = get_uint state in            (* _get_uint *)
      let end_pc     = get_uint state in            (* _get_uint *)
      let loc = create_local local_name start_pc end_pc in
      append_local chunk loc
    done;
  
    (* Décodage des upvalues *)
    let num_upvals = get_uint state in              (* _get_uint *)
    for _i = 1 to num_upvals do
      let name_size = get_size_t state in
      let upval_name = get_string state name_size in (* _get_string *)
      append_upvalue chunk upval_name
    done;
  
    chunk
  
(* cette fonction permet de décoder un bytecode *)
let decode_bytecode (bytecode : bytes) : chunk =
  (* Vérification de la taille minimale du bytecode *)
  if Bytes.length bytecode < 12 then
    failwith "Bytecode too short to be valid"
  else if Bytes.get bytecode 0 <> '\x1b' || Bytes.get bytecode 1 <> 'L'
       || Bytes.get bytecode 2 <> 'u' || Bytes.get bytecode 3 <> 'a' then
    failwith "Invalid Lua bytecode header"
  else
    (* Initialisation de la structure lua_undump *)
    let lua_undump = {
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
      root_chunk = create_chunk ();
    } in

    (* Lire les métadonnées en avançant l'index *)
    lua_undump.vm_version <- get_byte lua_undump;
    lua_undump.bytecode_format <- get_byte lua_undump;
    lua_undump.big_endian <- (get_byte lua_undump = 0);
    lua_undump.int_size <- get_byte lua_undump;
    lua_undump.size_t <- get_byte lua_undump;
    lua_undump.instr_size <- get_byte lua_undump;
    lua_undump.l_number_size <- get_byte lua_undump;
    lua_undump.integral_flag <- get_byte lua_undump;

    (* Vérification des valeurs lues *)
    if lua_undump.size_t <= 0 || lua_undump.size_t > 8 then
      failwith "Invalid size_t value in bytecode header";

    (* Décodage du chunk principal *)
    lua_undump.root_chunk <- decode_chunk lua_undump;

    (* Affichage des informations (optionnel) *)
    Printf.printf "\n---- Decoded Bytecode Info ----\n";
    Printf.printf "VM Version        : %d\n" lua_undump.vm_version;
    Printf.printf "Bytecode Format   : %d\n" lua_undump.bytecode_format;
    Printf.printf "index    : %d\n" lua_undump.index;
    Printf.printf "Big Endian        : %b\n" lua_undump.big_endian;
    Printf.printf "Int Size          : %d bytes\n" lua_undump.int_size;
    Printf.printf "Size_t Size       : %d bytes\n" lua_undump.size_t;
    Printf.printf "Instruction Size  : %d bytes\n" lua_undump.instr_size;
    Printf.printf "Lua Number Size   : %d bytes\n" lua_undump.l_number_size;
    Printf.printf "Integral Flag     : %d (%s)\n"
      lua_undump.integral_flag (if lua_undump.integral_flag = 0 then "Floating point" else "Integer-based");
    Printf.printf "--------------------------------\n\n";

    lua_undump.root_chunk
    
    
(* Fonction pour charger un fichier bytecode Lua *)
let load_file luaCFile =
  (* Ouvrir le fichier en mode binaire *)
  let ic = open_in_bin luaCFile in
  (* Lire tout le contenu du fichier *)
  let bytecode = really_input_string ic (in_channel_length ic) in
  (* Fermer le fichier *)
  close_in ic;

  (* Vérifier que le bytecode est assez long *)
  if String.length bytecode < 9 then
    failwith "Lua bytecode file too short"
  else
    (* Convertir la chaîne en Bytes.t *)
    let bytecode_bytes = Bytes.of_string bytecode in
    (* Décoder le bytecode *)
    decode_bytecode bytecode_bytes
    



(* Fonction pour afficher le désassemblage du bytecode *)
let print_disassembly lua_undump =
  print_chunk lua_undump.root_chunk


(* Fonction pour décoder un bytecode brut *)
let decode_rawbytecode rawbytecode =
  (* Vérification de la taille avant de manipuler la chaîne *)
  if String.length rawbytecode < 4 then
    failwith "Raw bytecode too short"
  else if not (String.sub rawbytecode 0 4 = luamagic) then
    raise LuaBytecodeExpected
  else
    let bytecode = Bytes.init (String.length rawbytecode) (fun i -> rawbytecode.[i]) in
    decode_bytecode bytecode

















type lua_dump = {
  mutable rootChunk : chunk;
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

let create_lua_dump rootChunk = {
  rootChunk;
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

(* Ajoute un bloc de données (tableau d'entiers) à bytecode *)
let write_block (dump : lua_dump) (data : bytes) =
  dump.bytecode <- Bytes.cat dump.bytecode data


(* Convertit un entier en un octet et l'ajoute au bytecode *)
let set_byte dump b =
  write_block dump (Bytes.make 1 (char_of_int b))

(* cette fonction permet de convertir un entier en un bloc de bytes et de l'ajouter au bytecode *)
let set_uint32 (dump: lua_dump) (i: int) =
  let bytes = Bytes.make 4 '\000' in  (* Initialise avec des zéros *)
  if dump.big_endian then (
    Bytes.set bytes 0 (Char.chr ((i lsr 24) land 0xFF));
    Bytes.set bytes 1 (Char.chr ((i lsr 16) land 0xFF));
    Bytes.set bytes 2 (Char.chr ((i lsr 8) land 0xFF));
    Bytes.set bytes 3 (Char.chr (i land 0xFF))
  ) else (
    Bytes.set bytes 0 (Char.chr (i land 0xFF));
    Bytes.set bytes 1 (Char.chr ((i lsr 8) land 0xFF));
    Bytes.set bytes 2 (Char.chr ((i lsr 16) land 0xFF));
    Bytes.set bytes 3 (Char.chr ((i lsr 24) land 0xFF))
  );
  write_block dump bytes


(* cette fonction permet de convertir un entier en un bloc de bytes et de l'ajouter au bytecode *)
let set_uint (dump: lua_dump) (i: int) =
  let bytes = Bytes.create dump.int_size in (* Crée un Bytes.t de taille int_size *)
  if dump.big_endian then
    for n = 0 to dump.int_size - 1 do
      Bytes.set bytes n (Char.chr ((i lsr (8 * (dump.int_size - 1 - n))) land 0xFF))
    done
  else
    for n = 0 to dump.int_size - 1 do
      Bytes.set bytes n (Char.chr ((i lsr (8 * n)) land 0xFF))
    done;
  write_block dump bytes


(* Cette fonction permet de convertir un entier en un bloc de bytes et de l'ajouter au bytecode *)
let set_size_t (dump: lua_dump) (i: int) =
  let bytes = Bytes.create dump.size_t in (* Crée un Bytes.t de taille size_t *)
  if dump.big_endian then
    for n = 0 to dump.size_t - 1 do
      Bytes.set bytes n (Char.chr ((i lsr (8 * (dump.size_t - 1 - n))) land 0xFF))
    done
  else
    for n = 0 to dump.size_t - 1 do
      Bytes.set bytes n (Char.chr ((i lsr (8 * n)) land 0xFF))
    done;
  write_block dump bytes
  

(* Cette fonction permet de convertir un double en un bloc de bytes et de l'ajouter au bytecode *)
let set_double (dump: lua_dump) (f: float) =
  let int_repr = Int64.bits_of_float f in
  let bytes = Bytes.create 8 in (* Crée un Bytes.t de taille 8 *)
  if dump.big_endian then
    for n = 0 to 7 do
      Bytes.set bytes n (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right int_repr (8 * (7 - n))) 0xFFL)))
    done
  else
    for n = 0 to 7 do
      Bytes.set bytes n (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right int_repr (8 * n)) 0xFFL)))
    done;
  write_block dump bytes

(* Cette fonction permet d'écrire une chaîne de caractères dans le bytecode *)
let set_string (dump: lua_dump) (s: string) =
  (* Écrire la taille de la chaîne + 1 pour le null terminator *)
  set_size_t dump (String.length s + 1);
  (* Convertir la chaîne en bytes (UTF-8) *)
  let bytes = Bytes.of_string s in
  write_block dump bytes;
  (* Écrire le null terminator *)
  set_byte dump 0x00
  

let rec decode_chunk_dump (state: lua_dump) (chunk1: chunk) =
  set_string state chunk1.name_chunk;
  set_uint state chunk1.frst_line;
  set_uint state chunk1.last_line;
  set_byte state chunk1.numUpvals;
  set_byte state chunk1.numParams;
  set_byte state (if chunk1.isVarg then 1 else 0);
  set_byte state chunk1.maxStack;

  (* Encodage des instructions *)
  set_uint state (List.length chunk1.instructions);
  List.iter (fun instr -> set_uint32 state (match encode_instr instr with Some v -> v | None -> 0)) chunk1.instructions;

  (* Encodage du nombre de constantes *)
  set_uint state (List.length chunk1.constants);
  List.iter (fun constant ->
    match constant.type_const with
    | NIL -> set_byte state 0
    | BOOL -> set_byte state 1; set_byte state (if bool_of_string constant.data then 1 else 0)
    | NUMBER -> set_byte state 3; set_double state (float_of_string constant.data)
    | STRING -> set_byte state 4; set_string state constant.data
  ) chunk1.constants;

  (* Encodage des sous-protos *)
  set_uint state (List.length chunk1.protos);
  List.iter (fun p -> decode_chunk_dump state p) chunk1.protos;

  (* Encodage des numéros de ligne *)
  set_uint state (List.length chunk1.lineNums);
  List.iter (fun l -> set_uint state l) chunk1.lineNums;

  (* Encodage des variables locales *)
  set_uint state (List.length chunk1.locals);
  List.iter (fun l ->
    set_string state l.name_local;
    set_uint state l.start;
    set_uint state l.end_pc
  ) chunk1.locals;

  (* Encodage des upvalues *)
  set_uint state (List.length chunk1.upvalues);
  List.iter (fun upval -> set_string state upval) chunk1.upvalues


  let dump_Header (state: lua_dump) =
    write_block state (Bytes.of_string luamagic);  
    set_byte state state.vm_version;
    set_byte state state.bytecode_format;
    set_byte state (if state.big_endian then 0 else 1);
    set_byte state state.int_size;
    set_byte state state.size_t;
    set_byte state state.instr_size;
    set_byte state state.l_number_size;
    set_byte state (if state.integral_flag then 1 else 0)
  

let dump (state: lua_dump) =
  dump_Header state;
  decode_chunk_dump state state.rootChunk;
  state.bytecode






(*
let () =
  let chunk = load_file "samples/test1.luac" in
  print_chunk chunk
*)

let save_to_file filename (bytecode: Bytes.t) =
  let oc = open_out_bin filename in
  Bytes.iter (fun c -> output_byte oc (Char.code c)) bytecode;
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
    let lua_dump = create_lua_dump chunk in

    (* Génération du bytecode *)
    let bytecode = dump lua_dump in

    (* Sauvegarde du bytecode dans un fichier *)
    save_to_file "output.luac" bytecode
    