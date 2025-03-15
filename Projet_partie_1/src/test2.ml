exception LuaBytecodeExpected
open Bytes
open Printf
open Int64

(* Définition du type énuméré InstructionType *)
type instruction_type =
  | ABC
  | ABx
  | AsBx

(* Définition du type énuméré opcode *)
type opcode =
  | MOVE | LOADK | LOADBOOL | LOADNIL | GETUPVAL | GETGLOBAL | GETTABLE
  | SETGLOBAL | SETUPVAL | SETTABLE | NEWTABLE | SELF | ADD | SUB | MUL
  | DIV | MOD | POW | UNM | NOT | LEN | CONCAT | JMP | EQ | LT | LE
  | TEST | TESTSET | CALL | TAILCALL | RETURN | FORLOOP | FORPREP
  | TFORLOOP | SETLIST | CLOSE | CLOSURE | VARARG

(* Définition du type énuméré ConstType *)
type const_type =
  | NIL
  | BOOL
  | NUMBER
  | STRING

(* Définition du type instruction *)
type instruction = {
  instr_type : instruction_type;  (* Type d'instruction : ABC, ABx, AsBx *)
  name_instr : string;                  (* Nom de l'instruction *)
  mutable opcode : int option;    (* Opcode associé, mutable car modifiable *)
  mutable a : int option;         (* Champ A (optionnel) *)
  mutable b : int option;         (* Champ B (optionnel) *)
  mutable c : int option;         (* Champ C (optionnel) *)
}

(* Définition du type constant *)
type constant ={
  type_const : const_type;
  data : string;
}

(* Définition du type local *)
type local = {
  name_local : string;
  start : int;
  end_pc : int;
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

type endian = BigEndian | LittleEndian

let _LUAMAGIC = "\x1bLua"

let set_bits num data p s =
  (num land (lnot ((lnot ((lnot 0) lsl s)) lsl p))) lor
  ((data lsl p) land ((lnot ((lnot 0) lsl s)) lsl p))

let _encode_instr instr =
  let data = 0 in
  match instr.opcode, instr.a, instr.b, instr.c with
  | Some opcode, Some a, Some b, Some c ->
      let data = set_bits data opcode 0 6 in
      let data = set_bits data a 6 8 in
      let data =
        match instr.instr_type with
        | ABC  -> let data = set_bits data b 23 9 in
                  set_bits data c 14 9
        | ABx  -> set_bits data b 14 18
        | AsBx -> set_bits data (b + 131071) 14 18
      in
      Some data
  | _ -> None  (* Retourne None si une des valeurs nécessaires est absente *)


(*LuaDump*)

type lua_dump = {
  mutable root_chunk : chunk;
  mutable bytecode : int array;
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
  bytecode = [||];
  vm_version = 0x51;
  bytecode_format = 0x00;
  big_endian = false;
  int_size = 4;
  size_t = 8;
  instr_size = 4;
  l_number_size = 8;
  integral_flag = false;
}

let _writeBlock lua_dump data =
  let new_bytecode = Array.append lua_dump.bytecode data in
  lua_dump.bytecode <- new_bytecode

let _set_byte lua_dump b =
  let new_bytecode = Array.append lua_dump.bytecode [|b|] in
  lua_dump.bytecode <- new_bytecode

let _set_uint32 lua_dump i =
  let order = if lua_dump.big_endian then BigEndian else LittleEndian in
  let bytes = Bytes.create 4 in
  match order with
  | BigEndian ->
      Bytes.set bytes 0 (Char.chr ((i lsr 24) land 0xFF));
      Bytes.set bytes 1 (Char.chr ((i lsr 16) land 0xFF));
      Bytes.set bytes 2 (Char.chr ((i lsr 8) land 0xFF));
      Bytes.set bytes 3 (Char.chr (i land 0xFF))
  | LittleEndian ->
      Bytes.set bytes 3 (Char.chr ((i lsr 24) land 0xFF));
      Bytes.set bytes 2 (Char.chr ((i lsr 16) land 0xFF));
      Bytes.set bytes 1 (Char.chr ((i lsr 8) land 0xFF));
      Bytes.set bytes 0 (Char.chr (i land 0xFF));
  _writeBlock lua_dump (Array.init 4 (fun i -> Char.code (Bytes.get bytes i)))

let _set_uint lua_dump i =
  let order = if lua_dump.big_endian then BigEndian else LittleEndian in
  let bytes = Bytes.create lua_dump.int_size in
  for j = 0 to lua_dump.int_size - 1 do
    let shift = if order = BigEndian then (lua_dump.int_size - 1 - j) * 8 else j * 8 in
    Bytes.set bytes j (Char.chr ((i lsr shift) land 0xFF))
  done;
  _writeBlock lua_dump (Array.init lua_dump.int_size (fun k -> Char.code (Bytes.get bytes k)))


let _set_size_t lua_dump i =
  let order = if lua_dump.big_endian then BigEndian else LittleEndian in
  let bytes = Bytes.create lua_dump.size_t in
  for j = 0 to lua_dump.size_t - 1 do
    let shift = if order = BigEndian then (lua_dump.size_t - 1 - j) * 8 else j * 8 in
    Bytes.set bytes j (Char.chr ((i lsr shift) land 0xFF))
  done;
  _writeBlock lua_dump (Array.init lua_dump.size_t (fun k -> Char.code (Bytes.get bytes k)))


let _set_double lua_dump f =
  let order = if lua_dump.big_endian then BigEndian else LittleEndian in
  let bytes = Bytes.create 8 in
  let i64 = Int64.bits_of_float f in
  for j = 0 to 7 do
    let shift = if order = BigEndian then (7 - j) * 8 else j * 8 in
    Bytes.set bytes j (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right i64 shift) 0xFFL)))
  done;
  _writeBlock lua_dump (Array.init 8 (fun k -> Char.code (Bytes.get bytes k)))

let _set_string lua_dump str =
  _set_size_t lua_dump (String.length str + 1);
  _writeBlock lua_dump (Array.init (String.length str) (fun i -> Char.code str.[i]));
  _set_byte lua_dump 0x00

let _dump_header lua_dump =
  _writeBlock lua_dump (Array.init 4 (fun i -> Char.code _LUAMAGIC.[i]));
  _set_byte lua_dump lua_dump.vm_version;
  _set_byte lua_dump lua_dump.bytecode_format;
  _set_byte lua_dump (if lua_dump.big_endian then 0 else 1);
  _set_byte lua_dump lua_dump.int_size;
  _set_byte lua_dump lua_dump.size_t;
  _set_byte lua_dump lua_dump.instr_size;
  _set_byte lua_dump lua_dump.l_number_size;
  _set_byte lua_dump (if lua_dump.integral_flag then 1 else 0)


let rec _dump_chunk lua_dump chunk =
  _set_string lua_dump chunk.name_chunk;
  _set_uint lua_dump chunk.frst_line;
  _set_uint lua_dump chunk.last_line;
  _set_byte lua_dump chunk.numUpvals;
  _set_byte lua_dump chunk.numParams;
  _set_byte lua_dump (if chunk.isVarg then 1 else 0);
  _set_byte lua_dump chunk.maxStack;

  _set_uint lua_dump (List.length chunk.instructions);
  List.iter (fun instr -> _set_uint32 lua_dump (match _encode_instr instr with Some v -> v | None -> 0)) chunk.instructions;

  _set_uint lua_dump (List.length chunk.constants);
  List.iter (fun constant ->
    match constant.type_const with
    | NIL -> _set_byte lua_dump 0
    | BOOL -> _set_byte lua_dump 1; _set_byte lua_dump (if bool_of_string constant.data then 1 else 0)
    | NUMBER -> _set_byte lua_dump 3; _set_double lua_dump (float_of_string constant.data)
    | STRING -> _set_byte lua_dump 4; _set_string lua_dump constant.data
  ) chunk.constants;

  _set_uint lua_dump (List.length chunk.protos);
  List.iter (fun proto -> _dump_chunk lua_dump proto) chunk.protos;

  _set_uint lua_dump (List.length chunk.lineNums);
  List.iter (fun line -> _set_uint lua_dump line) chunk.lineNums;

  _set_uint lua_dump (List.length chunk.locals);
  List.iter (fun local ->
    _set_string lua_dump local.name_local;
    _set_uint lua_dump local.start;
    _set_uint lua_dump local.end_pc
  ) chunk.locals;

  _set_uint lua_dump (List.length chunk.upvalues);
  List.iter (fun upval -> _set_string lua_dump upval) chunk.upvalues


let dump lua_dump =
  _dump_header lua_dump;
  _dump_chunk lua_dump lua_dump.root_chunk;
  lua_dump.bytecode

let save_to_file filename bytecode =
  let oc = open_out_bin filename in
  Array.iter (fun byte -> output_byte oc byte) bytecode;
  close_out oc;
  Printf.printf "Bytecode écrit dans : %s\n" filename

let () =
(* Création du chunk de test *)
let chunk = {
  constants = [
    { type_const = NUMBER; data = "9" };
    { type_const = NUMBER; data = "33" };
    { type_const = STRING; data = "print" };
  ];
  instructions = [
    { instr_type = ABC; name_instr = "LOADK"; opcode = Some 1; a = Some 0; b = Some 0; c = None };
    { instr_type = ABC; name_instr = "LOADK"; opcode = Some 1; a = Some 1; b = Some 1; c = None };
    { instr_type = ABC; name_instr = "CALL"; opcode = Some 2; a = Some 2; b = Some 1; c = Some 1 };
  ];
  protos = [];
  name_chunk = "test";
  frst_line = 1;
  last_line = 1;
  numUpvals = 0;
  numParams = 0;
  isVarg = false;
  maxStack = 2;
  upvalues = [];
  lineNums = [];
  locals = [];
} in

(* Création de l'objet lua_dump *)
let lua_dump = create_dump chunk in

(* Génération du bytecode *)
let bytecode = dump lua_dump in

(* Sauvegarde du bytecode dans un fichier *)
save_to_file "output.luac" bytecode