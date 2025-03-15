type const_type =
  | NIL
  | BOOL
  | NUMBER
  | STRING

type constant ={
  type_const : const_type;
  data : string;
}

type local = {
  name_local : string;
  start : int;
  end_pc : int;
}

type instruction_type =
  | ABC
  | ABx
  | AsBx

type instruction = {
  instr_type : instruction_type;  (* Type d'instruction : ABC, ABx, AsBx *)
  name_instr : string;                  (* Nom de l'instruction *)
  mutable opcode : int option;    (* Opcode associé, mutable car modifiable *)
  mutable a : int option;         (* Champ A (optionnel) *)
  mutable b : int option;         (* Champ B (optionnel) *)
  mutable c : int option;         (* Champ C (optionnel) *)
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

