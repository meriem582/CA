open Printf
open Bytes

exception MalformedBytecode of string

class luaUndump = object (self)
  val mutable bytecode = Bytes.empty
  val mutable index = 0
  val mutable big_endian = false
  val mutable int_size = 0
  val mutable size_t = 0
  val mutable l_number_size = 0

  (* Fonction utilitaire pour inverser une chaîne *)
  method private reverse_string s =
    let len = String.length s in
    String.init len (fun i -> s.[len - i - 1])

  method private load_block sz =
    if index + sz > Bytes.length bytecode then
      raise (MalformedBytecode "Bytecode mal formé !");
    let temp = Bytes.sub bytecode index sz in
    index <- index + sz;
    temp

  method private get_byte () =
    Char.code (Bytes.get (self#load_block 1) 0)

  method private bytes_to_int32 bytes =
    let result = ref 0l in
    for i = 0 to 3 do
      result := Int32.logor !result (Int32.shift_left (Int32.of_int (Char.code (Bytes.get bytes i))) (8 * i))
    done;
    !result

  method private bytes_to_int64 bytes =
    let result = ref 0L in
    for i = 0 to 7 do
      result := Int64.logor !result (Int64.shift_left (Int64.of_int (Char.code (Bytes.get bytes i))) (8 * i))
    done;
    !result

  method private get_uint32 () =
    let bytes = self#load_block 4 in
    let bytes = if big_endian then bytes else Bytes.of_string (self#reverse_string (Bytes.to_string bytes)) in
    Int32.to_int (self#bytes_to_int32 bytes)

  method private get_uint () =
    let bytes = self#load_block int_size in
    let bytes = if big_endian then bytes else Bytes.of_string (self#reverse_string (Bytes.to_string bytes)) in
    Int64.to_int (self#bytes_to_int64 bytes)

  method private get_size_t () =
    let bytes = self#load_block size_t in
    let bytes = if big_endian then bytes else Bytes.of_string (self#reverse_string (Bytes.to_string bytes)) in
    Int64.to_int (self#bytes_to_int64 bytes)

  method private get_string () =
    let size = self#get_size_t () in
    if size = 0 then ""
    else
      let raw_string = Bytes.to_string (self#load_block size) in
      String.sub raw_string 0 (size - 1)

  method decode_bytecode (bc : Bytes.t) =
    bytecode <- bc;
    index <- 4; (* Skip magic number *)

    let vm_version = self#get_byte () in
    let bytecode_format = self#get_byte () in
    big_endian <- (self#get_byte () = 0);
    int_size <- self#get_byte ();
    size_t <- self#get_byte ();
    let instr_size = self#get_byte () in
    l_number_size <- self#get_byte ();
    let integral_flag = self#get_byte () in

    printf "VM version: %d\n" vm_version;
    printf "Bytecode format: %d\n" bytecode_format;
    printf "Big endian: %b\n" big_endian;
    printf "Int size: %d\n" int_size;
    printf "Size_t: %d\n" size_t;
    printf "Instruction size: %d\n" instr_size;
    printf "Lua Number size: %d\n" l_number_size;
    printf "Integral flag: %d\n" integral_flag;

  method load_file (filename : string) =
    try
      let ic = open_in_bin filename in
      let len = in_channel_length ic in
      let content = really_input_string ic len in
      close_in ic;

      if String.sub content 0 4 <> "\x1bLua" then
        raise (MalformedBytecode "Bytecode Lua attendu !");

      printf "Fichier chargé avec succès : %s\n" filename;
      self#decode_bytecode (Bytes.of_string content)
    with
    | Sys_error msg -> printf "Erreur lors de l'ouverture du fichier : %s\n" msg
    | MalformedBytecode msg -> printf "Erreur de bytecode : %s\n" msg
    | _ -> printf "Une erreur inconnue s'est produite.\n"

end

let () =
  try
    print_endline "Démarrage du programme";
    let dumper = new luaUndump in

    if Array.length Sys.argv < 2 then
      print_endline "Utilisation : ./test2 <fichier.luac>"
    else
      dumper#load_file Sys.argv.(1);

    print_endline "Fin normale du programme"
  with
  | e -> print_endline ("Erreur : " ^ Printexc.to_string e)
