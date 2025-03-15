












  (* Programme de test étendu pour valider les fonctions de la VM Lua *)
  let test_print_chunk () =
    (* Création d'un chunk vide *)
    let chunk_example = create_chunk () in
    
    (* Ajout de constantes (exemples) *)
    let constant1 = create_constant STRING "hello" in
    let constant2 = create_constant BOOL "true" in
    let constant3 = create_constant NUMBER "42" in
    let constant4 = create_constant NUMBER "3.14" in
    chunk_example.constants <- [constant1; constant2; constant3; constant4];
   
    (* Ajout de variables locales (exemples) *)
    let local1 = create_local "localVar1" 0 10 in
    let local2 = create_local "localVar2" 11 20 in
    chunk_example.locals <- [local1; local2];
    
    (* Création d'instructions Lua (format typique) *)
   
    (* 1. MOVE instruction (déplacer une valeur) *)
    let instruction1 = create_instruction ABC "MOVE" in
    instruction1.opcode <- Some 0;  (* MOVE *)
    instruction1.a <- Some 0;  (* Registre 0 *)
    instruction1.b <- Some 1;  (* Registre 1 *)
    
    (* 2. LOADK instruction (charger une constante) *)
    let instruction2 = create_instruction ABx "LOADK" in
    instruction2.opcode <- Some 1;  (* LOADK *)
    instruction2.a <- Some 1;  (* Registre 1 *)
    instruction2.b <- Some 0;  (* Constante 0 (index de la constante) *)
     
    (* 3. ADD instruction (additionner deux registres) *)
    let instruction3 = create_instruction ABC "ADD" in
    instruction3.opcode <- Some 2;  (* ADD *)
    instruction3.a <- Some 2;  (* Registre 2 *)
    instruction3.b <- Some 0;  (* Registre 0 *)
    instruction3.c <- Some 1;  (* Registre 1 *)
     
    (* 4. SUB instruction (soustraire deux registres) *)
    let instruction4 = create_instruction ABC "SUB" in
    instruction4.opcode <- Some 3;  (* SUB *)
    instruction4.a <- Some 3;  (* Registre 3 *)
    instruction4.b <- Some 2;  (* Registre 2 *)
    instruction4.c <- Some 0;  (* Registre 0 *)
     
    (* 5. LOADK instruction (charger une autre constante) *)
    let instruction5 = create_instruction ABx "LOADK" in
    instruction5.opcode <- Some 4;  (* LOADK *)
    instruction5.a <- Some 4;  (* Registre 4 *)
    instruction5.b <- Some 2;  (* Constante 2 (index de la constante) *)
      
    (* Ajout des instructions au chunk *)
    chunk_example.instructions <- [instruction1; instruction2; instruction3; instruction4; instruction5];
     
    (* Appel de print_chunk pour afficher le contenu du chunk *)
    print_chunk chunk_example
  
  let test_create_instruction () =
    let instr = create_instruction ABC "MOVE" in
    instr.opcode <- Some 0;  (* MOVE *)
    instr.a <- Some 1;
    instr.b <- Some 2;
    instr.c <- Some 3;
  
    assert (instr.opcode = Some 0);
    assert (instr.a = Some 1);
    assert (instr.b = Some 2);
    assert (instr.c = Some 3);
    Printf.printf "Test create_instruction passed\n"
  
  let test_create_constant () =
    let const1 = create_constant NUMBER "42" in
    let const2 = create_constant STRING "Hello" in
  
    assert (const1.type_const = NUMBER);
    assert (const1.data = "42");
    assert (const2.type_const = STRING);
    assert (const2.data = "Hello");
    Printf.printf "Test create_constant passed\n"
  
  let test_append_instruction () =
    let chunk = create_chunk () in
    let instr = create_instruction ABC "MOVE" in
    instr.opcode <- Some 0;
    instr.a <- Some 1;
    append_instruction chunk instr;
  
    assert (List.length chunk.instructions = 1);
    assert (chunk.instructions = [instr]);
    Printf.printf "Test append_instruction passed\n"
  
  let test_append_constant () =
    let chunk = create_chunk () in
    let const = create_constant NUMBER "42" in
    append_constant chunk const;
  
    assert (List.length chunk.constants = 1);
    assert (chunk.constants = [const]);
    Printf.printf "Test append_constant passed\n"
  
  let test_get_annotation () =
    let chunk = create_chunk () in
  
    (* Test MOVE annotation *)
    let instr_move = create_instruction ABC "MOVE" in
    instr_move.opcode <- Some 0;
    instr_move.a <- Some 1;
    instr_move.b <- Some 0; (* R[0] existe *)
    append_constant chunk (create_constant NUMBER "10");
    append_instruction chunk instr_move;
    assert (getAnnotation instr_move chunk = "move R[0] into R[1]");
  
    (* Test LOAD annotation *)
    let instr_load = create_instruction ABC "LOAD" in
    instr_load.opcode <- Some 1;
    instr_load.a <- Some 0;
  
    (* On ajoute une constante pour l'index 0 *)
    append_constant chunk (create_constant NUMBER "99");
  
    instr_load.b <- Some 0;  (* L'index doit correspondre à la position dans chunk.constants *)
  
    append_instruction chunk instr_load;
    assert (getAnnotation instr_load chunk = "load 99 into R[0]");
  
    (* Test ADD annotation *)
    let instr_add = create_instruction ABC "ADD" in
    instr_add.opcode <- Some 12;
    instr_add.a <- Some 3;
    instr_add.b <- Some 1;
    instr_add.c <- Some 2;
    append_instruction chunk instr_add;
    assert (getAnnotation instr_add chunk = "add R[2] to R[1], place into R[3]");
  
    (* Test SUB annotation *)
    let instr_sub = create_instruction ABC "SUB" in
    instr_sub.opcode <- Some 13;
    instr_sub.a <- Some 4;
    instr_sub.b <- Some 2;
    instr_sub.c <- Some 1;
    append_instruction chunk instr_sub;
    assert (getAnnotation instr_sub chunk = "sub R[1] from R[2], place into R[4]");
  
    (* Test MUL annotation *)
    let instr_mul = create_instruction ABC "MUL" in
    instr_mul.opcode <- Some 14;
    instr_mul.a <- Some 5;
    instr_mul.b <- Some 3;
    instr_mul.c <- Some 2;
    append_instruction chunk instr_mul;
    assert (getAnnotation instr_mul chunk = "mul R[2] to R[3], place into R[5]");
  
    (* Test DIV annotation *)
    let instr_div = create_instruction ABC "DIV" in
    instr_div.opcode <- Some 15;
    instr_div.a <- Some 6;
    instr_div.b <- Some 4;
    instr_div.c <- Some 3;
    append_instruction chunk instr_div;
    assert (getAnnotation instr_div chunk = "div R[3] from R[4], place into R[6]");
    (* Test instruction inconnue *)
    let instr_unknown = create_instruction ABC "UNKNOWN" in
    instr_unknown.opcode <- Some 99;
    append_instruction chunk instr_unknown;
    assert (getAnnotation instr_unknown chunk = "");
  
    Printf.printf "Test get_annotation passed\n"
  
  let test_create_local () =
    let chunk = create_chunk () in
    let local = create_local "var1" 0 10 in
    append_local chunk local;
  
    assert (List.length chunk.locals = 1);
    assert (chunk.locals = [local]);
    Printf.printf "Test create_local passed\n"
  
  let test_chunk_integrity () =
    let chunk = create_chunk () in
  
    (* Ajout de plusieurs instructions *)
    for i = 0 to 15 do
      let instr = create_instruction ABC "TEST" in
      instr.opcode <- Some i;
      append_instruction chunk instr
    done;
  
    assert (List.length chunk.instructions = 16);
  
    (* Ajout de plusieurs constantes *)
    for i = 0 to 4 do
      append_constant chunk (create_constant NUMBER (string_of_int i))
    done;
  
    assert (List.length chunk.constants = 5);
  
    (* Ajout de plusieurs variables locales *)
    for i = 0 to 2 do
      append_local chunk (create_local ("var" ^ string_of_int i) i (i + 10))
    done;
  
    assert (List.length chunk.locals = 3);
  
    Printf.printf "Test chunk_integrity passed\n"
  
  let test_decode_rawbytecode() =
    let rawbytecode = "\x1bLua\x51\x01\x00\x04\x04\x04\x08\x00" in
    try
      let result = decode_rawbytecode rawbytecode in
      Printf.printf "Final Decoded Chunk: %s\n" result
    with
    | LuaBytecodeExpected -> Printf.printf "Exception: Lua Bytecode expected!\n"
    | Failure msg -> Printf.printf "Error: %s\n" msg
  
  let test_get_size_t () =
     (* Tableau d'octets de 2 octets pour size_t de 2 octets : 0x12, 0x34 *)
    let bytecode = [| 0x12; 0x34 |] in
    let state = {
      bytecode;
      index = 0;
      vm_version = 0;
      bytecode_format = 0;
      big_endian = true;
      int_size = 0;
      size_t = 2;
      instr_size = 0;
      l_number_size = 0;
      integral_flag = 0;
      root_chunk = "";
    } in
    
    (* Appel à _get_size_t pour lire les 2 octets *)
    let result = _get_size_t state in
    Printf.printf "Big-endian Result: %d\n" result;
    
    (* On change l'ordre à Little Endian pour voir la différence *)
    let state = {
      bytecode;
      index = 0;
      vm_version = 0;
      bytecode_format = 0;
      big_endian = false;
      int_size = 0;
      size_t = 2;
      instr_size = 0;
      l_number_size = 0;
      integral_flag = 0;
      root_chunk = "";
    } in
    let result_little = _get_size_t state in
    Printf.printf "Little-endian Result: %d\n" result_little
  
  let test_get_double_e () =
    (* Encodage de 2.71828 en IEEE 754 big-endian : [0x40; 0x05; 0xBF; 0x0A; 0x8B; 0x14; 0x57; 0x69] *)
    let instance = {
      bytecode = [|0x40; 0x05; 0xBF; 0x0A; 0x8B; 0x14; 0x57; 0x69|]; (* 2.71828 *)
      index = 0;
      vm_version = 0;
      bytecode_format = 0;
      big_endian = true; (* Little-endian = false *)
      int_size = 4;
      size_t = 4;
      instr_size = 4;
      l_number_size = 8;
      integral_flag = 0;
      root_chunk = "";
    } in
    let result = _get_double instance in
    Printf.printf "Result (expected 2.71828): %f\n" result
  
  let test_get_string () =
    let instance = {
      bytecode = [|104; 101; 108; 108; 111; 0|]; (* "hello\0" *)
      index = 0;
      vm_version = 0;
      bytecode_format = 0;
      big_endian = true;
      int_size = 4;
      size_t = 4;
      instr_size = 4;
      l_number_size = 8;
      integral_flag = 0;
      root_chunk = "";
    } in
    let result = _get_string instance 6 in
    Printf.printf "Result (expected 'hello'): %s\n" result
  
  let print_binary n =
    let rec aux n acc =
      if n = 0 then acc
      else aux (n lsr 1) ((string_of_int (n land 1)) ^ acc)
    in
    print_endline (aux n "");;
  
  let test_encode_instr () =
    print_endline "Instruction 1 (ABC - ADD) :";
    
    (* Création d'instructions de test *)
    let instr1 = create_instruction ABC "ADD" in
    instr1.opcode <- Some 0;
    instr1.a <- Some 1;
    instr1.b <- Some 2;
    instr1.c <- Some 3;
  
    let instr2 = create_instruction ABx "LOADK" in
    instr2.opcode <- Some 1;
    instr2.a <- Some 4;
    instr2.b <- Some 255;
  
    let instr3 = create_instruction AsBx "JMP" in
    instr3.opcode <- Some 2;
    instr3.a <- Some 0;
    instr3.b <- Some (-50);
  
    (* Encodage et assertions *)
    let encoded1 = _encode_instr instr1 in
    let encoded2 = _encode_instr instr2 in
    let encoded3 = _encode_instr instr3 in
  
    print_binary encoded1;
    assert (encoded1 = 0b1000000001100000001000000);
  
    print_endline "Instruction 2 (ABx - LOADK) :";
    print_binary encoded2;
    assert (encoded2 = 0b1111111100000100000001);
  
    print_endline "Instruction 3 (AsBx - JMP) :";
    print_binary encoded3;
    assert (encoded3 = 0b1111111111100110100000000000010);
  
    print_endline "Tous les tests de test_encode_instr sont passés !";;
  
  let test_get_uint () =
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
  
    let result = _get_uint lua_undump in
    Printf.printf "Résultat (big-endian) : 0x%08X\n" result;
  
    lua_undump.index <- 0;
    lua_undump.big_endian <- false;
  
    let result_le = get_uint32 lua_undump in
    Printf.printf "Résultat (little-endian) : 0x%08X\n" result_le;()
  let run_tests () =
    test_get_uint32 ();
    test_get_uint ();
    test_decode_chunk ();
    Printf.printf "All tests passed\n"
  
  (* Exécution des tests *)
  
  (* Création d'un chunk de test *)
  
  