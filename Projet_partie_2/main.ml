open Lexing
open Parser
open Lexer
open Ast
open Transformateur
open Vm
open Traducteur
open Sys
open Unix

let parse_string str =
  let lexbuf = from_string str in
  try Parser.prog Lexer.token lexbuf
  with
  | Parser.Error ->
      let pos = lexbuf.lex_curr_p in
      Printf.eprintf "Erreur de syntaxe à la ligne %d, colonne %d\n"
        pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1);
      exit 1

let ensure_dir_exists dir =
  if not (Sys.file_exists dir && Sys.is_directory dir)
  then Unix.mkdir dir 0o755

let generate_code_ecl flat_code path =
  let oc = open_out path in
  Printf.fprintf oc "let code = create<1024>() ;;\n\n";
  Printf.fprintf oc "let load_code() =\n";

  List.iteri (fun i com ->
    let line = match com with
      | FQuote (Int n) -> Printf.sprintf "  set(code,%d,quote_int %d);" i n
      | FQuote (Bool b) -> Printf.sprintf "  set(code,%d,quote_bool %b);" i b
      | FQuote Nullvalue -> Printf.sprintf "  set(code,%d,quote_null());" i
      | FOpc Add -> Printf.sprintf "  set(code,%d,add);" i
      | FOpc Sub -> Printf.sprintf "  set(code,%d,sub);" i
      | FOpc Mult -> Printf.sprintf "  set(code,%d,mult);" i
      | FOpc Div -> Printf.sprintf "  set(code,%d,div);" i
      | FOpc Eq -> Printf.sprintf "  set(code,%d,eq);" i
      | FOpc Lt -> Printf.sprintf "  set(code,%d,lt);" i
      | FOpc Gt -> Printf.sprintf "  set(code,%d,gt);" i
      | FOpc Leq -> Printf.sprintf "  set(code,%d,leq);" i
      | FOpc Geq -> Printf.sprintf "  set(code,%d,geq);" i
      | FPush -> Printf.sprintf "  set(code,%d,push);" i
      | FSwap -> Printf.sprintf "  set(code,%d,swap);" i
      | FApp -> Printf.sprintf "  set(code,%d,app);" i
      | FCons -> Printf.sprintf "  set(code,%d,cons);" i
      | FRplac -> Printf.sprintf "  set(code,%d,rplac);" i
      | FCar -> Printf.sprintf "  set(code,%d,car);" i
      | FCdr -> Printf.sprintf "  set(code,%d,cdr);" i
      | FCur idx -> Printf.sprintf "  set(code,%d,cur %d);" i idx
      | FBranch (i1, i2) -> Printf.sprintf "  set(code,%d,branch %d %d);" i i1 i2
      | _ -> Printf.sprintf "  set(code,%d,unknown);" i
    in
    Printf.fprintf oc "%s\n" line
  ) flat_code;

  Printf.fprintf oc ";;\n";
  close_out oc

let rec string_of_com = function
  | Quote (Int n) -> Printf.sprintf "Quote %d" n
  | Quote (Bool b) -> Printf.sprintf "Quote %b" b
  | Quote Nullvalue -> "Quote Null"
  | Quote _ -> "Quote <complex>"
  | Opc Add -> "Add"
  | Opc Eq -> "Eq"
  | Opc Sub -> "Sub"
  | Opc Mult -> "Mult"
  | Opc Div -> "Div"
  | Opc Lt -> "Lt"
  | Opc Gt -> "Gt"
  | Opc Leq -> "Leq"
  | Opc Geq -> "Geq"
  | Cdr -> "Cdr"
  | Car -> "Car"
  | Cons -> "Cons"
  | Rplac -> "Rplac"
  | Push -> "Push"
  | Swap -> "Swap"
  | App -> "App"
  | Cur cs -> "Cur [" ^ (String.concat "; " (List.map string_of_com cs)) ^ "]"
  | Branch (c1, c2) ->
      "Branch [" ^ (String.concat "; " (List.map string_of_com c1)) ^ "] [" ^
      (String.concat "; " (List.map string_of_com c2)) ^ "]"

let () =
  let test_dir = "tests" in
  let output_dir = "result_Cam" in

  ensure_dir_exists output_dir;

  Sys.readdir test_dir
  |> Array.to_list
  |> List.filter (fun name -> Filename.check_suffix name ".mml")
  |> List.iter (fun filename ->
    let path = Filename.concat test_dir filename in
    let content = In_channel.with_open_bin path In_channel.input_all in

    Printf.printf "Traitement du fichier : %s\n" filename;
    let ast = parse_string content in
    let cam_code = Transformateur.compile Nullpat ast in
    let flat_code = Traducteur.flatten cam_code in

    (* Écriture du fichier .cam *)
    let cam_str = "[" ^ String.concat "; " (List.map string_of_com cam_code) ^ "]" in
    let cam_filename =
      Filename.concat output_dir (Filename.chop_suffix filename ".mml" ^ ".cam")
    in
    Out_channel.with_open_text cam_filename (fun oc ->
      Printf.fprintf oc "%s\n" cam_str
    );

    (* Écriture du fichier .ecl *)
    let eclat_file =
      Filename.concat output_dir (Filename.chop_suffix filename ".mml" ^ ".ecl")
    in
    generate_code_ecl flat_code eclat_file;

    (* Exécution OCaml pour debug *)
    let (res, _, _) = Vm.exec_code cam_code in
    Printf.printf "Résultat : %s\n\n" (Transformateur.string_of_value !res)
  )
