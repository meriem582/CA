open Lexing
open Parser
open Lexer
open Ast
open Transformateur
open Sys
open Unix

let parse_string_to_string s =
  let lexbuf = from_string s in
  try
    let result = prog token lexbuf in
    string_of_expr result ^ "\n"
  with
  | Failure msg -> "Erreur: " ^ msg ^ "\n"
  | Parser.Error -> "Erreur de syntaxe !\n"



let compile_string_to_string s =
  let lexbuf = from_string s in
  try
    let ast = prog token lexbuf in
    let compiled_code = compile [] ast in
    String.concat "; " (List.map string_of_com compiled_code) ^ "\n"
  with
  | Failure msg -> "Erreur: " ^ msg ^ "\n"
  | Parser.Error -> "Erreur de syntaxe !\n"

let ensure_directory_exists dir =
  if not (Sys.file_exists dir && Sys.is_directory dir) then Unix.mkdir dir 0o755

let process_file filename =
  let content = In_channel.with_open_bin filename In_channel.input_all in
  let basename = Filename.basename filename in
  let without_ext = Filename.remove_extension basename in

  let parsed_output = parse_string_to_string content in
  let compiled_output = compile_string_to_string content in

  ensure_directory_exists "resultat_parsing";
  ensure_directory_exists "resultat_CAM";

  let out_parse = "resultat_parsing/" ^ without_ext ^ ".txt" in
  let out_cam = "resultat_CAM/" ^ without_ext ^ ".txt" in

  let oc_parse = open_out out_parse in
  output_string oc_parse parsed_output;
  close_out oc_parse;

  let oc_cam = open_out out_cam in
  output_string oc_cam compiled_output;
  close_out oc_cam


let () =
  let input_dir = "tests" in
  let files = Sys.readdir input_dir |> Array.to_list in
  let mini_ml_files = List.filter (fun f -> Filename.check_suffix f ".mml") files in
  List.iter (fun f -> process_file (Filename.concat input_dir f)) mini_ml_files
