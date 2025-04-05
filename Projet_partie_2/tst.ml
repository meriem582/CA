open Lexing
open Parser
open Lexer
open Ast
open Transformateur


let parse_string s =
  let lexbuf = from_string s in
  try
    let result = prog token lexbuf in
    Printf.printf "Parsing réussi : %s\n" (string_of_expr result)
  with
  | Failure msg -> Printf.printf "Erreur: %s\n" msg
  | Parsing.Parse_error -> Printf.printf "Erreur de syntaxe !\n"

let compile_string s =
  let lexbuf = from_string s in
  try
    let ast = prog token lexbuf in
    (*Printf.printf "Parsing réussi : %s\n" (string_of_expr ast);*)
    let compiled_code = compile [] ast in
    Printf.printf "Code CAM : %s\n" (String.concat "; " (List.map string_of_com compiled_code))
  with
  | Failure msg -> Printf.printf "Erreur: %s\n" msg
  | Parsing.Parse_error -> Printf.printf "Erreur de syntaxe !\n"

(* Fonction pour tester différents cas *)
let () =
  print_endline "Test 1: let x = 42 in x";
  parse_string "let x = 42 in x";
  compile_string "let x = 42 in x";

  print_endline "\nTest 2: if true then 1 else 0";
  parse_string "if true then 1 else 0";
  compile_string "if true then 1 else 0";


  print_endline "\nTest 3: fun x -> x + 1";
  parse_string "fun x -> x + 1";
  compile_string "fun x -> x + 1";


  print_endline "\nTest 4: let rec f = fun x -> x in f 3";
  parse_string "let rec f = fun x -> x in f 3";
  compile_string "let rec f = fun x -> x in f 3";

  print_endline "\nTest 5: 3 + 5";
  parse_string "3 + 5";
  compile_string "3 + 5";

  print_endline "\nTest 6: 10 - 4";
  parse_string "10 - 4";
  compile_string "10 - 4";

  print_endline "\nTest 7: 6 * 7 / 2";
  parse_string "6 * 7 / 2";
  compile_string "6 * 7 / 2";

  print_endline "\nTest 8: fun x -> x * 2";
  parse_string "fun x -> x * 2";
  compile_string "fun x -> x * 2";

  print_endline "\nTest 9: let rec fact = fun n -> if n > 0 then n * fact (n - 1) else 1 in fact 5";
  parse_string "let rec fact = fun n -> if n > 0 then n * fact (n - 1) else 1 in fact 5";
  compile_string "let rec fact = fun n -> if n > 0 then n * fact (n - 1) else 1 in fact 5";

  print_endline "\nTest 10: let p = (1, 2) in p";
  parse_string "let p = (1, 2) in p";
  compile_string "let p = (1, 2) in p";

  print_endline "\nTest 11: let p = (3, 4) in fst p";
  parse_string "let p = (3, 4) in fst p";
  compile_string "let p = (3, 4) in fst p";


  print_endline "\nTest 12: let x = 5 in x + 2";
  parse_string "let x = 5 in x + 2";
  compile_string "let x = 5 in x + 2";

  print_endline "\nTest 13: let a = 10 in let b = 20 in a + b";
  parse_string "let a = 10 in let b = 20 in a + b";
  compile_string "let a = 10 in let b = 20 in a + b";

  print_endline "\nTest 14: if 4 < 5 then true else false";
  parse_string "if 4 < 5 then true else false";
  compile_string "if 4 < 5 then true else false";

  print_endline "\nTest 15: (fun x -> x * x) 3";
  parse_string "(fun x -> x * x) 3";
  compile_string "(fun x -> x * x) 3";

  print_endline "\nTest 16: let rec fib = fun n -> if n <= 1 then n else fib(n-1) + fib(n-2) in fib 5";
  parse_string "let rec fib = fun n -> if n <= 1 then n else fib(n-1) + fib(n-2) in fib 5";
  compile_string "let rec fib = fun n -> if n <= 1 then n else fib(n-1) + fib(n-2) in fib 5";


  print_endline "\nTest 17: let y = 4 * (2 + 3) in y";
  parse_string "let y = 4 * (2 + 3) in y";
  compile_string "let y = 4 * (2 + 3) in y";

  print_endline "\nTest 18: let pair = (10, 20) in snd pair";
  parse_string "let pair = (10, 20) in snd pair";
  compile_string "let pair = (10, 20) in snd pair";

  print_endline "\nTest 19: let z = (fun x -> x + 1) 5 in z * 2";
  parse_string "let z = (fun x -> x + 1) 5 in z * 2";
  compile_string "let z = (fun x -> x + 1) 5 in z * 2";

  print_endline "\nTest 20: (fun x -> x * x) 5";
  parse_string "(fun x -> x * x) 5";
  compile_string "(fun x -> x * x) 5";


  print_endline "\nTest 21: let a = 5 in let b = 10 in a + b";
  parse_string "let a = 5 in let b = 10 in a + b";
  compile_string "let a = 5 in let b = 10 in a + b";


  print_endline "\nTest 22: let swap = fun (x, y) -> (y, x) in swap (3, 4)";
  parse_string "let swap = fun (x, y) -> (y, x) in swap (3, 4)";
  compile_string "let swap = fun (x, y) -> (y, x) in swap (3, 4)";

  print_endline "\nTest 23: let triple = fun x -> 3 * x in triple 7";
  parse_string "let triple = fun x -> 3 * x in triple 7";
  compile_string "let triple = fun x -> 3 * x in triple 7";


  print_endline "\nTest 24: let x = 5 in if x > 3 then x * 2 else x / 2";
  parse_string "let x = 5 in if x > 3 then x * 2 else x / 2";
  compile_string "let x = 5 in if x > 3 then x * 2 else x / 2";

  print_endline "\nTest 25: let id = fun x -> x in id (id 5)";
  parse_string "let id = fun x -> x in id (id 5)";
  compile_string "let id = fun x -> x in id (id 5)";


  print_endline "\nTest 26: let x = 4 in let y = x + 3 in x * y";
  parse_string "let x = 4 in let y = x + 3 in x * y";
  compile_string "let x = 4 in let y = x + 3 in x * y";

  print_endline "\nTest 27: let p = (1, 2) in let (a, b) = p in a + b";
  parse_string "let p = (1, 2) in let (a, b) = p in a + b";
  compile_string "let p = (1, 2) in let (a, b) = p in a + b";


  print_endline "\nTest 28: let x = 10 in x == 10";
  parse_string "let x = 10 in x == 10";
  compile_string "let x = 10 in x == 10";

  print_endline "\nTest 29: let is_positive = fun x -> if x > 0 then true else false in is_positive (-5)";
  parse_string "let is_positive = fun x -> if x > 0 then true else false in is_positive (-5)";
  compile_string "let is_positive = fun x -> if x > 0 then true else false in is_positive (-5)";

  print_endline "\nTest 30: let rec countdown = fun n -> if n == 0 then 0 else countdown (n - 1) in countdown 5";
  parse_string "let rec countdown = fun n -> if n == 0 then 0 else countdown (n - 1) in countdown 5";
  compile_string "let rec countdown = fun n -> if n == 0 then 0 else countdown (n - 1) in countdown 5";

  print_endline "\nTest 31: let x = 3 in (x, x + 1)";
  parse_string "let x = 3 in (x, x + 1)";
  compile_string "let x = 3 in (x, x + 1)";


  print_endline "\nTest 32: let neg = fun x -> -x in neg (-10)";
  parse_string "let neg = fun x -> -x in neg (-10)";
  compile_string "let neg = fun x -> -x in neg (-10)";
