(* File calc.ml *)
open Ast
open Pp

let print_channel ch =
  let lexbuf = Lexing.from_channel ch in
      let result = Parser.parse Lexer.token lexbuf in
        Printf.printf "%s" (pp_prog result); print_newline(); flush stdout

let _ =
  if (Array.length Sys.argv) == 1 then
    print_channel stdin
  else
    print_channel (open_in Sys.argv.(1))
