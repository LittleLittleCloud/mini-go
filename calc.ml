(* File calc.ml *)
open Ast
open Pp

let _ =
  let lexbuf = Lexing.from_channel stdin in
      let result = Parser.parse Lexer.token lexbuf in
        Printf.printf "%s" (pp_prog result); print_newline(); flush stdout
