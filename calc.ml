(* File calc.ml *)
open Ast

let _ =
  let lexbuf = Lexing.from_channel stdin in
      let result = Parser.parse Lexer.token lexbuf in
        Printf.printf "%s" (pp_exp result); print_newline(); flush stdout

