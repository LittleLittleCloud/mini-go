(* File pp.ml *)
open Ast
open Pp_ast
open Pp_fmt

let pretty_print ch =
  let lexbuf = Lexing.from_channel ch in
      let result = Parser.parse Lexer.token lexbuf in
        Printf.printf "AST:\n%s\n\nPretty Print:\n%s\n"
        (Pp_ast.pp_prog result)
        (Pp_fmt.pp_prog result);
        flush stdout

let ch = match (Array.length Sys.argv) with
  | 1 -> stdin
  | _ -> (open_in Sys.argv.(1))

let _ =
  pretty_print ch
  