(* File pp.ml *)
open Ast
open Typing
open Pp_ast
open ICG
open ICGType
open Pp_print_ICG
open Pp_fmt
open FormatAST
open RenameAST
let pretty_print ch =
  let lexbuf = Lexing.from_channel ch in
      let result = Parser.parse Lexer.token lexbuf in
      let fmtAST=FormatAST.normalizeProg result in 
      let renameAST=RenameAST.renameProg [] fmtAST in 
      let icg=ICG.translateProg [] renameAST in 
        Printf.printf "AST:\n%s\n%s\n\nPretty Print:\n%s\nICG:\n%s\n"
        (Pp_ast.pp_prog result)
        
        (Pp_fmt.pp_prog fmtAST)
        (Pp_fmt.pp_prog renameAST)
        (Pp_print_ICG.pp_irc icg);
        flush stdout

let ch = match (Array.length Sys.argv) with
  | 1 -> stdin
  | _ -> (open_in Sys.argv.(1))

let _ =
  pretty_print ch
  