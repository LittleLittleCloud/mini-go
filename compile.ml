(* File compile.ml *)
open Ast
open Typing
open Pp_ast
open ICGType
open ICG
open Pp_print_ICG
open Pp_fmt
open FormatAST
open Typing
open RenameAST
open VMType
open VMC
open Pp_print_VMC
open VM
let pretty_print ch =
  let lexbuf = Lexing.from_channel ch in
      let result = Parser.parse Lexer.token lexbuf in
      let t=Typing.checkProg [] result in
      match t with
      | Some _   -> let fmtAST=FormatAST.normalizeProg result in 
                    let renameAST=RenameAST.renameProg [] fmtAST in
                    let i=RenameAST.getNameSupply() in 
                    ICG.chgLabelSupply i;
                    let icg=ICG.translateProg [] renameAST in 
                    let vmc=VMC.genVMC icg in 
                      Printf.printf "VM Code:\n%s\n"
                      (Pp_print_VMC.pp_vmc vmc);
                      (*VM.run vmc;*)
                      flush stdout

      | None      -> print_endline "error";
                      VM.run [Halt];
                      flush stdout














(* 
      let fmtAST=FormatAST.normalizeProg result in 
      let renameAST=RenameAST.renameProg [] fmtAST in
      let i=RenameAST.getNameSupply() in 
      ICG.chgLabelSupply i;
      let icg=ICG.translateProg [] renameAST in 
      let vmc=VMC.genVMC icg in 
      if t!=None then
      begin
        Printf.printf "AST:\n%s\n\n%s\nPretty Print:\n%s\nICG:\n%s\nVM:\n%s\n"
        (Pp_ast.pp_prog result) 
        (Pp_fmt.pp_prog fmtAST)
        (Pp_fmt.pp_prog renameAST)
        (Pp_print_ICG.pp_irc icg)
        (Pp_print_VMC.pp_vmc vmc);
        VM.run vmc
      end
      flush stdout *)

let ch = match (Array.length Sys.argv) with
  | 1 -> stdin
  | _ -> (open_in Sys.argv.(1))

let _ =
  pretty_print ch
  