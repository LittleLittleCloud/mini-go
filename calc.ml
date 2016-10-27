(* File calc.ml *)
open Ast
open Lexer
open Parser
open Typing

let make_ast filename =
      Parser.parse Lexer.token (Lexing.from_channel (open_in filename))

let type_check filename =
      Typing.checkProg [] (make_ast filename)

