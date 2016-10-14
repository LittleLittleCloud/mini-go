(* File pp2.ml *)
open Ast

let rec pp_prog s = match s with
  | Prog (x, y)       -> String.concat "\n" [(pp_proc_list x); "{"; (pp_stmt y); "}"]

and pp_proc s = match s with
  | Proc (x, y, z, w) -> String.concat "" ["Func "; x; "("; (pp_param_list y); ") "; (pp_types_option z); "{\n"; (pp_stmt w); "\n}"]

and pp_proc_list s = match s with
  | []                -> ""
  | hd::tl            -> String.concat "\n" [pp_proc hd; pp_proc_list tl]

and pp_param_list s = match s with
  | []                -> ""
  | (ex,ty)::[]       -> pp_exp ex ^ " " ^ pp_types ty
  | (ex,ty)::tl       -> pp_exp ex ^ " " ^ pp_types ty ^ ", " ^ pp_param_list tl

and pp_types s = match s with
  | TyInt             -> "Int"
  | TyBool            -> "Bool"
  | TyChan (x)        -> "Chan " ^ (pp_types x)
  | TyFunc (x, y)     -> "Func (" ^ (pp_types_list x) ^ ") " ^ (pp_types y)

and pp_types_list s = match s with
  | []                -> ""
  | hd::[]            -> pp_types hd
  | hd::tl            -> pp_types hd ^ ", " ^ pp_types_list tl

and pp_types_option s = match s with
  | Some(x)           -> pp_types x ^ " "
  | None              -> ""

and pp_stmt s = match s with
  | Seq (x, y)        -> String.concat ";\n" [pp_stmt x; pp_stmt y]
  | Go (x)            -> String.concat "" ["Go {\n"; pp_stmt x; "\n}"]
  | Transmit (x, y)   -> String.concat "<-" [x; pp_exp y]
  | RcvStmt (x)       -> String.concat "<-" [""; x]
  | Decl (x, y)       -> String.concat ":=" [x; pp_exp y]
  | DeclChan (x)      -> String.concat ":=" [x; "newChannel"]
  | Assign (x, y)     -> String.concat "=" [x; pp_exp y]
  | While (x, y)      -> String.concat "" ["While "; pp_exp x; " {\n"; pp_stmt y; "\n}"]
  | ITE (x, y, z)     -> String.concat "" ["If "; pp_exp x; " {\n"; pp_stmt y; "\n} else {\n"; pp_stmt z; "\n}"]
  | Return (x)        -> String.concat "" ["Return "; pp_exp x]
  | FuncCall (x, y)   -> String.concat "" [x; "("; (pp_exp_list y); ")"]
  | Print (x)         -> String.concat "" ["Print "; pp_exp x]

and pp_exp s = match s with
  | And (x,y)         -> String.concat "&&" [pp_exp x; pp_exp y]
  | Eq (x,y)          -> String.concat "==" [pp_exp x; pp_exp y]
  | Gt (x,y)          -> String.concat ">" [pp_exp x; pp_exp y]
  | Plus (x,y)        -> String.concat "+" [pp_exp x; pp_exp y]
  | Minus (x,y)       -> String.concat "-" [pp_exp x; pp_exp y]
  | Times (x,y)       -> String.concat "*" [pp_exp x; pp_exp y]
  | Division (x,y)    -> String.concat "/" [pp_exp x; pp_exp y]
  | Not (x)           -> String.concat "!" [pp_exp x]
  | RcvExp (x)        -> String.concat "<-" [""; x]
  | IConst (x)        -> string_of_int x
  | BConst (x)        -> string_of_bool x
  | Var (x)           -> x
  | FuncExp(x, y)     -> String.concat "" [x; "("; (pp_exp_list y); ")"]

and pp_exp_list s = match s with
  | []                -> ""
  | hd::[]            -> pp_exp hd
  | hd::tl            -> pp_exp hd ^ ", " ^ pp_exp_list tl
