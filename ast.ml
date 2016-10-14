(* Abstract Syntax Tree (AST) representation of Mini-Go *)

type prog = Prog of (proc list) * stmt

and proc = Proc of string * ((exp * types) list) * (types option) * stmt

and types = TyInt
           | TyBool
           | TyChan of types
           | TyFunc of (types list * types)
                                                                       
and stmt = Seq of stmt * stmt
          | Go of stmt
          | Transmit of string * exp
          | RcvStmt of string 
          | Decl of string * exp
          | DeclChan of string
          | Assign of string * exp
          | While of exp * stmt
          | ITE of exp * stmt * stmt
          | Return of exp
          | FuncCall of string * (exp list)
          | Print of exp
          
and exp = And of exp * exp
         | Eq of exp * exp
         | Gt of exp * exp
         | Plus of exp * exp
         | Minus of exp * exp
         | Times of exp * exp
         | Division of exp * exp
         | Not of exp
         | RcvExp of string
         | IConst of int
         | BConst of bool
         | Var of string
         | FuncExp of string * (exp list)


let rec pp_exp s = match s with
  | And (x,y)    -> String.concat "" ["And("; pp_exp x; ","; pp_exp y; ")"]
  | Eq (x,y)     -> String.concat "" ["Eq("; pp_exp x; ","; pp_exp y; ")"]
  | Gt (x,y)     -> String.concat "" ["Gt("; pp_exp x; ","; pp_exp y; ")"]
  | Plus (x,y)   -> String.concat "" ["Plus("; pp_exp x; ","; pp_exp y; ")"]
  | Minus (x,y)  -> String.concat "" ["Minus("; pp_exp x; ","; pp_exp y; ")"]
  | Times (x,y)  -> String.concat "" ["Times("; pp_exp x; ","; pp_exp y; ")"]
  | Division (x,y)  -> String.concat "" ["Division("; pp_exp x; ","; pp_exp y; ")"]
  | Not (x)      -> String.concat "" ["Not("; pp_exp x; ")"]
  | RcvExp (x)   -> String.concat "" ["RcvExp("; x; ")"]
  | IConst (x)   -> String.concat "" ["IConst("; string_of_int x; ")"]
  | BConst (x)   -> String.concat "" ["BConst("; string_of_bool x; ")"]
  | Var (x)      -> String.concat "" ["Var("; x; ")"]
  | FuncExp(x, y)  -> String.concat "" ["FuncExp("; x; ",["; (pp_exp_list y); "])"]
and pp_exp_list s = match s with
  | [] -> ""
  | hd::tl -> String.concat " " [pp_exp hd; pp_exp_list tl]

