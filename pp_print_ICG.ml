open Ictype
let rec pp_exp exp =match exp with
| IRC_And (s1,s2)		-> s1^" && "^s2
| IRC_Eq(s1,s2)			-> s1^" == "^s2 
| IRC_Gt (s1,s2) 		-> s1^" > "^s2
| IRC_Plus (s1,s2) 		-> s1^" + "^s2
| IRC_Minus (s1,s2) 		-> s1^" - "^s2
| IRC_Times (s1,s2) 		-> s1^" * "^s2
| IRC_Division (s1,s2) 		-> s1^" / "^s2
| IRC_Not s1 				->"!"^s1
| IRC_IConst i 				-> string_of_int i 
| IRC_Var s 				-> s 

let rec pp_cmd cmd=match cmd with
| IRC_Assign (s,exp)	-> s^":="^pp_exp exp
|IRC_Label i 			->	string_of_int i^":"
| IRC_Goto i 			-> 	"jmp "^string_of_int i 
|IRC_NonzeroJump (s,i) 	->	"if "^s^" ==0"^"jmp "^string_of_int i 
| IRC_Param s -> 			"Param "^ s
| IRC_Call (i,j)->			"Call "^string_of_int i ^" "^string_of_int j
| IRC_RETURN	 ->			"return "
| IRC_Get s 	->			"pop "^ s 
| IRC_NewThreadBegin 	->  "NewThreadBegin" 
| IRC_NewThreadEnd 		->	"NewThreadEnd"
| IRC_RETURNE s 		->	"return "^s
| IRC_Print s 			->	"print " ^s


let rec pp_irc  _irc=match _irc with
| IRC lst-> String.concat "\n" (List.map pp_cmd lst)