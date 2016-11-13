open VMType

let rec pp_cmd cmd=match cmd with
| PushS i		->"PushS "^string_of_int i
| PopS			->"PopS"
| Add			->"Add"
| Sub  			->"Sub"
| Mult  			->"Mult"
| Div -> "Div"
| Halt	->"Halt"
| Lt ->		"Lt"
| Gt -> "Gt"
| Halt ->"Halt"
| Eq -> "Eq"
| And -> "And"
| Not -> "Not"
| Shift i -> "SP "^string_of_int i 
| Output -> "Output"
| NonZero i -> "NonZero "^string_of_int i 
| Zero i -> 	"Zero "^ string_of_int i 
| NonZero i ->	"NonZero "^ string_of_int i 
| Jump i -> 	"Jump"^ string_of_int i 
| Jr -> 		"Jr"
| Assign (i1,i2) -> "$"^string_of_int i1 ^" = "^string_of_int i2
| PushToStack i -> "PushS $"^string_of_int i
| AssignFromStack (i1,i2) ->"PopS $"^string_of_int i1^" $"^string_of_int i2 
| PushE i		->"PushE "^string_of_int i
| PopE			->"PopE"
| PushToEnv i -> "PushE $"^string_of_int i
| AssignFromEnv (i1,i2) ->"PopE $"^string_of_int i1^" $"^string_of_int i2 
| Thread ist			->"Thread{\n"^pp_vmc ist^"\n}"
| Lock i 				->"Lock by Thread"^string_of_int i 
| Unlock i 				->"UnLock by Thread"^string_of_int i

and pp_vmc  _vmc=match _vmc with
| lst-> String.concat "\n" (List.map pp_cmd lst)