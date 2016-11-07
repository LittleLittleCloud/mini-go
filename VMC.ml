open ICGType
open VMType
let lineNum=ref 0
let label =ref []
let update (el:(string*int)) env= let nenv=List.filter (fun a ->fst a<>fst el) env in el::nenv
let lookup (el:(string)) lst = snd(List.find (fun t->(fst t)=el) lst)
let setLine _= lineNum:=!lineNum+1;
				!lineNum

let rec genVMCS icg=match icg with
| IRC_Assign (s,exp)-> 	let e=genVMCE exp in 
						let vmc= 
						e
						@
						[AssignFromStack(1,int_of_string s);PopS] in 
						lineNum:=!lineNum+List.length vmc;
						vmc
| IRC_Label i 		-> 	label:=update (string_of_int i,!lineNum) !label;[]
| IRC_Goto i 		->	let vmc=
						[JumpI (string_of_int i)]
						in 
						lineNum:=!lineNum+List.length vmc;
						vmc

| IRC_GotoE s 		->	let vmc=
						[Jr]
						in
						lineNum:=!lineNum+List.length vmc;
						vmc

| IRC_NonzeroJump (s,i) -> let vmc=
							[PushToStack (int_of_string s)]
							@
							[NonZeroI (string_of_int i)]
							in 
							lineNum:=!lineNum+List.length vmc;
							vmc
| IRC_Param s 			->	let vmc=[PushToStack (int_of_string s)]
							in 
							lineNum:=!lineNum+List.length vmc;
							vmc
| IRC_Call (i1,i2)		->	let vmc=
							[Shift (- (i2+1))]
							@
							[PushS (!lineNum + 4)]
							@
							[Shift (i2)]
							@
							[JumpI (string_of_int i1)]
							in
							lineNum:= !lineNum+List.length vmc;
							vmc
| IRC_Get s 			->	let vmc=[AssignFromStack(1,int_of_string(s));PopS] 
							in 
							lineNum:=!lineNum+List.length vmc;
							vmc
| IRC_Thread irc 		->  let vmc=genVMCIRC irc in 
							lineNum:=!lineNum+List.length vmc;
							[Thread vmc]
| IRC_Print s 			->	let vmc = 
							[ PushToStack (int_of_string s)]
							@
							[Output;PopS]
							in
							lineNum:=!lineNum+List.length vmc;
							vmc
and genVMCE exp =match exp with
| IRC_And(s1,s2) 		-> 	let vmc=
							[PushToStack (int_of_string s1)]
							@
							[PushToStack (int_of_string s2)]
							@
							[And]
							in
							vmc
| IRC_Eq(s1,s2)			->	let vmc=
							[PushToStack (int_of_string s1)]
							@
							[PushToStack (int_of_string s2)]
							@
							[Eq]
							in
							vmc
| IRC_Gt(s1,s2)			->	let vmc=
							[PushToStack (int_of_string s1)]
							@
							[PushToStack (int_of_string s2)]
							@
							[Gt]
							in
							vmc
| IRC_Plus(s1,s2)			->	let vmc=
							[PushToStack (int_of_string s1)]
							@
							[PushToStack (int_of_string s2)]
							@
							[Add]
							in
							vmc
| IRC_Minus(s1,s2)			->	let vmc=
							[PushToStack (int_of_string s1)]
							@
							[PushToStack (int_of_string s2)]
							@
							[Sub]
							in
							vmc

| IRC_Division(s1,s2)			->	let vmc=
							[PushToStack (int_of_string s1)]
							@
							[PushToStack (int_of_string s2)]
							@
							[Div]
							in
							vmc
| IRC_Times(s1,s2)			->	let vmc=
							[PushToStack (int_of_string s1)]
							@
							[PushToStack (int_of_string s2)]
							@
							[Mult]
							in
							vmc

| IRC_Not s1			->	let vmc=
							[PushToStack (int_of_string s1)]
							@
							[Not]
							in
							vmc
| IRC_IConst i 			->	let vmc=
							[PushS i]
							in
							vmc
| IRC_Var s 			->	let vmc=
							[PushToStack (int_of_string s)]
							in 
							vmc
and genVMCIRC irc=	match irc with
| IRC lst -> let a=List.map genVMCS lst in 
						let vmc=List.concat a in 
						vmc	

let updateLabel vmc=match vmc with
| NonZeroI s 	->let i=lookup s !label in 
					NonZero i
| JumpI s 		-> let i = lookup s !label in 
					Jump i
| vmc 			-> vmc

let genVMC icg = let vmci = genVMCIRC icg in 
				let vmc= List.map updateLabel vmci in 
				let result=vmc@[Halt] in 
				result