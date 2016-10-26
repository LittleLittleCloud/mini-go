let labelSupply = ref 1
let freshLabel _ =  labelSupply := !labelSupply + 1;
                    !labelSupply

let freshName _=String.concat "" ["IRGtmp" ; string_of_int (freshLabel )] 
(* (parts) of translation of Booleans (short-circuit evaluation!),
   yields a tuple where first component represents the IRC and
   second component a variable name which is bound to the result *)

let irc_ZeroJump (x,l) = let y = freshName() in
                         [IRC_Assign (y, IRC_Not x);
                          IRC_NonzeroJump (y,l)]

let rec translateB exp = match exp with
  | BConst true -> let x = freshName() in
                   ([IRC_Assign (x, IRC_IConst 1)], x)
  | BConst false -> let x = freshName() in
                    ([IRC_Assign (x, IRC_IConst 0)], x)                      
  | And (e1, e2) -> (* 
                       e1.code;
                       if !e1.result goto l1
                       e2.code;
                       x = e2.result;
                       goto l2;
                       l1:
                       x = 0;             Booleans represented as integers
                       l2:
                     *)

                    let r1 = translateB e1 in
                    let r2 = translateB e2 in
                    let x = freshName() in
                    let l1 = freshLabel() in
                    let l2 = freshLabel() in                   
                    ((fst r1)
                     @
                     (irc_ZeroJump (snd r1,l1))
                     @
                     (fst r2)
                     @
                     [IRC_Assign (x, IRC_Var (snd r2));
                      IRC_Goto l2 ]                       
                     @
                     [IRC_Label l1;
                      IRC_Assign (x, IRC_IConst 0);
                      IRC_Label l2],
                     x)
                    
  | Eq (e1,e2)  ->    let r1=translateV e1 in 
                      let r2=translateV e2 in 
                      let tmp=freshName() in 
                      let x = freshName() in 
                      let l1=freshLabel() in 
                      let l2 = freshLabel()
                      ((fst r1)
                        @
                        (fst r2)
                        @
                        [IRC_Assign(tmp,IRC_Minus(IRC_Var (snd r1),IRC_Var (snd r2)))]
                        @(irc_ZeroJump (tmp,l1) )
                        @[IRC_Assign(x,IRC_IConst 0);IRC_Goto l2]
                        @[IRC_Label l1;IRC_Assign(x,IRC_IConst 1);IRC_Label l2],
                      x)