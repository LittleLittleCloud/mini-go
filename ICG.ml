open ICGType
open Ast
let labelSupply = ref 1
let env1= ref []
let env2=ref []
let env3=ref []
let chgLabelSupply x =labelSupply:=x
let freshLabel _ =  labelSupply := !labelSupply + 1;
                    !labelSupply


let update (el:(string*int)) env= let nenv=List.filter (fun a ->fst a<>fst el) env in el::nenv
let lookup (el:(string)) lst = snd(List.find (fun t->(fst t)=el) lst)
let freshName _=String.concat ""  [string_of_int (freshLabel() )] 
(* (parts) of translation of Booleans (short-circuit evaluation!),
   yields a tuple where first component represents the IRC and
   second component a variable name which is bound to the result *)

let irc_ZeroJump (x,l) = let y = freshName() in
                         [IRC_Assign (y, IRC_Not x);
                          IRC_NonzeroJump (y,l)]


let rec translateB env exp = match exp with
  | BConst true -> let x = freshName() in
                   ([IRC_Assign (x, IRC_IConst 1)], x)
  | BConst false -> let x = freshName() in
                    ([IRC_Assign (x, IRC_IConst 0)], x)                      
  | And (e1, e2) -> 
                    let r1 = translateE env e1 in
                    let r2 = translateE env e2 in
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
                    
  | Eq (e1,e2)  ->    let r1=translateE env e1 in 
                      let r2=translateE env e2 in 
                      let x = freshName() in 
                      ((fst r1)
                        @
                        (fst r2)
                        (* @
                        [IRC_Assign(tmp,IRC_Minus((snd r1),(snd r2)))]
                        @(irc_ZeroJump (tmp,l1) )
                        @[IRC_Assign(x,IRC_IConst 0);IRC_Goto l2]
                        @[IRC_Label l1;IRC_Assign(x,IRC_IConst 1);IRC_Label l2], *)
                        @
                        [IRC_Assign(x, IRC_Eq(snd r1,snd r2))],
                      x)

  | Gt (e1,e2)  ->    let r1=translateE env e1 in 
                      let r2=translateE env e2 in 
                      let tmp1=freshName() in 
                      let x = freshName() in 
                      let l1=freshLabel() in 
                      let l2 = freshLabel() in
                      ((fst r1)
                        @
                        (fst r2)
                        @
                        [IRC_Assign(tmp1,IRC_Gt((snd r1),(snd r2)))]
                        @irc_ZeroJump(tmp1,l1)
                        @[IRC_Assign(x,IRC_IConst 1);IRC_Goto l2]
                        @[IRC_Label l1;IRC_Assign(x,(IRC_IConst 0));IRC_Label l2],
                      x)  

  | Not e1      ->    let r1=translateE env e1 in 
                      let x = freshName() in 
                      ((fst r1)
                        @
                        [IRC_Assign(x,IRC_Not(snd r1))]
                        ,x
                      )

and 
translateV env exp = match exp with
| IConst i      ->  let x = freshName() in 
                    ([IRC_Assign(x,IRC_IConst i)],x)

| Plus (e1,e2)  ->  let r1=translateE env e1 in 
                    let r2=translateE env e2 in
                    let x= freshName() in 
                    (
                      (fst r1)
                      @
                      (fst r2)
                      @
                      [IRC_Assign(x,IRC_Plus(snd r1,snd r2))],
                      x
                    )   
| Minus (e1,e2) ->  let r1=translateE env e1 in 
                    let r2=translateE env e2 in
                    let x= freshName() in 
                    ((fst r1)
                      @
                      (fst r2)
                      @
                      [IRC_Assign(x,IRC_Minus((snd r1),snd r2))],
                      x
                    )
| Times(e1,e2)  ->  let r1=translateE env e1 in 
                    let r2=translateE env e2 in
                    let x= freshName() in 
                    ((fst r1)
                      @
                      (fst r2)
                      @
                      [IRC_Assign(x,IRC_Times((snd r1),snd r2))],
                      x
                    )
| Division(e1,e2)-> let r1=translateE env e1 in 
                    let r2=translateE env e2 in
                    let x= freshName() in 
                    ((fst r1)
                      @
                      (fst r2)
                      @
                      [IRC_Assign(x,IRC_Division((snd r1),snd r2))],
                      x
                    )  
| Var s         ->  ([],s)
| RcvExp s      ->  let x=freshName() in 
                    ([IRC_Assign(x,IRC_Var(s))],x)

| FuncExp (s,est) -> let x=freshName() in
                      let l=freshLabel() in 
                      let rtAddr=freshName() in  
                      let pst=List.map (fun f->translateE env f) est in
                      env1:=update ("return",l) !env1;
                      
                      (
                      [IRC_Assign(rtAddr,IRC_IConst l)]
                      @
                      [IRC_Param rtAddr]
                      @
                      List.fold_left (fun a f->a@f) [] (List.map fst pst)
                      @
                      (List.map (fun f->IRC_Param f) (List.map snd pst))
                      @
                      [IRC_Call ((lookup s !env1),(List.length est))]        (*change this *)
                      @
                      [IRC_Label l]
                      @
                      [IRC_Get x],x
                      )
and
translateE env exp=match exp with
| And(_,_)|Eq(_,_)|Gt(_,_)|Not _|BConst _  -> (translateB env exp)
| _ -> (translateV env exp)


let rec translateStmt env stmt=match stmt with
| Seq (s1,s2)     -> (translateStmt env s1)@(translateStmt env s2)
| Go s            -> let t=translateStmt env s in 
                      [IRC_Thread (IRC t)]

| DeclChan s      -> [IRC_Assign(s,IRC_Var(s))]
| Transmit(s,exp)|Assign(s,exp)|Decl(s,exp)   ->begin
                      match exp with
                      | IConst i      ->  [IRC_Assign(s,IRC_IConst i)]
                      | BConst true   ->  [IRC_Assign(s,IRC_IConst 1)]
                      | BConst false  ->  [IRC_Assign(s,IRC_IConst 0)]
                      | Var s1        ->  [IRC_Assign(s,IRC_Var s1)]
                      | _             -> let e=translateE env exp in 
                                          (fst e )
                                          @
                                          [IRC_Assign(s,IRC_Var(snd e))]
                    end
|While (exp,s)    ->  let e=translateE env exp in 
                      let st=translateStmt env s in 
                      let l1=freshLabel() in
                      let l2=freshLabel() in
                      
                      [(IRC_Label l2)]
                      @(fst e)
                      @
                      
                      irc_ZeroJump ((snd e),l1)
                      @
                      st
                      @[(IRC_Goto l2)]
                      @[(IRC_Label l1)]

| ITE (exp,s1,s2)  -> let e=translateE env exp in 
                      let st1=translateStmt env s1 in 
                      let st2=translateStmt env s2 in 
                      let l1=freshLabel() in 
                      let l2=freshLabel() in 
                      (fst e)
                      @
                      (irc_ZeroJump ((snd e),l1))
                      @
                      st1
                      @
                      [(IRC_Goto l2)]
                      @
                      [(IRC_Label l1)]
                      @
                      st2
                      @
                      [(IRC_Label l2)]

| Return exp       -> let e=translateE env exp in 
                      let rddr=freshName() in 

                      (fst e)
                      @ !env3
                      @[IRC_Get rddr]
                      @[IRC_Param (snd e)]
                      @[IRC_Param rddr]
                      @ !env2
                      @
                      [IRC_GotoE rddr]

| FuncCall (s,est)  ->let l=freshLabel() in  
                      
                      let pst=List.map (fun f->translateE env f) est in
                      let rtAddr=freshName() in  
                      env1:=update ("return",l) !env1;
                      [IRC_Assign(rtAddr,IRC_IConst l)]
                      @
                      [IRC_Param rtAddr]
                      @
                      List.fold_left (fun a f->a@f) [] (List.map fst pst)
                      @
                      (List.map (fun f->IRC_Param f) (List.map snd pst))
                      @
                      [IRC_Call ((lookup s !env1),(List.length est))]        (*change this *)
                      @
                      [IRC_Label l]

| Print exp       -> let e=translateE env exp in 
                      (fst e)
                      @
                      [IRC_Print (snd e)]
|_                ->[]


let rec translateProc env proc=match proc with
                                

| Proc (s,lst,Some ty,stmt) ->  let l=freshLabel() in  
                                let tmp= !labelSupply in
                                env1:=update (s,l) !env1;
                                let stmt1=translateStmt env stmt in 
                                let vars=List.concat (List.map (fun f->match f with
                                                                        | IRC_Assign(s,_) -> [s]
                                                                        |_  ->[]) stmt1) in 


                                let lst1=List.rev lst in 
                                let lstS=List.map (fun s->let e=translateE env (fst s) in
                                                          IRC_Get (snd e)) lst1 in 
                                let st=List.map (fun f->match f with
                                | IRC_Get s -> s) lstS in 
                                let st=List.sort_uniq (fun f1 f2->(int_of_string f1)-int_of_string f2 ) (List.append st vars) in 
                                let savebuf=List.map (fun s->IRC_Assign(freshName(),IRC_Var s)) st in 
                                let pushStack=List.map (fun s->match s with
                                | IRC_Assign(s1,IRC_Var s2) -> IRC_Param s1) savebuf in 
                                let popbuf=List.map (fun s->match s with
                                | IRC_Assign(s1,IRC_Var s2) -> IRC_Get s1) (List.rev savebuf) in

                                let load = List.map (fun s->match s with
                                | IRC_Assign(s1,IRC_Var s2) -> IRC_Assign(s2,IRC_Var s1)) savebuf in 
                                env2:=load;
                                env3:=popbuf;
                                labelSupply:=tmp;
                                let stmt1=translateStmt env stmt in 
                                [IRC_Label l]
                                @
                                savebuf
                                @
                                lstS
                                @
                                pushStack
                                @
                                stmt1
                                


| Proc(s,lst,None,stmt)     ->  let l=freshLabel() in
                                env1:=update (s,l) !env1;
                                let rddr=freshName() in
                                let stmt1=translateStmt env stmt in 
                                let lst1=List.rev lst in 
                                let lstS=List.map (fun s->let e=translateE env (fst s) in
                                                          IRC_Get (snd e)) lst1 in  
                                
                                [IRC_Label l]
                                @
                                lstS
                                @
                                stmt1
                                @
                                [IRC_Get rddr]
                                @
                                [IRC_Param rddr]
                                @
                                [IRC_Param rddr]
                                @
                                [IRC_GotoE rddr]
                                

let rec translateProg env prog = match prog with
| Prog (pst,stmt) ->  let exit = freshLabel() in 
                      let procst=List.fold_left (fun a f->a@(translateProc env f)) [] pst in 
                      let s=translateStmt env stmt in 
                      IRC (s@[(IRC_Goto exit)]@procst@[(IRC_Label exit)])
