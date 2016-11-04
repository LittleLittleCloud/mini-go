open Ast

(* equality among types *)
let nameSupply=ref 0
let getNameSupply _ =let x = !nameSupply in x

let freshName _ =  nameSupply := !nameSupply + 1;
                   String.concat "" ["$";string_of_int (!nameSupply )] 
(*
We assume that the type environment is represented as a list of pairs of variables and types
where variables are represented as strings.

Here's a convenience function which lookups if there's a binding for a variable.
It's actual type is actually slightly more general:
'a -> ('a * 'b) list -> 'b option

 *)
let lookup el lst = let a=List.find (fun el2 -> el = (fst el2)) lst in (snd a)
                          
let update el env= let nenv=List.filter (fun a ->fst a<>fst el) env in el::nenv



let rec collectStmt env stmt=match stmt with
  | Decl (v,e)        ->  update (v,freshName()) env
  | Assign (v,e)      ->  env
  | Transmit (s,e)    ->  env
  | Seq (s1,s2)       ->  let r = collectStmt env s1 in 
                          let r2=collectStmt r s2 in 
                          r2
  | Go s              ->  let r=collectStmt env s in 
                          r
  | RcvStmt s         ->  env
  | DeclChan s        ->  update (s,freshName()) env
  | While (e,s)   ->  env
  | WHILE (e,s)  -> let r=collectStmt env s in 
                          r
  | ITE (e,s1,s2)   ->  env
  | ITE1 (e,s1,s2)  ->  let r1=collectStmt env s1 in 
                                let r2=collectStmt r1 s2 in 
                                r2

  | Return e        ->  env
  | FuncCall (s,el)->   env
  | Print e         ->  env
  | Skip             ->  env

let rec collectProc env proc=match proc with
| Proc(s,lst,_,stmt) -> env
|Proc1(_,lst,_,stmt)    -> let sst=List.map (fun f->match (fst f) with
                                                        | Var s  -> s) lst in 
                                let stmtst=List.map2 (fun f e->Decl(f,fst e)) sst lst in 
                                let args=List.fold_right (fun f s->Seq(f,s)) stmtst Skip in
                                let e=collectStmt env args in 
                                let e2=collectStmt e stmt in 
                                e2

let rec collectProg env prog=match prog with
| Prog (_,stmt) -> collectStmt env stmt


let rec renameExp env e = match e with
  |IConst i   ->IConst i
  |BConst b   ->BConst b 
  | Var v    -> let r = lookup v env in
                Var  r

  | Eq(a,b) ->  let r = (renameExp env a,renameExp env b) in 
                begin
                  match r with
                  | (a1,b1) ->  Eq(a1,b1)
                end
                    

  | And(a,b) -> let r = (renameExp env a,renameExp env b) in 
                begin
                  match r with
                  |(a1,b1)  ->And(a1,b1)
                end

  | Gt(a,b) -> let r = (renameExp env a,renameExp env b) in 
                begin
                  match r with
                  |(a1,b1)  ->Gt(a1,b1)
                end
  | Plus(a,b) -> let r = (renameExp env a,renameExp env b) in 
                begin
                  match r with
                  |(a1,b1)  ->Plus(a1,b1)
                end

  | Minus(a,b) -> let r = (renameExp env a,renameExp env b) in 
                begin
                  match r with
                  |(a1,b1)  ->Minus(a1,b1)
                end
  | Times(a,b) -> let r = (renameExp env a,renameExp env b) in 
                begin
                  match r with
                  |(a1,b1)  ->Times(a1,b1)
                end
  | Division(a,b) -> let r = (renameExp env a,renameExp env b) in 
                begin
                  match r with
                  |(a1,b1)  ->Division(a1,b1)
                end
  | Not a       -> let r=renameExp env a in
                begin
                  match r with
                  | a1  ->Not a1
                end
  | RcvExp v  ->let r = lookup v env in
                begin
                  match r with
                  | v1  ->RcvExp v1
                end
  | FuncExp (s,el) -> let est=List.map (fun f->renameExp env f) el in
                      FuncExp(s,est)
  (* add remaining cases *)               

(* Implementation of G | (stmt : Cmd | G) where we simply skip Cmd and as above
  use 'option' to report failure 

  As discussed, there's a bit of design space when it comes to 'nested' type declarations.
  The below is just a sketch.

*)
let rec renameStmt env stmt = match stmt with
  | Decl (v,e) -> let r = renameExp env e in
                  let s= lookup v env in 
                  Decl(s,r)

  | Assign (v,e) -> let r = renameExp env e in
                    let s= lookup v env in 
                    Assign(s,r)
  | Transmit (s,e)  ->  let r = renameExp env e in
                        let v= lookup s env in 
                        Transmit(v,r)
  | Seq (s1,s2)     ->  let (r1,r2)=(renameStmt env s1,renameStmt env s2) in 
                        Seq(r1,r2)  
  | Go s            ->  let r1=renameStmt env s in 
                        Go r1
  | RcvStmt s       ->  let r=lookup s env in 
                        RcvStmt r

  | While (e,s)     ->  let tmp=WHILE(e,s) in 
                            let r=collectStmt  env tmp in 
                            let s1=renameStmt r s in 
                            let e1=renameExp env e in 
                            While(e1,s1)
  | ITE (e,s1,s2)   ->  let e1=renameExp env e in 
                        let tmp=ITE1(e,s1,s2) in 
                        let r=collectStmt env tmp in 
                        let ss1=renameStmt r s1 in 
                        let ss2=renameStmt r s2 in 
                        ITE(e1,ss1,ss2)
  | DeclChan s      ->  let r=lookup s env in 
                        DeclChan r                        
  | Return e        ->  let e1=renameExp env e in 
                        Return e1
  | FuncCall (s,el)->  let el1=List.map (fun f->renameExp env f) el in 
                        FuncCall(s,el1)
  | Print e         ->  let e1=renameExp env e in 
                        Print e1 
  |Skip             ->  Skip
let rec renameProc env proc = match proc with
| Proc(s,el,t,stmt) -> let _P=Proc1(s,el,t,stmt) in 
                            let e1=collectProc env _P in 
                            let el1=List.map (fun f->(renameExp e1 (fst f)),snd f) el in 
                            let st1=renameStmt e1 stmt in 
                            Proc(s,el1,t,st1)
| _->                proc
let rec renameProg env prog=match prog with
| Prog (pst,stmt) ->  let e1=collectProg env prog in 
                      let pst1=List.map (fun f->renameProc e1 f) pst in 
                      let stmt1=renameStmt e1 stmt in 
                      Prog(pst1,stmt1)



