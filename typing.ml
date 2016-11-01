open Ast

(* equality among types *)
let rec eqTy t1 t2 = match (t1,t2) with
  | (TyInt, TyInt) -> true
  | (TyBool, TyBool) -> true
  | (TyChan t1, TyChan t2) -> eqTy t1 t2
  | (TyFunc (ts1, s1), TyFunc (ts2,s2)) -> 
                                          begin
                                            match (s1,s2) with
                                          | (Some t1,Some t2 ) -> (eqTy t1 t2 &&
                                                                  (List.length ts1 == List.length ts2) &&
                                                                  (List.for_all (fun (t1,t2) -> eqTy t1 t2) (List.combine ts1 ts2))) 
                                          | (None,None) -> (List.length ts1 == List.length ts2) &&
                                                                  (List.for_all (fun (t1,t2) -> eqTy t1 t2) (List.combine ts1 ts2))
                                          | _           ->false
                                          end
  | _ ->                                 false

(*
We assume that the type environment is represented as a list of pairs of variables and types
where variables are represented as strings.

Here's a convenience function which lookups if there's a binding for a variable.
It's actual type is actually slightly more general:
'a -> ('a * 'b) list -> 'b option

 *)
let lookup el lst = try (Some (snd (List.find (fun (el2,_) -> el = el2) lst))) with
                    | Not_found -> None
                          
let update el env= let nenv=List.filter (fun a ->fst a<>fst el) env in el::nenv


(* Implementation of G |- exp : t where we use 'option' to report failure *)
let rec inferTyExp env e = match e with
  | IConst i -> Some TyInt
  | BConst b -> Some TyBool
  | Var v    -> let r = lookup v env in
                begin
                  match r with
                  | Some t -> Some t
                  | None -> None
                end

  | Eq(a,b) ->  let r = (inferTyExp env a,inferTyExp env b) in 
                begin
                  match r with
                  | (Some a1,Some b1) ->  if eqTy a1 b1
                                          then Some TyBool
                                          else None
                  | _                 ->  None
                end
                    

  | And(a,b) -> let r = (inferTyExp env a,inferTyExp env b) in 
                begin
                  match r with
                  | (Some TyBool,Some TyBool) -> Some TyBool
                  | _                 ->  None
                end

  | Gt(a,b) -> let r = (inferTyExp env a,inferTyExp env b) in 
                begin
                  match r with
                  | (Some TyInt,Some TyInt) -> Some TyBool
                  | _                 ->  None
                end
  | Plus(a,b) | Minus(a,b) | Times(a,b) | Division(a,b) 
            ->let r = (inferTyExp env a,inferTyExp env b) in 
                begin
                  match r with
                  | (Some TyInt,Some TyInt) ->  Some TyInt
                  | _                 ->  None
                end
  | Not a       -> let r=inferTyExp env a in
                begin
                  match r with
                  | Some TyBool -> Some TyBool    
                  | _ -> None
                end
  | RcvExp v  ->let r = lookup v env in
                begin
                  match r with
                  | Some t -> Some t
                  | None -> None
                end
  | FuncExp (s,el) -> let r1 = lookup s env in
                begin
                  match  r1 with
                  | Some TyFunc(l,r) -> if List.length l==List.length el &&
                                      List.for_all2 (fun a b->match (a,inferTyExp env b)with
                                                              | (t1,Some t2) -> eqTy t1 t2
                                                              | _       -> false  )
                                                    l el
                                    then 
                                    begin        
                                      match r with
                                      | Some t1 -> r
                                      | None -> None
                                    end
                                    else None
                  | _           ->  None
                end 
  (* add remaining cases *)               

(* Implementation of G | (stmt : Cmd | G) where we simply skip Cmd and as above
  use 'option' to report failure 

  As discussed, there's a bit of design space when it comes to 'nested' type declarations.
  The below is just a sketch.

*)
let rec typeCheckStmt env stmt = match stmt with
  | Decl (v,e) -> let r = inferTyExp env e in
                  begin
                    match r with
                    | None -> None
                    | Some t -> Some (update (v,t) env)
                  end
  | Assign (v,e) -> begin
                    match (lookup v env) with
                    | None -> None (* Unknown variable *)
                    | Some t1 -> let t2 = inferTyExp env e in
                                 match t2 with
                                 | None -> None
                                 | Some t3 -> if eqTy t1 t3
                                              then Some env
                                              else None
                    end
  | Transmit (s,e)  ->  let r=lookup s env in
                        begin
                          match r with
                          | Some (TyChan t1)   ->  let t2 = inferTyExp env e in
                                          begin
                                            match t2 with
                                             | None -> None
                                             | Some t3 -> if eqTy t1 t3
                                                          then Some env
                                                          else None 
                                          end
                          | _ -> None
                        end
  | Seq (s1,s2)     ->  let r = typeCheckStmt env s1 in 
                        begin
                          match r with
                              | Some e1    -> let e2=List.fold_right update e1 env in
                                              let r2 = typeCheckStmt e2 s2 in 
                                              begin
                                                match r2 with
                                                | Some e3 -> Some e3
                                                | _       -> None   
                                              end
                              | _ -> None    
                        end  
  | Go s            ->  let e2 = typeCheckStmt env s in     (*can re-decl in this block*)
                        begin
                          match e2 with
                          | Some e3 -> Some env
                          | _ -> None
                        end       
  | RcvStmt s       ->  let s1=lookup s env in 
                        begin
                          match s1 with
                          | Some e1 -> Some env
                          | _ -> None
                        end
  | DeclChan s      ->  Some (update (s,TyChan TyInt) env)
  | While (e,s)     ->  let r=(inferTyExp env e,typeCheckStmt env s) in 
                        begin
                          match r with
                          | (Some TyBool,Some s2) -> Some env
                          | _ -> None
                        end
  | ITE (e,s1,s2)   ->  let r =(inferTyExp env e,typeCheckStmt env s1,typeCheckStmt env s2) in 
                        begin
                          match r with
                          | (Some TyBool,Some _,Some _) -> Some env
                          | _ -> None
                        end
  | Return e        ->  let r = (inferTyExp env e, lookup "_RETURN_TYPE_" env) in 
                        begin
                          match r with
                          | (Some t1,Some t2) ->  if eqTy t1 t2 
                                                  then Some env
                                                  else None     
                          | _ -> None
                        end
  | FuncCall (s,el)->  let r1 = (lookup s env) in 
                        begin
                          match r1 with
                          | Some (TyFunc(l,r)) -> if List.length l==List.length el &&
                                      List.for_all2 (fun a b->match (a,inferTyExp env b)with
                                                              | (t1,Some t2) -> eqTy t1 t2
                                                              | _       -> false  )
                                                    l el
                                    then Some env
                                    else None
                          | _                 ->  None
                        end
  | Print e         ->  let r = inferTyExp env e in 
                        begin
                          match r with
                          | Some _ -> Some env
                          | _ -> None
                        end
  |Skip             ->  Some env
let rec typeCollectProc env proc = match proc with
| Proc (funcId, argLst, ret, stmt) ->  
                      let addArgToEnv arg tmpEnv =
                        begin
                          match arg with
                          | (Var argId, argTy) -> update (argId, argTy) tmpEnv
                          | _ -> tmpEnv
                        end
                      in
                        let argEnv = List.fold_right addArgToEnv argLst env in 
                        let argTyLst = List.map (fun x -> snd x) argLst in
                        let funcEnv = update (funcId, TyFunc(argTyLst, ret)) argEnv in 
                        let funcEnvWithRetType = ( match ret with
                          | Some retTy -> update ("_RETURN_TYPE_", retTy) funcEnv
                          | None       -> funcEnv )
                        in
                        let stmtTy = typeCheckStmt funcEnvWithRetType stmt in 
                          begin
                            match stmtTy with
                            | Some _ -> Some (update (funcId, TyFunc(argTyLst, ret)) env)
                            | _ -> None
                          end
(*                          
| Proc (s,lst,None,stmt) -> 
                        let tmp v e=
                        begin
                          match v with
                          | (Var s,t) -> update (s,t) e
                          | _ -> e
                        end
                        in
                        let e1=List.fold_right tmp lst env in 
                        let tylst=List.map (fun x->snd x) lst in
                        let e2=update (s,TyFunc(tylst,None)) e1 in 
                        let r=typeCheckStmt e2 stmt in 
                        begin
                          match r with
                          | Some _ -> Some (update (s,TyFunc(tylst,None)) env)
                          | _ ->  None
                        end
| Proc (s,lst,Some ty,stmt) ->  
                        let tmp v e=
                        begin
                          match v with
                          | (Var s,t) -> update (s,t) e
                          | _ -> e
                        end
                        in
                        let e1=List.fold_right tmp lst env in 
                        let tylst=List.map (fun x->snd x) lst in
                        let e2=update (s,TyFunc(tylst,Some ty)) e1 in 
                        let e3 = update ("1FUNCALL",ty) e2 in
                        let r=typeCheckStmt e3 stmt in 
                        begin
                          match r with
                          | Some t -> Some (update (s,TyFunc(tylst,Some ty)) env)
                          | _ -> None
                        end
*)
let rec checkProg env prog=match prog with
| Prog(lst,stmt) -> let tmp p e=let e1=typeCollectProc e p in 
                                begin
                                  match e1 with
                                  | Some e2 -> List.fold_right update e2 e
                                  | _ -> []
                                end
                    in let l1=List.fold_right tmp lst env in
                    let r=typeCheckStmt l1 stmt in
                    begin
                      match r with
                      | Some t -> r
                      | None   -> None
                    end


