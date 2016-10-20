open Ast

(* equality among types *)
let rec eqTy t1 t2 = match (t1,t2) with
  | (TyInt, TyInt) -> true
  | (TyBool, TyBool) -> true
  | (TyChan t1, TyChan t2) -> eqTy t1 t2
  | (TyFunc (ts1, t1), TyFunc (ts2, t2)) -> eqTy t1 t2 &&
                                            (List.length ts1 == List.length ts2) &&
                                            (List.for_all (fun (t1,t2) -> eqTy t1 t2) (List.combine ts1 ts2))    
       

(*
We assume that the type environment is represented as a list of pairs of variables and types
where variables are represented as strings.

Here's a convenience function which lookups if there's a binding for a variable.
It's actual type is actually slightly more general:
'a -> ('a * 'b) list -> 'b option

 *)
let lookup el lst = try (Some (snd (List.find (fun (el2,_) -> el == el2) lst))) with
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
                                          then  Some a1
                                          else  None
                  | _                 ->  None
                end
                    

  | And(a,b) -> let r = (inferTyExp env a,inferTyExp env b) in 
                begin
                  match r with
                  | (Some a1,Some b1) ->  if a1==TyBool && b1==TyBool
                                          then  Some TyBool
                                          else  None
                  | _                 ->  None
                end

  | Gt (a,b) -> let r = (inferTyExp env a,inferTyExp env b) in 
                begin
                  match r with
                  | (Some a1,Some b1) ->  if a1==TyInt && b1==TyInt
                                          then  Some TyBool
                                          else  None
                  | _                 ->  None
                end
  | Plus(a,b) ->let r = (inferTyExp env a,inferTyExp env b) in 
                begin
                  match r with
                  | (Some a1,Some b1) ->  if a1==TyInt && b1==TyInt
                                          then  Some TyInt
                                          else  None
                  | _                 ->  None
                end
  | Minus(a,b) ->let r = (inferTyExp env a,inferTyExp env b) in 
                begin
                  match r with
                  | (Some a1,Some b1) ->  if a1==TyInt && b1==TyInt
                                          then  Some TyInt
                                          else  None
                  | _                 ->  None
                end
  | Times(a,b) ->let r = (inferTyExp env a,inferTyExp env b) in 
                begin
                  match r with
                  | (Some a1,Some b1) ->  if a1==TyInt && b1==TyInt
                                          then  Some TyInt
                                          else  None
                  | _                 ->  None
                end
  | Division(a,b) ->let r = (inferTyExp env a,inferTyExp env b) in 
                begin
                  match r with
                  | (Some a1,Some b1) ->  if a1==TyInt && b1==TyInt
                                          then  Some TyInt
                                          else  None
                  | _                 ->  None
                end
  | Not a       -> let r=inferTyExp env a in
                begin
                  match r with
                  | Some TyBool -> Some TyBool    
                  | _ -> None
                end
  | RcvExp  v  ->let r = lookup v env in
                begin
                  match r with
                  | Some t -> Some t
                  | None -> None
                end
  | FuncExp (s,el) -> let r = lookup s env in
                begin
                  match  r with
                  | Some TyFunc(l,r) -> if List.length l==List.length el &&
                                      List.for_all2 (fun a b->match (a,inferTyExp env b)with
                                                              | (t1,Some t2) -> eqTy t1 t2
                                                              | _       -> false  )
                                                    l el
                                    then Some r
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
                          | Some t1   ->  let t2 = inferTyExp env e in
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
  | _ -> None
(*
What's still missing are implementations for 

(1) collection of type signatures from functions (procedures)

(2) type checking of procedure bodies, and

(3) type checking of the main program.

 *)                                                     