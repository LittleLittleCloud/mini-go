open Ast

(* equality among types *)
let rec eqTy t1 t2 = match (t1,t2) with
  | (TyInt, TyInt) -> true
  | (TyBool, TyBool) -> true
  | (TyChan t1, TyChan t2) -> eqTy t1 t2
  | (TyFunc (ts1, t1), TyFunc (ts2, t2)) -> eqTy t1 t2 &&
                                            (List.length ts1 == List.length ts2) &&
                                            (List.for_all (fun (t1,t2) -> eqTy t1 t2) (List.combine ts1 ts2))
  | (_,_) -> false

(*
We assume that the type environment is represented as a list of pairs of variables and types
where variables are represented as strings.

Here's a convenience function which lookups if there's a binding for a variable.
It's actual type is actually slightly more general:
'a -> ('a * 'b) list -> 'b option

 *)
let lookup el lst = try (Some (snd (List.find (fun (el2,_) -> el = el2) lst))) with
                    | Not_found -> None

let update el lst = el :: (List.filter (fun (v,t) -> v <> fst el) lst)

(* Implementation of G |- exp : t where we use 'option' to report failure *)
let rec inferTyExp env e = match e with
  | IConst i -> Some TyInt
  | BConst b -> Some TyBool

  | Var v -> let r = lookup v env in
             begin
               match r with
               | Some t -> Some t
               | None -> None
             end

  | And (e1,e2) -> let r = (inferTyExp env e1, inferTyExp env e2) in
                   begin
                     match r with
                     | (Some t1, Some t2) -> if t1 = TyBool && t2 = TyBool
                                             then Some TyBool
                                             else None
                     | _ -> None
                   end

  | Eq (e1,e2) -> let r = (inferTyExp env e1, inferTyExp env e2) in
                   begin
                     match r with
                     | (Some t1, Some t2) -> if eqTy t1 t2
                                             then Some TyBool
                                             else None
                     | _ -> None
                   end

  | Gt (e1,e2) -> let r = (inferTyExp env e1, inferTyExp env e2) in
                   begin
                     match r with
                     | (Some t1, Some t2) -> if eqTy t1 t2
                                             then Some TyBool
                                             else None
                     | _ -> None
                   end

  | Plus (e1,e2) -> let r = (inferTyExp env e1, inferTyExp env e2) in
                   begin
                     match r with
                     | (Some t1, Some t2) -> if t1 = TyInt && t2 = TyInt
                                             then Some TyInt
                                             else None
                     | _ -> None
                   end

  | Minus (e1,e2) -> let r = (inferTyExp env e1, inferTyExp env e2) in
                   begin
                     match r with
                     | (Some t1, Some t2) -> if t1 = TyInt && t2 = TyInt
                                             then Some TyInt
                                             else None
                     | _ -> None
                   end

  | Times (e1,e2) -> let r = (inferTyExp env e1, inferTyExp env e2) in
                   begin
                     match r with
                     | (Some t1, Some t2) -> if t1 = TyInt && t2 = TyInt
                                             then Some TyInt
                                             else None
                     | _ -> None
                   end

  | Division (e1,e2) -> let r = (inferTyExp env e1, inferTyExp env e2) in
                   begin
                     match r with
                     | (Some t1, Some t2) -> if t1 = TyInt && t2 = TyInt
                                             then Some TyInt
                                             else None
                     | _ -> None
                   end

  | RcvExp v -> let r = lookup v env in
                begin
                  match r with
                  | Some TyChan t1 -> Some t1
                  | _ -> None
                end

  | Not e1 -> let r = inferTyExp env e1 in
              begin
                match r with
                | Some TyBool -> Some TyBool
                | _ -> None
              end

  | FuncExp (v, es) -> let r = lookup v env in
                       begin
                       match r with
                         | Some TyFunc (ts, t) -> if (List.length ts == List.length es) &&
                                                     (List.for_all (fun (e1,t2) -> match (inferTyExp env e1) with
                                                                                   | Some t1 -> eqTy t1 t2
                                                                                   | None -> false)
                                                                   (List.combine es ts))
                                                  then Some t 
                                                  else None
                         | _ -> None
                       end

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

  | Assign (v,e) -> let r = (lookup v env) in 
                    begin
                    match r with
                    | None -> None (* Unknown variable *)
                    | Some t1 -> let t2 = inferTyExp env e in
                                 match t2 with
                                 | None -> None
                                 | Some t3 -> if eqTy t1 t3
                                              then Some env
                                              else None
                    end

  | While (e,s) -> let t = inferTyExp env e in
                  begin
                    match t with
                    | Some TyBool -> let r = typeCheckStmt env s in
                                    begin
                                      match r with
                                      | Some env1 -> Some env
                                      | None -> None
                                    end
                    | Some _ -> None
                    | None -> None
                  end

  (* FIXME: add remaining statements *)
  | _ -> None

(*

What's still missing are implementations for 

(1) collection of type signatures from functions (procedures)

(2) type checking of procedure bodies, and

(3) type checking of the main program.

 *)