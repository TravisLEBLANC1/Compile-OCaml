open Miniml

module Env = Map.Make(String)
(* typing environment for variables *)
type tenv = typ Env.t
(* typing environment for constructors *)
type senv = (typ list * string) Env.t
(*
type tree = E | N of tree*tree
Env.find "N" senv ==> ([TStruct "tree"; TStruct "tree"], "tree")


*)
open Printf
let rec print_type = function
  | TInt -> printf "TInt"
  | TBool -> printf "TBool"
  | TFun(_,_) -> printf "TFun"
  | TPair(t1,t2) -> printf "TPair("; print_type t1; printf ","; print_type t2; printf ")"
  | TStruct(s) -> printf "TStruct %s" s
and print_type_list = function
  | [] -> ()
  | t::l' -> print_type t; printf "/"; print_type_list l'

(* return true if the list have the same number of elements and the elements are equal one by one*)
let rec eq_list l1 l2 : bool= match l1, l2 with
  | [], [] -> true
  | _, [] -> false
  | [], _ -> false
  | x::l1', y::l2' -> x = y && eq_list l1' l2'




(*return the environement with the variable inside the match added *)
let tenv_from_pattern (pat:pattern) (senv:senv) (tenv:tenv) :tenv = 
  let rec add_env_list (ltyp:typ list) (lpattern:pattern list) (senv:senv) (tenv:tenv):tenv = match ltyp, lpattern with
  | [], [] -> tenv
  | t::ltyp', p::lpattern' -> 
    let new_tenv = match p with
      | PVar s -> 
        if Env.mem s tenv then
          failwith "multiple variables in the pattern"
        else
        Env.add s t tenv
      | PCstr(s,lpat) -> let (lt, _) = Env.find s senv in add_env_list lt lpat senv tenv
    in add_env_list ltyp' lpattern' senv new_tenv
  | _ -> failwith "list pattern not good size"
  in

  match pat with
  | PVar _ -> failwith "PVar alone in a match"
  | PCstr(s, lpat) -> 
    let (lt, _) = Env.find s senv in add_env_list lt lpat senv tenv

let typ_expr (e: expr) (senv: senv) =
  
  (*return the supposed type of the pattern (don't check inside if it's correct)*)
  let rec typ_pattern (pat:pattern) = match pat with
    | PVar _ -> failwith "PVar alone in a match"
    | PCstr(s, _) ->  TStruct(snd @@ Env.find s senv)
  
  (* return (type of pattern, type of expr), fail if the types are not consistent*)
  and check_case (lcase:case list) tenv : typ*typ = match lcase with
    | [] ->  failwith "type error: match empty"
    | [(p, e)] -> 
      let tp = typ_pattern p in 
      let te = typ e (Env.union (fun _ _ b -> Some b) tenv (tenv_from_pattern p senv Env.empty)) in
      (tp, te)
    | (p,e)::lcase' -> 
      let tp = typ_pattern p in
      let te = typ e (Env.union (fun _ _ b -> Some b) tenv (tenv_from_pattern p senv Env.empty)) in
      let (_,t2) = check_case lcase' tenv in
      if t2 <> te then 
        failwith "type error: match result different" (*if all expression don't have the same type*)
        else
        (tp, te)
   
  (* return the type of e in the environment tenv*)
  and typ (e: expr) (tenv: tenv) = match e with
    | Int _ -> TInt
    | Bool _ -> TBool
    | Uop(op, e) ->
      begin match op, typ e tenv with
        | Not, TBool -> TBool
        | Minus, TInt -> TInt
        | Fst, TPair(t1, _) -> t1
        | Snd, TPair(_, t2) -> t2
        | _ -> failwith "type error: uop"
      end
    | Bop(op, e1, e2) ->
      begin match op, typ e1 tenv, typ e2 tenv with
        | (Add | Sub | Mul | Div | Rem | Lsl | Lsr), TInt, TInt -> TInt
        | (Lt | Le | Gt | Ge), TInt, TInt -> TBool
        | Eq, t1, t2 when t1 = t2 -> TBool
        | Neq, t1, t2 when t1 = t2 -> TBool
        | (And | Or), TBool, TBool -> TBool
        | Pair, t1, t2 -> TPair(t1, t2)
        | _ -> failwith "type error: binop"
      end
    | Var(x) -> 
      if not @@ Env.mem x tenv then
        begin
        printf "%s\n" x;
        failwith "variable not found"
        end
      else
        Env.find x tenv
    | Let(x, e1, e2) ->
      let t1 = typ e1 tenv in
      typ e2 (Env.add x t1 tenv)
    | If(c, e1, e2) ->
      begin match typ c tenv, typ e1 tenv, typ e2 tenv with
        | TBool, t1, t2 when t1 = t2 -> t1
        | _ -> failwith "type error: if"
      end
    | App(e1, e2) ->
      begin match typ e1 tenv, typ e2 tenv with
        | TFun(ta, t1), t2 when ta = t2 -> t1
        | _ -> failwith "type error: application"
      end
    | Fun(x, t, e) ->
      TFun(t, typ e (Env.add x t tenv))
    | Fix(x, t, e) ->
      if typ e (Env.add x t tenv) = t then
        t
      else
        failwith "type error: fix"
    | Cstr(c, args) -> 
      if not @@ Env.mem c senv then
        failwith "type error: constructor don't exist"
      else
      let (type_ref, type_name) =  Env.find c senv in
      let arg_type = List.map (fun e -> typ e tenv) args in
      if not @@ eq_list type_ref arg_type  then
        failwith "type error: constructor"
      else
        TStruct type_name
    | Match(e, cases) ->
      let t = typ e tenv in
      let (tp, te) = check_case cases tenv in
      if t <> tp then
        failwith "type error: match" (*if all expression don't have the same type*)
        
      else
        te
    (* | _ -> failwith "not implemented" *)

  in
  typ e Env.empty

let rec add_cstr (lcstr:cstr_decl list) (type_name:string) env = match lcstr with
  | [] -> env
  | (cstr_name, lt)::l' -> add_cstr l' type_name (Env.add cstr_name (lt, type_name) env)

let create_env ltype_decl =
  let rec create_env (ltype_decl:typ_decl list) env = match ltype_decl with
    | [] -> env
    | (type_name, lcstr)::l' -> create_env l' (add_cstr lcstr type_name env)
  in
  create_env ltype_decl Env.empty


let typ_prog p =
  let senv = create_env p.typs in
  ignore(typ_expr p.code senv);
  printf "typecheck done\n"

