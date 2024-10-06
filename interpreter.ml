open Miniml

module Env = Map.Make(String)
type value =
  | VInt of int
  | VBool of bool
  | VPair of value * value
  | VClos of string * expr * env
  | VFix of expr * string * value * env 
  | VCstr of string * value list
and env = value Env.t

open Printf
let rec print_value = function
  | VInt n -> printf "%d" n
  | VBool b -> printf "%b" b
  | VPair(v1, v2) -> printf "("; print_value v1; printf ","; print_value v2; printf ")"
  | VClos _ -> printf "<fun>"
  | VFix(Fun _, _, _, _) -> printf "<fun>"
  | VFix _ -> failwith "fix value error"
  | VCstr(c, vlist) -> printf "%s(" c; print_vlist vlist; printf ")"
and print_vlist = function
  | [] -> ()
  | [v] -> print_value v
  | v::vlist -> print_value v; printf ", "; print_vlist vlist

let rec env_from_pattern pat lv env  =
  let rec add_env_list lv lp env = match lv, lp with
  | [], [] -> env
  | v::lv', p::lp' -> 
    let new_env = match v,p with
      | _, PVar s -> Env.add s v env
      | VCstr(_, lval), PCstr(_, lpat) -> add_env_list lval lpat env
      | _ -> failwith "list pattern not good size"
    in add_env_list lv' lp' new_env
  | _ -> failwith "list pattern not good size"
  in
  match pat with
  | PVar s -> failwith "PVar alone in a match"
  | PCstr(s, lpat) -> add_env_list lv lpat env


let rec pattern_match v pat = 
  let rec list_pattern_match lv lp = match lv, lp with
    | [], [] -> true
    | _, [] -> false
    | [], _ -> false
    | v::lv', p::lp' -> (pattern_match v p) && list_pattern_match lv' lp'
  in
  
  match v, pat with
  | VCstr(s, lv), PCstr(name, lp) -> 
    if s = name then
      list_pattern_match lv lp
    else
      false
  | _, PVar _ -> true
  | _, _ -> false

let rec eval e env = match e with
  | Int n -> VInt n
  | Bool b -> VBool b
  | Var x -> Env.find x env
  | Let(x, e1, e2) -> let v1 = eval e1 env in eval e2 @@ Env.add x v1 env
  | Uop(op, e) ->
    begin match op, eval e env with
      | Not, VBool b -> VBool(not b)
      | Minus, VInt n -> VInt(-n)
      | Fst, VPair(v, _) -> v
      | Snd, VPair(_, v) -> v
      | _ -> failwith "unauthorized operation"
    end
  | Bop(op, e1, e2) ->
     begin match op, eval e1 env, eval e2 env with
       | Add, VInt n1, VInt n2 -> VInt (n1 + n2)
       | Sub, VInt n1, VInt n2 -> VInt (n1 - n2)
       | Mul, VInt n1, VInt n2 -> VInt (n1 * n2)
       | Div, VInt n1, VInt n2 -> VInt (n1 / n2)
       | Rem, VInt n1, VInt n2 -> VInt (n1 mod n2)
       | Lsl, VInt n1, VInt n2 -> VInt (Int.shift_left n1 n2)
       | Lsr, VInt n1, VInt n2 -> VInt (Int.shift_right n1 n2)
       | Lt,  VInt n1, VInt n2 -> VBool (n1 < n2)
       | Le, VInt n1, VInt n2 -> VBool (n1 <= n2)
       | Gt, VInt n1, VInt n2 -> VBool (n1 > n2)
       | Ge, VInt n1, VInt n2 -> VBool (n1 >= n2)
       | Eq,  v1,      v2      -> VBool (v1 = v2)
       | Neq, v1,      v2      -> VBool (v1 <> v2)
       | And, VBool b1, VBool b2 -> VBool(b1 && b2)
       | Or, VBool b1, VBool b2 -> VBool(b1 || b2)
       | Pair, v1, v2 -> VPair(v1, v2)
       | _ -> failwith "unauthorized operation"
     end
  | If(c, e1, e2) -> 
     begin match eval c env with
       | VBool b -> if b then eval e1 env else eval e2 env
       | _ -> failwith "unauthorized operation"
     end
  | App(e1, e2) ->
     let x, e, env' = match force @@ eval e1 env with
       | VClos(x, e, env) -> x, e, env
       | _ -> failwith "unauthorized operation"
     in
     let v2 = eval e2 env in 
     eval e (Env.add x v2 env')
  | Fun(x, _, e) -> VClos(x, e, env)
  | Fix(x, _, e) -> let rec v = VFix(e, x, v, env) in v
  | Cstr(s, le) -> VCstr(s, List.map (fun e -> eval e env) le)
  | Match(e, lc) -> 
    let v = eval e env in 
    begin
    match v with
      | VCstr(s, lv) ->     
        let (pat, exp) = List.find (fun (p, e) -> pattern_match v p) lc in
        eval exp (env_from_pattern pat lv env)
      | _ -> failwith "not constructor value in match"
    end

  (* | _ -> failwith "not implemented" *)
and force v = match v with
  | VFix(e, x, v, env) -> force (eval e @@ Env.add x v env)
  | v -> v

let eval_prog p = eval p.code Env.empty
