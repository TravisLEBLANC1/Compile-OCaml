open Imp
open Nimp

(* use to contains alive variable*)
module VSet = Set.Make(String)
module VMap = Map.Make(String)
type vmap = (int*int) VMap.t
(* returns the set of variables accessed by the expression [e] *)
let rec use_expr e = match e with
  | Cst _ | Bool _ -> VSet.empty
  | Var x -> VSet.singleton x
  | Binop (_, e1, e2) -> VSet.union (use_expr e1) (use_expr e2)
  | Call(_, le) -> List.fold_left (fun acc e -> VSet.union acc (use_expr e)) VSet.empty le

let liveness fdef =
  let n = max_instr_list fdef.code in
  let live = Array.make (n+1) VSet.empty in
  (* returns the set of variable that live in entry to the numbered 
     instruction [i], assuming a set of live variables [lv_out] on 
     exit of [i] *)
  let rec lv_in_instr (i:instruction) lv_out = 
    let s = match i.instr with
      (* by case on the contents of i.instr *)
      | Putchar e | Return e | Expr e -> VSet.union lv_out (use_expr e)
      | Set(s, e) -> VSet.union (VSet.remove s lv_out) (use_expr e)
      | If(e, li1, li2) -> 
        let tmp = VSet.union (lv_in_list li1 lv_out) (lv_in_list li2 lv_out) in
        VSet.union tmp (use_expr e)
      | While(e, li) -> 
        let tmp_in = VSet.union (use_expr e) lv_out in
        let tmp_out = lv_in_list li tmp_in in
        let s = lv_in_list li (VSet.union tmp_in tmp_out) in
        s; (* in a while expression, there is no Definition possible, so nothing to remove from tmp_out*)
    in
    live.(i.nb) <- s;
    s;
  (* the same for a sequence, and records in [live] the live sets computed
     on entry to each analyzed instruction *)
  and lv_in_list (l:sequence) lv_out = match l with
    | [] -> lv_out
    | i::l' -> 
      let lv_out_i = lv_in_list l' lv_out in
      lv_in_instr i  lv_out_i
  in
  let _ = lv_in_list fdef.code VSet.empty in
  live

let liveness_intervals_from_liveness fdef =
  (* for each variable [x], create the smallest interval that contains all
     the numbers of instructions where [x] is live *)
  let live = liveness fdef in
  let rec update_vmap (index:int) (lv_in:string list) (vmap:vmap) = match lv_in with
    | [] -> vmap
    | s::l' -> 
      let new_vamp = 
        if VMap.mem s vmap then
          let (low, high) = VMap.find s vmap in
          VMap.add s (low, index) vmap
        else
          VMap.add s (index, index) vmap
      in
      update_vmap index l' new_vamp 
  in
  (* first we fold the array to create a vmap of var -> (low,high), 
  then we fold the map to create a list (var, low, high)*)
  let (_,vmap) = Array.fold_left (fun (index, map) vset -> (index+1, update_vmap index (VSet.to_list vset) map)) (0, VMap.empty) live
  in
  VMap.fold (fun s (low, high) l -> (s,low,high)::l) vmap  []

