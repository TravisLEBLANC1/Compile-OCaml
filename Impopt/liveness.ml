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
      | Putint e | Putchar e | Expr e -> VSet.union lv_out (use_expr e)
      | Return e -> use_expr e
      | Set(s, e) -> VSet.union (VSet.remove s lv_out) (use_expr e)
      | If(e, li1, li2) -> 
        let tmp = VSet.union (lv_in_list li1 lv_out) (lv_in_list li2 lv_out) in
        VSet.union (VSet.union tmp lv_out) (use_expr e)
      | While(e, li) -> 
        let tmp_in = VSet.union (use_expr e) lv_out in
        let tmp_out = lv_in_list li tmp_in in
        let s = lv_in_list li (VSet.union tmp_in tmp_out) in(* in a while expression, there is no Definition possible, so nothing to remove from tmp_out*)
        VSet.union tmp_in s;  (*we need to add again tmp_in because in the begin of the while, there is e (otherwise it would be the live_in of the first expr) *)
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
  let vmap = ref VMap.empty in

  (* update the vmap according the the lv_in of the index instruction*)
  let update_vmap (index:int) (lv_in:VSet.t) = 
    let is_not_global var = List.mem var fdef.locals || List.mem var fdef.params in

    let update_vmap_internal (s:string) :unit = match VMap.find_opt s !vmap with
    | Some (low, high) -> vmap := VMap.add s (low, index) !vmap
    | None -> vmap := VMap.add s (index, index) !vmap
    in
    (* we filter to get rid of global variable*)
    VSet.iter update_vmap_internal (VSet.filter is_not_global lv_in) 
  in
  
  (*first we create the vmap according the the liveness*)
  Array.iteri update_vmap live;
  (* then we create the list in the right way ie (string, int, int) list*)
  VMap.fold (fun s (low, high) acc -> (s, low, high)::acc) !vmap []

