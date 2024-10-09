open Imp
open Nimp

(* use to contains alive variable*)
module VSet = Set.Make(String)

(* returns the set of variables accessed by the expression [e] *)
let rec use_expr (e:expression):VSet = match e with
  | Cst _ -> VSet.empty
  | Var x -> VSet.singleton x
  | (*TODO...*)
  | _ -> failwith "not implemented"

let liveness fdef =
  let n = max_instr_list fdef.code in
  let live = Array.make (n+1) VSet.empty in
  (* returns the set of variable that live in entry to the numbered 
     instruction [i], assuming a set of live variables [lv_out] on 
     exit of [i] *)
  let rec lv_in_instr (i:instruction) (lv_out:VSet) :VSet = match i.instr with
    (* by case on the contents of i.instr *)
    | Putchar(e) -> let s = VSet.union lv_out (use_expr e) in 
                    live.(i.nb) <- s;
                    s;
    | (*WHile? it's enought to do 2 turn in the loop*)
    | (*TODO *)
    | _ -> failwith "not implemented"
  (* the same for a sequence, and records in [live] the live sets computed
     on entry to each analyzed instruction *)
  and lv_in_list l lv_out = match l with
    | [] -> lv_out
    | i::l' -> 
      let lv_out_i = lv_in_list l' incr in
      lv_in_instr i  lv_out_i
    | _ -> failwith "not implemented"
  in
  let _ = lv_in_list fdef.code VSet.empty in
  live

let liveness_intervals_from_liveness fdef =
  let live = liveness fdef in
  (* for each variable [x], create the smallest interval that contains all
     the numbers of instructions where [x] is live *)
  failwith "not implemented"
