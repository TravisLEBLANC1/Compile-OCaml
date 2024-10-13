open Imp
open Nimp
(*uses interval of liveness of variables *)


(* sort by ascending lower bound, and sort equals by ascending upper bound *)
let sort_2 l =
  List.stable_sort (fun (_, l1, _) (_, l2, _) -> l1 - l2) l
let sort_3 l =
  List.stable_sort (fun (_, _, h1) (_, _, h2) -> h1 - h2) l
let sort_intervals l =
  sort_2 (sort_3 l)

(* insert interval [i] in active list [l] 
   pre/post-condition: sorted by ascending upper bound *)

let rec insert_sorted f i l = match l with
  | [] -> [i]
  | elt::l' -> 
    if f elt i then 
      elt::insert_sorted f i l'
    else
      elt::i::l'

let insert_free = insert_sorted (fun elt i -> elt < i)
let insert_active = insert_sorted (fun elt i -> snd elt < snd i)

(* raw allocation information for a variable *)
type raw_alloc =
  | RegN  of int  (* index of the register *)
  | Spill of int  (* index of the spill *)

(* allocation of the local variables of a function [fdef] using linear scan
   algorithm, with [nb_regs] registers available for allocation ;
   return a raw allocation for each variable, as well as the maximum index of
   used registers, and the number of used stack slots (spills) *)
let lscan_alloc nb_regs fdef =
  let live_intervals = Liveness.liveness_intervals_from_liveness fdef in (*list of the variables with their corresponding intervals*)
  let alloc = Hashtbl.create (List.length fdef.locals) in (* associate variable to his "raw_alloc"*)
  let active = ref [] in (* temporary list of active variables? *)
  let free = ref (List.init nb_regs (fun i -> i)) in (* temporary list of free registers*) 
  let r_max = ref (-1) in  (* maximum index of used register *)
  let spill_count = ref 0 in (* number of spilled variables *)

  (*assuming it is already in alloc*)
  let free_register (var:string) =
    let raw = Hashtbl.find alloc var in 
      match raw with
        | RegN reg -> free := insert_free reg !free
        | Spill spil -> failwith "unimplemented"
  in
  (* free registers allocated to intervals that stop before timestamp a,
     returns remaining intervals *)
  let rec expire a l = match l with
    | [] -> []
    | (var, high)::l' ->
      if high < a then (*<=???*)
        begin 
        free_register var;
        expire a l'
        end
      else
        l
  in
  (* for each interval i, in sorted order *)
  List.iter (fun i ->
      let xi, li, hi = i in
      (* free registers that expire before the lower bound of i *)
      active := expire li !active;
      (* if there are available registers *)
      if not (List.is_empty !free) then
        (* ... then allocate one *)
        let reg = List.hd !free in
        free := List.tl !free;
        Hashtbl.add alloc xi (RegN reg);
        active := insert_active (xi, hi) !active;
        if !r_max < reg then r_max := reg
      else
        (* otherwise, may replace an already used register if this can
            make this register available again earlier *)
        failwith "not implemented"
    ) (sort_intervals live_intervals);
  alloc, !r_max, !spill_count
