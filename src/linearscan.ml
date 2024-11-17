open Nimp
open Printf
(*uses interval of liveness of variables *)


(* sort by ascending lower bound, and sort equals by ascending upper bound *)
let sort_2 l =
  List.stable_sort (fun (_, l1, _) (_, l2, _) -> l1 - l2) l
let sort_3 l =
  List.stable_sort (fun (_, _, h1) (_, _, h2) -> h1 - h2) l
let sort_intervals l =
  sort_2 (sort_3 l)

let hastbl_to_list tab = Hashtbl.fold (fun k v acc -> (k,v)::acc) tab []

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

let print_raw_alloc (var,raw) = match raw with
    | RegN n -> print_string var; print_string " ->reg "; print_int n; print_char '\n'
    | Spill n -> print_string var; print_string " ->spill "; print_int n; print_char '\n'

let print_interval (s, low,high) =  printf "%s[%d, %d]\n" s low high
let print_intervals lst = List.iter print_interval lst

let print_list_raw rawtable = List.iter print_raw_alloc (hastbl_to_list rawtable)

(* allocation of the local variables of a function [fdef] using linear scan
   algorithm, with [nb_regs] registers available for allocation ;
   return a raw allocation for each variable, as well as the maximum index of
   used registers, and the number of used stack slots (spills) *)
let lscan_alloc nb_regs fdef =
  let live_intervals, vmap = Liveness.liveness_intervals_from_liveness fdef in (*list of the variables with their corresponding intervals*)
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
        (*TODO what if we want to free but not the last one? is it possible? (spoiler: yeah, and i think we just don't free the stack)*)
        | Spill _ -> failwith "unimplemented spill"
  in
  (* free registers allocated to intervals that stop before timestamp a,
     returns remaining intervals *)
  let rec expire a l = match l with
    | [] -> []
    | (var, high)::l' ->
      if high <= a then (*<=???*)
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
      if (List.exists (fun _ -> true) !free) then
        begin
        (* ... then allocate one *)
        let reg = List.hd !free in
        free := List.tl !free;
        Hashtbl.add alloc xi (RegN reg);
        active := insert_active (xi, hi) !active;
        if !r_max < reg then r_max := reg
        end
      else
        begin
        (* otherwise on the stack*)
        Hashtbl.add alloc xi (Spill !spill_count);
        spill_count := !spill_count + 1;
        end
        (* otherwise, may replace an already used register if this can
            make this register available again earlier *)
        
    ) (sort_intervals live_intervals);
  alloc, !r_max, !spill_count, vmap
