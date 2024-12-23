(**
   Translation from MiniML to Clj (closure conversion)
 *)

(* Module for sets of variables *)
module VSet = Set.Make(String)

(* return the tail only if the list is non empty*)
let safe_tl lst = if List.exists (fun _ -> true) lst then List.tl lst else lst

let rec vars_from_pat pat = match pat with
  | Miniml.PVar(s) -> VSet.singleton s 
  | Miniml.PCstr(s, patl) -> List.fold_left (fun set pat -> VSet.union set (vars_from_pat pat)) (VSet.singleton s) patl

(* let print_cvar (x, i) = Printf.printf "(%s, %d)" x i

let rec print_cvars = function
  | [] -> ()
  | cvar :: cvars -> print_cvar cvar ; print_string ", "; print_cvars cvars *)

let translate_program (p: Miniml.prog) =
  (* List of global function definitions *)
  let fdefs = ref [] in
  (* Generate a unique function name, for creating a Clj global function
     from a MiniML anonymous function *)
  let new_fname =
    let cpt = ref (-1) in
    fun () -> incr cpt; Printf.sprintf "fun_%i" !cpt
  in
  
  (* Translation of an expression
     Parameters:
     - e     : MiniML expression
     - bvars : set of bound variable names 
              (bound locally, i.e. not free in the full expression)

     Result: pair (e', l)
     - e' : Clj expression
     - l  : association list, maps each free variable to its index in the current closure
   *)
  let rec tr_expr (e: Miniml.expr) (bvars: VSet.t): Clj.expression * (string * int) list =
    (* association list, maps free variables to indices in the closure *)
    let cvars = ref [] in
    (* utility function, to be called for each new free variable:
       - records the variable in cvars
       - returns the index *)
    let new_cvar =
      let cpt = ref 0 in (* indices will start at 1 *)
      fun x -> incr cpt; cvars := (x, !cpt) :: !cvars; !cpt
    in
    (* translate the pattern from miniml to a pattern of clj*)
    let rec tr_pat pat = 
      let tr_pat_lst patl =
        List.map tr_pat patl
      in
      match pat with
        | Miniml.PVar(s) -> Clj.PVar(s)
        | Miniml.PCstr(s, pl) -> Clj.PCstr(s, tr_pat_lst pl)
    in

    (* Translation of a variable, extending the closure if needed *)
    let rec convert_var x bvars =
      Clj.(if VSet.mem x bvars
        then Name(x) (* if the variable is bound, keep its identifier *)
        else if List.mem_assoc x !cvars (* if the variable has already been recorded *)
        then CVar(List.assoc x !cvars) (* then take its index *)
        else CVar(new_cvar x))        (* otherwise, create and record a new index *)

    (* Translation of an expression (main loop)
       Returns the translated expression, and extend the closure as a side effect *)
    and crawl (e: Miniml.expr) bvars: Clj.expression = match e with
      | Int(n) ->
        Int(n)

      | Bool(b) ->
        Bool(b)
          
      | Var(x) ->
        Var(convert_var x bvars)
      
      | Uop(op, e) ->
        Unop(op, crawl e bvars)
      
      | Bop(op, e1, e2) ->
        Binop(op, crawl e1 bvars, crawl e2 bvars)
      
      | If(cond, e1, e2) ->
        If(crawl cond bvars, crawl e1 bvars, crawl e2 bvars)
      
      (* The range of 'x' in 'let x = e1 in e2' is the expression e2:
         add x in the bound variables when translating e2 *)
      | Let(x, e1, e2) ->
        Let(x, crawl e1 bvars, crawl e2 (VSet.add x bvars))

      | Fun(x, _, e) -> 
        let body, sub_cvars = tr_expr e (VSet.singleton x) in
        let fun_name = new_fname () in
        let new_fdef = Clj.({name=fun_name; body=body; param=x}) in
        fdefs := new_fdef :: !fdefs;
        (* conver_var automaticaly add the free variable we don't know yet to cvars*)
        let sorted_cvars = List.sort (fun (_,i) (_,j) -> i-j) sub_cvars in
        Clj.MkClj(fun_name, List.map (fun (x, _) -> convert_var x bvars) sorted_cvars)

      | Fix(f, _, e) ->
        let res = crawl e (VSet.add f bvars) in
        Fix(f, res)

      | App(e1, e2) ->
        App(crawl e1 bvars, crawl e2 bvars)
      
      | Cstr(c, el) ->
        Cstr(c, List.map (fun e -> crawl e bvars) el)
      
      | Match(e1, cl) ->
        let mapf (pat, e2) = (tr_pat pat, crawl e2 (VSet.union (vars_from_pat pat) bvars)) in
        Match(crawl e1 bvars, List.map mapf cl)

    in
    (* Return the result of the translation, and the list of variables to be 
       put in the closure *)
    let te = crawl e bvars in
    te, !cvars

  in
  let map_cstr typs = 
    let map = ref Clj.CstrTbl.empty in 
    let map_cstr_internal cl =
      List.iteri (fun i (c, _) -> map := Clj.CstrTbl.add c i !map) cl
    in
    List.iter (fun (_, cl) -> map_cstr_internal cl) typs ;
    !map
  in
  (* Translation of a full program:
     - start with an empty set of bound variables
     - collect the translated main expression and the global function definitions *)
  (* Remark: for the main expression, the set of free variables collected in cvars
     shall be empty (otherwise, the program has undefined variables!) *)
  let code, cvars = tr_expr p.code VSet.empty in
  assert (cvars = []);
  print_string "mini2clj done\n";
  Clj.({
    functions = !fdefs;
    code = code;
    cstrs = map_cstr p.typs
  })
