(**
   Translation from MiniML to Clj (closure conversion)
 *)

(* Module for sets of variables *)
module VSet = Set.Make(String)

(* return the tail only if the list is non empty*)
let safe_tl lst = if List.exists (fun _ -> true) lst then List.tl lst else lst

(* change an mkclj to false, don't change the other expressions*)
let mkclj_false (e:Clj.expression) = match e with
  | Clj.MkClj(s, _, vl) -> Clj.MkClj(s, false, vl)
  | x -> x 

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
  let tr_expr (e: Miniml.expr) (bvars: VSet.t): Clj.expression * (string * int) list =
    (* association list, maps free variables to indices in the closure *)
    let cvars = ref [] in
    let cpt = ref 0 in (* indices will start at 1 *)
    (* utility function, to be called for each new free variable:
       - records the variable in cvars
       - returns the index *)
    let new_cvar =
      fun x -> incr cpt; cvars := (x, !cpt) :: !cvars; !cpt
    in

    let varlist_from_cvars = fun () ->
      List.map (fun (x,_) -> Clj.Name(x)) (List.sort (fun (_,i) (_,j) -> i - j) !cvars )
    in 

    (* Translation of a variable, extending the closure if needed *)
    let rec convert_var x bvars =
      Clj.(if VSet.mem x bvars
        then Name(x) (* if the variable is bound, keep its identifier *)
        else if List.mem_assoc x !cvars (* if the variable has already been recorded *)
        then CVar(List.assoc x !cvars) (* then take its index *)
        else CVar(new_cvar x))        (* otherwise, create and record a new index *)
    
    and crawl_closure e bvars : Clj.expression = match e with
      | Miniml.Fun(x, _, (Fun(_) as f)) ->
        let fun_name = new_fname () in
        let new_fdef = Clj.({name=fun_name; body=crawl_closure f bvars; param=x}) in
        let closure = Clj.MkClj(fun_name, true, varlist_from_cvars ()) in
        fdefs := new_fdef::!fdefs;
        closure
      (* case where e is not a fun*)
      | Miniml.Fun(x, _, e) ->
        let fun_name = new_fname () in
        let new_fdef = Clj.({name=fun_name; body=crawl_closure e (VSet.add x bvars); param=x}) in
        let closure = Clj.MkClj(fun_name, true, varlist_from_cvars ()) in
        fdefs := new_fdef::!fdefs;
        closure
      | _ -> crawl e bvars

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

      | Fun(_) as f -> 
        let result = mkclj_false @@ crawl_closure f (VSet.empty) in 
        cvars := [];
        cpt := 0;
        result

      | App(e1, e2) ->
        App(mkclj_false @@ crawl e1 bvars, crawl e2 bvars)
      
      | _ ->
         failwith "todo mini to clj"

    in
    (* Return the result of the translation, and the list of variables to be 
       put in the closure *)
    let te = crawl e bvars in
    te, !cvars

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
  })
