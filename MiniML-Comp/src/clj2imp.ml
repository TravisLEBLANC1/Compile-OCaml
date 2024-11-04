(**
   Translation from Clj to Imp

   To avoid name clashes, the translation generates fresh names on the fly 
   for every variable.
 *)

(* Module for variables renamings *)
module STbl = Map.Make(String)


let merge_default _ opt_a opt_b = match opt_a,opt_b with
   | None, None -> None
   | Some a, None -> Some a 
   | None, Some b -> Some b
   | Some a, Some _ -> Some a 

(* Translation of variables
   - named variables are looked up in the renaming table (or accessed
     directly)
   - closure variables generate an array access *)
let tr_var v env = match v with
  | Clj.Name(x) ->
    Imp.(if STbl.mem x env then Var(STbl.find x env) else (print_string (x^" does'nt exist\n"); Var x))
      
  | Clj.CVar(n) ->
    Imp.(array_get (Var "closure") (Int n))


(* translate the varlist from a closure
   we add all the variable that we know currently in the closure array*)
(* let rec tr_varlist var_cl varlist env i = match varlist with
   | [] -> []
   | Clj.Name(x)::varlist' -> 
     Imp.(if STbl.mem x env then 
            [array_set (Var var_cl) (Int (i + 1)) (Var(STbl.find x env))] @
            tr_varlist var_cl varlist' env (i+1) 
          else 
            tr_varlist var_cl varlist' env (i+1))
   | Clj.CVar(i)::varlist' ->
      Imp.([array_set (Var var_cl) (Int (i + 1)) (array_get (Var "closure") (Int i))] @
            tr_varlist var_cl varlist' env (i+1)) *)


let tr_varlist var_cl varlist env = 
   List.mapi (fun i v -> Imp.array_set (Var var_cl) (Int (i + 1)) (tr_var v env)) varlist

let print_var v = match v with 
   | Clj.CVar(n) -> Printf.printf "cvar(%d)" n
   | Clj.Name(s) -> Printf.printf "name(%s)" s

let rec print_varlist varlist = match varlist with
   | [] -> ()
   | v::varlist' -> print_var v; Printf.printf ", "; print_varlist varlist'




(* Translation of an expression

   Parameters:
   - e   : Clj expression
   - env : variable renamings

   Result: triple (s, e', vars)
   - s    : Imp instruction sequence
   - e'   : Imp expression
   - vars : list of fresh variable names introduced for renamings
   
   Spec: executing s then evaluating e' in IMP is equivalent to evaluating e in CLJ
*)
let tr_expr e env (cstrs:int Clj.CstrTbl.t) =
  (* Counter for fresh variable names *)
  let cpt = ref (-1) in
  (* List of generated names *)
  let vars = ref [] in
  (* Creation of a variable name of the shape x_N, and recording in vars *)
  let new_var id =
    incr cpt;
    let v = Printf.sprintf "%s_%i" id !cpt in
    vars := v :: !vars;
    v
  in

  (* translate the application using only one tmp variable (instead of creating multiple for just 1 fun)*)
  (* let rec tr_app (e: Clj.expression) (env: string STbl.t) (tmp: string):
      Imp.sequence * Imp.expression = match e with
    | Clj.App(e1, e2) ->
      let is1, te1 = tr_app e1 env tmp in (* e1 should be a MkClj*)
      let is2, te2 = tr_app e2 env tmp in
      Imp.(is1 @ is2 @ [Set(tmp, te1)], PCall(Deref(Imp.array_get (Var tmp) (Int 0)), [te2] @ [Var tmp]))
    | _ -> tr_expr e env *)

   (* return 
      - an expression that give true if e match the pattern
      - a sequence of initialisation of the variables in case of match
      - the environement with those variables added
      *)
   let rec match_cond (e:Imp.expression) (pat:Clj.pattern) (env: string STbl.t) : Imp.expression * Imp.sequence * string STbl.t = 
    match pat with
      | PVar(s) -> let x = new_var s in 
         Bool(true), [Imp.Set(x, e)], (STbl.add s x env)
      
      | PCstr(c, patl) -> 
         let eq_c = Imp.Binop(Ops.Eq, Imp.array_get e (Int 0), Int (Clj.CstrTbl.find c cstrs)) in 
         let map_patl = 
            List.mapi (fun i pat -> match_cond (Imp.array_get e (Int (i+1))) pat env) patl
         in (* first we translate each pattern*)
         let fold_fun (e1, s1, env1) (e2, s2, env2) = 
            Imp.Binop(Ops.And, e1, e2), s1 @ s2, STbl.merge merge_default env1 env2
         in (*then fold them into the result*)
         List.fold_left fold_fun (eq_c,[], env) map_patl
   in 
   (* translate all expression of the list, and place the results in var_c[i]*)
   let rec tr_expr_lst (var_c: string) (el : Clj.expression list) (env: string STbl.t) = 
      let rec tr_internal el index = match el with
         | [] -> []
         | e::el' -> let is, te = tr_expr e env in 
            is@[Imp.(array_set (Var var_c) (Int index) te)] @ tr_internal el' (index+1) 
      in
      tr_internal el 1
   
   (*translate the caselist into a sequence of Imp.If, the result is put in (Var res)*)
   and tr_cases (e: Imp.expression) (res: string) (env: string STbl.t) (casel: Clj.case list) = 
      match casel with
      | [] -> failwith "empty match"
      | [(pat, expr)] ->
         let _, is2, new_env = match_cond e pat env in 
         let is, te = tr_expr expr new_env  in is2 @ is @ [Imp.Set(res, te)]
      | (pat, expr) :: casel -> 
         let cond, is2, new_env = match_cond e pat env in
         let is, te = tr_expr expr new_env in 
         [Imp.If(cond, is2 @ is @ [Imp.Set(res, te)], tr_cases e res env casel)]

  (* Main translation function
     Return the pair (s, e'), and records variable names in vars as a side effect *)
   and tr_expr (e: Clj.expression) (env: string STbl.t):
      Imp.sequence * Imp.expression =
    match e with
      | Clj.Int(n) ->
         [], Imp.Int(n)

      | Clj.Bool(b) ->
         [], Imp.Bool(b)

      | Clj.Var(v) ->
         [], tr_var v env
      
      | Clj.Unop(Fst, e) ->
         let is, te = tr_expr e env in 
         is, Imp.array_get te (Imp.Int 0)
      | Clj.Unop(Snd, e) ->
         let is, te = tr_expr e env in 
         is, Imp.array_get te (Imp.Int 1)
      | Clj.Unop(op, e) ->
         let is, te = tr_expr e env in
         is, Imp.Unop(op, te)
      
      | Clj.Binop(Pair, e1, e2) ->
         let is1, te1 = tr_expr e1 env in
         let is2, te2 = tr_expr e2 env in
         let vname = new_var "var_pair" in 
         let v = Imp.Var(vname) in
         is1 @ is2 @ 
         Imp.([Set(vname, array_create (Int 2))] @
              [array_set v (Int 1) te2 ] @
              [array_set v (Int 0) te1 ]
            ) , v
      | Clj.Binop(op, e1, e2) ->
         let is1, te1 = tr_expr e1 env in
         let is2, te2 = tr_expr e2 env in
         is1 @ is2, Imp.Binop(op, te1, te2)
      
      | Clj.If(cond, e1, e2) ->
         let iscond, tecond = tr_expr cond env in
         let is1, te1 = tr_expr e1 env in
         let is2, te2 = tr_expr e2 env in
         let vname = new_var "var_if" in
         iscond @ 
         [Imp.If(tecond, 
            is1 @ [Imp.Set(vname, te1)], 
            is2 @ [Imp.Set(vname, te2)])], Imp.Var(vname) 

      | Clj.Let(x, e1, e2) ->
         (* Creation of a unique name for 'x', to be used instead of 'x'
            in the expression e2. *)
         let lv = new_var x in
         let is1, t1 = tr_expr e1 env in
         let is2, t2 = tr_expr e2 (STbl.add x lv env) in
         Imp.(is1 @ [Set(lv, t1)] @ is2, t2)
      
      | Clj.MkClj(fun_name, varlist) ->
         let var_cl = new_var "closure" in 
         print_string fun_name;
         print_varlist varlist;
         print_string "\n";
         Imp.([Set(var_cl, array_create (Int (1+List.length varlist)))] @
              [array_set (Var var_cl) (Int 0) (Addr fun_name)] @ 
              tr_varlist var_cl varlist env
            ), Var var_cl
      
      | Clj.Fix(f, Clj.MkClj(fun_name, varlist)) ->
         let var_cl = new_var "closure" in 
         print_string fun_name;
         print_varlist varlist;
         print_string "\n";
         Imp.([Set(var_cl, array_create (Int (1+List.length varlist)))] @
              [array_set (Var var_cl) (Int 0) (Addr fun_name)] @ 
              tr_varlist var_cl varlist (STbl.add f var_cl env)
            ), Var var_cl

      | Clj.App(e1, e2) -> 
         let tmp = new_var "tmp" in
         let is1, te1 = tr_expr e1 env in (* e1 should be a MkClj*)
         let is2, te2 = tr_expr e2 env in
         Imp.(is1 @ is2 @ [Set(tmp, te1)], PCall(Deref(Imp.array_get (Var tmp) (Int 0)), [te2] @ [Var tmp]))
         (* tr_app e env (new_var "tmp") *)

      | Clj.Cstr(c, el) ->
         let nb_expr = List.length el in
         let var_c = new_var @@ String.lowercase_ascii c in
         Imp.([Set(var_c, array_create(Int (1+nb_expr)))] @
            [array_set (Var var_c) (Int 0) (Int (Clj.CstrTbl.find c cstrs))] @
            tr_expr_lst var_c el env
         ), Var var_c


      | Clj.Match(e, casel) ->
         let is1, te1 = tr_expr e env in 
         let res = new_var "var_match" in 
         is1 @ tr_cases te1 res env casel , Var res 
      | _ ->
         failwith "todo tr_expr into imp"

  in
    
  let is, te = tr_expr e env in
  is, te, !vars

    
(* Translation of a global function *)
let tr_fdef fdef cstrs =
  let env =
    let x = Clj.(fdef.param) in
    (* The parameter 'x' is renamed 'param_x' *)
    STbl.add x ("param_" ^ x) STbl.empty
  in
  (* The variables created for the translation of the body of the function
     are the local variables of the function *)
  let is, te, locals = tr_expr Clj.(fdef.body) env cstrs in
  Imp.({
    name = Clj.(fdef.name);
    code = is @ [Return te];
    (* Two parameters: the actual argument, and the closure *)
    params = ["param_" ^ Clj.(fdef.param); "closure"];
    locals = locals;
  })


(* Translation of a full program *)
let translate_program prog =
  let functions = List.map (fun f -> tr_fdef f Clj.(prog.cstrs)) Clj.(prog.functions) in
  (* Variables of the main expression are the global variables of the program *)
  let is, te, globals = tr_expr Clj.(prog.code) (STbl.empty) (prog.cstrs) in
  (* Main code ends after printing the result of the main expression *)
  let main = Imp.(is @ [Expr(Call("print_int", [te]))]) in
  print_string "clj2imp done\n";
  Imp.({main; functions; globals})
    
