(**
   Translation from Clj to Imp

   To avoid name clashes, the translation generates fresh names on the fly 
   for every variable.
 *)

(* Module for variables renamings *)
module STbl = Map.Make(String)


(* Translation of variables
   - named variables are looked up in the renaming table (or accessed
     directly)
   - closure variables generate an array access *)
let tr_var v env = match v with
  | Clj.Name(x) ->
    Imp.(if STbl.mem x env then Var(STbl.find x env) else (print_string (x^" does'nt exist\n"); Var x))
      
  | Clj.CVar(n) ->
    Imp.(array_get (Var "closure") (Int n))

let tr_var' v n env = match v with
   | Clj.Name(x) -> Imp.(if STbl.mem x env then Var(STbl.find x env) else Imp.(array_get (Var "closure") (Int n)))
   | _ -> failwith "i don't know anymore"

(* translate the varlist from a closure
   we add all the variable of the list in the closure array Var(var_cl)*)
let tr_varlist var_cl varlist env = 
   List.mapi (fun i v -> Imp.array_set (Var var_cl) (Int (i + 1)) (tr_var' v (i+1) env)) varlist

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
let tr_expr e env =
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
  
  (* Main translation function
     Return the pair (s, e'), and records variable names in vars as a side effect *)
  let rec tr_expr (e: Clj.expression) (env: string STbl.t):
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
         let vname = new_var "pair" in 
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
         let vname = new_var "if" in
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
         (*je crois qu'il y a un problem du genre le cvar est une ref, et du coup on a les même free variable partout, ou un truc du genre... j'ai envie de le changé en paramètre mais c'est bizzare qu'il l'aie mis du coup*)
         Imp.([Set(var_cl, array_create (Int (1+List.length varlist)))] @
              [array_set (Var var_cl) (Int 0) (Addr fun_name)] @
              tr_varlist var_cl varlist env
            ), Var var_cl
         
      | Clj.App(e1, e2) ->
         let is1, te1 = tr_expr e1 env in
         let is2, te2 = tr_expr e2 env in
         is1 @ is2 , PCall(Deref(Imp.array_get te1 (Int 0)), [te2] @ [te1])

      | _ ->
         failwith "todo tr_expr into imp"

  in
    
  let is, te = tr_expr e env in
  is, te, !vars

    
(* Translation of a global function *)
let tr_fdef fdef =
  let env =
    let x = Clj.(fdef.param) in
    (* The parameter 'x' is renamed 'param_x' *)
    STbl.add x ("param_" ^ x) STbl.empty
  in
  (* The variables created for the translation of the body of the function
     are the local variables of the function *)
  let is, te, locals = tr_expr Clj.(fdef.body) env in
  Imp.({
    name = Clj.(fdef.name);
    code = is @ [Return te];
    (* Two parameters: the actual argument, and the closure *)
    params = ["param_" ^ Clj.(fdef.param); "closure"];
    locals = locals;
  })


(* Translation of a full program *)
let translate_program prog =
  let functions = List.map tr_fdef Clj.(prog.functions) in
  (* Variables of the main expression are the global variables of the program *)
  let is, te, globals = tr_expr Clj.(prog.code) STbl.empty in
  (* Main code ends after printing the result of the main expression *)
  let main = Imp.(is @ [Expr(Call("print_int", [te]))]) in
  Imp.({main; functions; globals})
    
