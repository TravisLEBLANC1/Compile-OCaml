(**
   Abstract syntax for the IMP language, with instructions numbered.
   It is assumed that every instruction in a given function body has
   a unique number.
 *)

type binop = Ops.binop
type expression = Imp.expression

(*nb is to have a unique nb for each instr for liveness*)
type instruction = { nb: int; instr: instr }
and instr =
  | Putint  of expression
  | Putchar of expression
  | Set     of string * expression
  | If      of expression * sequence * sequence
  | While   of expression * sequence
  | Return  of expression
  | Expr    of expression
  | Write   of expression * expression (* write a memory address *)
      
and sequence = instruction list

type function_def = {
    name: string;
    params: string list;
    locals: string list;
    code: sequence;
  }

type program = {
    globals: string list;
    functions: function_def list;
  }

(**
   Numbering and utility functions.
 *)

let rec max_instr i = match i.instr with
    | Putint _ | Putchar _ | Set _ | Expr _ | Return _ -> i.nb
    | While(_, is) -> max i.nb (max_instr_list is)
    | If(_, is1, is2) -> max i.nb (max (max_instr_list is1) (max_instr_list is2))
    | Write(_, _) -> i.nb 
and max_instr_list = function
    | [] -> -1
    | i::l -> max (max_instr i) (max_instr_list l)

let from_imp_fdef fdef =
  (* Printf.printf "numbering %s..." Imp.(fdef.name); *)
  let cpt = ref (-1) in
  let new_lbl () = incr cpt; !cpt in
  let rec from_imp_instr = function
    | Imp.Putint e  -> { nb = new_lbl(); instr = Putint(e) }
    | Imp.Putchar e -> { nb = new_lbl(); instr = Putchar(e) }
    | Imp.Set(x, e) -> { nb = new_lbl(); instr = Set(x, e) }
    | Imp.Expr e    -> { nb = new_lbl(); instr = Expr(e) }
    | Imp.Return e  -> { nb = new_lbl(); instr = Return(e) }
    | Imp.While(e, is1) -> 
       let nb = new_lbl() in
       { nb; instr = While(e, from_imp_list is1) }
    | Imp.If(e, is1, is2) -> 
       let nb = new_lbl() in
       { nb; instr = If(e, from_imp_list is1, from_imp_list is2) }
    | Imp.Write(e1, e2) -> {nb = new_lbl(); instr = Write(e1, e2)}
  and from_imp_list = function
    | []   -> []
    | i::l -> let i = from_imp_instr i in i :: from_imp_list l
  in
  let code = from_imp_list Imp.(fdef.code) in
  (* Printf.printf " ok, max %d\n" (max_instr_list code); *)
  { name = Imp.(fdef.name);
    code = code;
    params = Imp.(fdef.params);
    locals = Imp.(fdef.locals); }

let from_imp_program p =
  { globals = Imp.(p.globals);
    functions = List.map from_imp_fdef Imp.(p.functions) }

let from_nimp_code (code:sequence) :Imp.sequence= 
  let rec from_nimp_instr (i:instruction) :Imp.instruction= match i.instr with 
    | Putint e  -> Imp.Putint(e)
    | Putchar e -> Imp.Putchar(e)
    | Set(x, e) -> Imp.Set(x, e)
    | Expr e    -> Imp.Expr(e)
    | Return e  -> Imp.Return(e)
    | While(e, is1) -> Imp.While(e, from_nimp_list is1)
    | If(e, is1, is2) -> Imp.If(e, from_nimp_list is1, from_nimp_list is2)
    | Write(e1, e2) -> Imp.Write(e1, e2)
  and from_nimp_list (li:sequence):Imp.sequence = match li with
    | []   -> []
    | i::l -> (from_nimp_instr i):: from_nimp_list l
  in
  from_nimp_list code