open Imp
open Ops
open Mips

let tmp_regs = [| t0; t1; t2; t3; t4; t5; t6; t7; t8; t9 |]
let nb_tmp_regs = Array.length tmp_regs

let var_regs = [| s0; s1; s2; s3; s4; s5; s6; s7 |]
let nb_var_regs = Array.length var_regs

(*TODO pourquoi ce n'est pas le même push et pop?*)
let push reg = sw reg 0 sp  @@ subi sp sp 4
let pop  reg = addi sp sp 4 @@ lw reg 0 sp

let rec    save regs k = if k < 0 then nop else save regs (k-1) @@ push regs.(k)
let rec restore regs k = if k < 0 then nop else    pop regs.(k) @@ restore regs (k-1)

let    save_tmp = save    tmp_regs
let restore_tmp = restore tmp_regs

let save_var = save var_regs
let restore_var = restore var_regs



let new_label =
  let cpt = ref (-1) in
  fun () -> incr cpt; Printf.sprintf "__label_%i" !cpt
        
type allocation_info =
  | Reg   of string  (* name of the register *)
  | Stack of int     (* offset on the stack, relative to fp *)

module STbl = Map.Make(String)
      
type allocation_context = {
  alloc: allocation_info STbl.t;
  r_max: int;
  spill_count:int;
}

let empty_allocation_context = { alloc = STbl.empty;
                                 r_max = 0;
                                 spill_count = 0;}
    
let mk_allocation_context nfdef =
  let allocation = ref STbl.empty in
  let raw_alloc, r_max, spill_count, liveness_map = Linearscan.lscan_alloc nb_var_regs nfdef in
  let add_in_alloc id raw = match raw with
    | Linearscan.RegN n -> allocation := STbl.add id (Reg var_regs.(n)) !allocation
    | Linearscan.Spill n ->  allocation := STbl.add id (Stack (-4*(n+2)))  !allocation
  in
  Linearscan.print_list_raw raw_alloc; (* print raw alloc*)
  Hashtbl.iter add_in_alloc raw_alloc; (*add locals in alloc*)
  List.iteri (fun k id -> allocation := STbl.add id (Stack(4*(k+1))) !allocation) nfdef.params; (*add params in alloc*)
  {alloc=(!allocation); r_max; spill_count }, liveness_map

(* Intègre une optimisation : le résultat est placé par défaut dans $t0
   plutôt que systématiquement sur la pile *)
let rec tr_expr e ctx = match e with
  | Int(n) ->
    li t0 n
        
  | Bool(b) ->
    if b then li t0 (-1) else li t0 0

  | Var(id) -> begin
    try
      match STbl.find id ctx.alloc with
        | Reg r -> move t0 r
        | Stack offset -> lw t0 offset(fp)
    with
      | Not_found -> la t0 id @@ lw t0 0 t0
  end
          
  | Unop(uop, e) ->
    let op = match uop with
      | Minus -> neg
      | Not -> not_
      | Fst -> failwith "fst not implemented in imp2mips"
      | Snd -> failwith "snd not implemented in imp2mips"
    in
    tr_expr e ctx @@ op t0 t0
        
  | Binop(bop, e1, e2) ->
    let op = match bop with
      | Add  -> add
      | Sub  -> sub
      | Mul  -> mul
      | Div  -> div
      | Rem  -> rem
      | Lsl  -> sll
      | Lsr  -> srl
      | Eq   -> seq
      | Neq  -> sne
      | Lt   -> slt
      | Le   -> sle
      | Gt   -> sgt
      | Ge   -> sge
      | And  -> and_
      | Or   -> or_
      | Pair -> failwith "pair not implemented in imp2mips"
      in
    tr_expr e2 ctx @@ push t0 @@ tr_expr e1 ctx @@ pop t1 @@ op t0 t0 t1      

  | Call(id, params) ->
    let params_code =
      List.fold_right
        (fun e code -> code @@ tr_expr e ctx @@ push t0)
        params nop
    in
    params_code @@ jal id
    @@ addi sp sp (4 * List.length params)

  | Deref(e) ->
    tr_expr e ctx @@ lw t0 0 t0

  | Addr(id) ->
    la t0 id

  | PCall(f, params) ->
    let params_code =
      List.fold_right (fun e code -> code @@ tr_expr e ctx @@ push t0) params nop
    in
    params_code @@ tr_expr f ctx @@ jalr t0 
    @@ addi sp sp (4 * List.length params)

  | Sbrk(e) ->
    tr_expr e ctx @@ move a0 t0 @@ li v0 9 @@ syscall @@ move t0 v0
      
(* MIPS instructions to put at the end of the function*)
let tr_cleaning ctx =  
  restore_var ctx.r_max 
  @@ move sp fp    (* Désallocation de la pile *)
  @@ lw ra (-4) fp (* Récupération de l'adresse de retour *)
  @@ lw fp 0 fp    (* Restauration du pointeur de base de l'appelant *)

let rec tr_instr (i:Imp.instruction) ctx = match i with
  | Putint(e) -> tr_expr e ctx @@ move a0 t0 @@ li v0 1 @@ syscall
  | Putchar(e) ->
    tr_expr e ctx @@ move a0 t0 @@ li v0 11 @@ syscall
        
  | Set(id, e) ->
    let set_code =
      try
        match STbl.find id ctx.alloc with
          | Reg r -> move r t0 
          | Stack offset -> sw t0 offset(fp)
      with
        | Not_found -> la t1 id @@ sw t0 0 t1
    in
    tr_expr e ctx @@ set_code
        
  | If(c, s1, s2) ->
    let then_label = new_label()
    and end_label = new_label()
    in
    tr_expr c ctx
    @@ bnez t0 then_label
    @@ tr_seq s2 ctx
    @@ b end_label
    @@ label then_label
    @@ tr_seq s1 ctx
    @@ label end_label
        
  | While(c, s) ->
    let test_label = new_label()
    and code_label = new_label()
    in
    b test_label
    @@ label code_label
    @@ tr_seq s ctx
    @@ label test_label
    @@ tr_expr c ctx
    @@ bnez t0 code_label
        
  | Return(e) ->
    tr_expr e ctx
    @@ tr_cleaning ctx
    @@ jr ra

  | Write(d, e) ->
    tr_expr e ctx
    @@ push t0
    @@ tr_expr d ctx
    @@ pop t1
    @@ sw t1 0 t0

  | Expr(e) ->
    tr_expr e ctx
      
      

and tr_seq s ctx = match s with
    | []   -> nop
    | [i]  -> tr_instr i ctx
    | i::s -> tr_instr i ctx @@ tr_seq s ctx


let filter_code ncode liveness_map :Nimp.sequence = 
  let filter_instr i = match i with
    | Nimp.({nb; instr=Set(x,e)}) -> 
      begin
      match Liveness.VMap.find_opt x liveness_map with
        | Some(_, high) -> 
          if high <= i.nb then 
            Nimp.({nb; instr=Expr(e)}) 
          else 
            Nimp.({nb; instr=Set(x,e)})
        | None -> Nimp.({nb; instr=Expr(e)})
      end
    | _  -> i
  in 
  List.map filter_instr ncode

let tr_function fdef =
  let nfdef = Nimp.from_imp_fdef fdef in
  let context, liveness_map = mk_allocation_context nfdef in
  let filtered_code = Nimp.from_nimp_code (filter_code nfdef.code liveness_map) in
  push fp
  @@ push ra
  @@ addi fp sp 8
  @@ addi sp sp (-4 * context.spill_count)
  @@ save_var context.r_max
  @@ tr_seq filtered_code context
  (* @@ ici, erreur, on n'a pas croisé de return *)
  (* Pour éviter trop de corruption, on renvoie 0 *)
  @@ tr_cleaning context
  @@ li t0 0
  @@ jr ra

    
let translate_program prog =
  let init =
    beqz a0 "init_end"
    @@ lw a0 0 a1
    @@ jal "atoi"
    @@ la t0 "arg"
    @@ sw v0 0 t0
    @@ label "init_end"
  and close =
    li v0 10
    @@ syscall
  and built_ins =
    comment "built-in atoi"
    @@ label "atoi"
    @@ move t0 a0
    @@ li   t1 0
    @@ li   t2 10
    @@ label "atoi_loop"
    @@ lbu  t3 0 t0
    @@ beq  t3 zero "atoi_end"
    @@ li   t4 48
    @@ blt  t3 t4 "atoi_error"
    @@ li   t4 57
    @@ bgt  t3 t4 "atoi_error"
    @@ addi t3 t3 (-48)
    @@ mul  t1 t1 t2
    @@ add  t1 t1 t3
    @@ addi t0 t0 1
    @@ b "atoi_loop"
    @@ label "atoi_error"
    @@ li   v0 10
    @@ syscall
    @@ label "atoi_end"
    @@ move v0 t1
    @@ jr   ra
  
    @@ comment "built-in power"
    @@ label "power"
    @@ lw s0 8 sp
    @@ lw s1 4 sp
    @@ li t0 1
    @@ b "power_loop_guard"
    @@ label "power_loop_code"
    @@ mul t0 t0 s1
    @@ subi s0 s0 1
    @@ label "power_loop_guard"
    @@ bgtz s0 "power_loop_code"
    @@ sw t0 0 sp
    @@ subi sp sp 4
    @@ jr ra

  in

  let main_code = tr_seq prog.main empty_allocation_context in
  let function_codes = List.fold_right
    (fun fdef code ->
      label fdef.name @@ tr_function fdef @@ code)
    prog.functions nop
  in
  let text = init @@ main_code @@ close @@ function_codes @@ built_ins
  and data = List.fold_right
    (fun id code -> label id @@ dword [0] @@ code)
    prog.globals (label "arg" @@ dword [0])
  in
  print_string "imp2mips done\n";
  { text; data }
