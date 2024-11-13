(**
   Translation from IMP to MIPS.

   Result of an expression stored in $t0. Every intermediate value on the
   stack, every function argument and every local variable also on the stack.
 *)

open Imp
open Mips

exception Error of string

let tmp_regs = [| t0; t1; t2; t3; t4; t5; t6; t7; t8; t9 |]
let nb_tmp_regs = Array.length tmp_regs

let var_regs = [| s0; s1; s2; s3; s4; s5; s6; s7 |]
let nb_var_regs = Array.length var_regs

let push reg = subi sp sp 4 @@ sw reg 0(sp)
let pop  reg = lw reg 0(sp) @@ addi sp sp 4

let rec    save regs k = if k < 0 then nop else save regs (k-1) @@ push regs.(k)
let rec restore regs k = if k < 0 then nop else    pop regs.(k) @@ restore regs (k-1)

let    save_tmp = save    tmp_regs
let restore_tmp = restore tmp_regs

let save_var = save var_regs
let restore_var = restore var_regs

let print_instr = function 
  | Putint(_) -> Printf.printf "putint\n"
  | Putchar(e) -> Printf.printf "puthchar\n"
  | Expr e ->  Printf.printf "expr\n"
  | Set(x,e) -> Printf.printf "set\n"
  | If(_) ->  Printf.printf "if\n"
  | While(_) ->  Printf.printf "While\n"
  | Return(_) -> Printf.printf "return\n"

(* explicit allocation information for a local variable *)
type explicit_alloc =
  | Reg   of string  (* name of the register *)
  | Stack of int     (* offset on the stack, relative to fp *)

(* create an explicit allocation for all local variables and function 
   parameters of a function [fdef] *)
let allocate_locals fdef =
  let nfdef = Nimp.from_imp_fdef fdef in
  let alloc = Hashtbl.create 16 in
  let raw_alloc, r_max, spill_count = Linearscan.lscan_alloc nb_var_regs nfdef in
  let add_in_alloc id raw = match raw with
    | Linearscan.RegN n -> Hashtbl.add alloc id (Reg var_regs.(n))
    | Linearscan.Spill n ->  Hashtbl.add alloc id (Stack (-4*(n+2)))
  in
  Linearscan.print_list_raw raw_alloc;
  Hashtbl.iter add_in_alloc raw_alloc; (*add locals in alloc*)
  List.iteri (fun k id -> Hashtbl.add alloc id (Stack(4*(k+1)))) fdef.params; (*add params in alloc*)
  alloc, r_max, spill_count

(* Generate Mips code for an Imp function *)
(* Call frame

     | sp                          fp             |
   --+-------------+--------+----+----+-----------+------
     | temp values | locals | ra | fp | arguments |
   --+-------------+--------+----+----+-----------+------
     |               callee frame                 |   caller frame

   The caller is reponsible for pushing/popping the arguments, the callee
   is reponsible for everything else.
   for the moment locals variables and arguments are in the frame, 
   globale variables are in static data
   *)
let tr_function fdef =
  (* Allocation info for local variables and function parameters *)
  let alloc, r_max, spill_count = allocate_locals fdef in

  (* load the variable id in ti*)
  let load_variable (ti:register) (id:explicit_alloc) = 
    match id with
    | Reg r -> move ti r
    | Stack offset -> lw ti offset(fp)
  in
  (* store ti in the variable id*)
  let store_variable (ti:register) (id:explicit_alloc) = 
    match id with
    | Reg r -> move r ti 
    | Stack offset -> sw ti offset(fp)
  in
  (* Generate Mips code for an Imp expression. The generated code produces the
     result in register $ti, and do not alter registers $tj with j < i. *)
  let rec tr_expr i e =
    let ti = 
      if i < nb_tmp_regs then tmp_regs.(i) 
      else raise (Error "not enough temporary registers")
    in 
    match e with
    | Cst(n)  -> li ti n
    | Bool(b) -> if b then li ti 1 else li ti 0
    | Var(x) -> 
      (match Hashtbl.find_opt alloc x with
       | Some id -> load_variable ti id 
       | None -> la ti x @@ lw ti 0(ti)) (* non-local assumed to be a valid global *)
    | Binop(bop, e1, e2) ->
       let op = match bop with
         | Add -> add
         | Mul -> mul
         | Lt  -> slt
       in
       if i+1 >= nb_tmp_regs then raise (Error "not enough temporary registers");
       tr_expr i e1 @@ tr_expr (i+1) e2 @@ op ti ti tmp_regs.(i+1)

    (* Function call.
       Evaluate the arguments and push their values onto the stack.
       Save all temporary registers with number < i
       Jump to the function.
       Finallly restore saved tempary registers and clean the stack. *)
    | Call(f, params) ->
       tr_params i params @@ save_tmp (i-1) 
       @@ jal f 
       @@ restore_tmp (i-1) @@ addi sp sp (4 * List.length params)

  and tr_params i = function
    | []        -> nop
    | e::params -> tr_params i params @@ tr_expr i e @@ push tmp_regs.(i)

  in

  (* Generate new labels for jumps *)
  let new_label =
    let cpt = ref (-1) in
    fun () -> incr cpt; Printf.sprintf "__%s_%i" fdef.name !cpt
  in

  (* Generate MIPS code for sequence of Imp instruction. *)
  let rec tr_seq = function
    | []   -> nop
    | i::s -> tr_instr i @@ tr_seq s
  (* MIPS instructions to put at the end of the function*)
  and tr_cleaning = fun () -> 
    restore_var r_max 
    @@ addi sp sp (4 * spill_count) 
    @@ addi sp fp (-4) @@ pop ra @@ pop fp 
  (* generate MIPS codefor an Imp instruction *)
  and tr_instr i = match i with
    | Putint(e) -> tr_expr 0 e @@ move a0 t0 @@ li v0 1 @@ syscall
    | Putchar(e) -> tr_expr 0 e @@ move a0 t0 @@ li v0 11 @@ syscall
    | Set(x, e) ->
       let set_code = match Hashtbl.find_opt alloc x with
         | Some id -> store_variable t0 id
         | None -> la t1 x @@ sw t0 0(t1)
       in
       tr_expr 0 e @@ set_code

    | If(c, s1, s2) ->
       let then_label = new_label()
       and end_label = new_label()
       in
       tr_expr 0 c @@ bnez t0 then_label
       (* fall to else branch *) @@ tr_seq s2 @@ b end_label
       @@ label then_label @@ tr_seq s1 (* fall through *)
       @@ label end_label

    | While(c, s) ->
       let test_label = new_label()
       and code_label = new_label()
       in
       b test_label
       @@ label code_label @@ tr_seq s
       @@ label test_label @@ tr_expr 0 c @@ bnez t0 code_label
       (* fall through *)

    (* Return from a call with a value. Includes cleaning the stack. *)
    | Return(e) -> 
      tr_expr 0 e @@ tr_cleaning () @@ jr ra
    | Expr(e) -> tr_expr 0 e

  in

  (* Mips code for the function itself. 
     Initialize the stack frame and save callee-saved registers, run the code of 
     the function, then restore callee-saved, clean the stack and returns with a 
     dummy value if no explicit return met. *)
  push fp @@ push ra @@ addi fp sp 4
  @@ addi sp sp (-4 * spill_count)
  @@ save_var r_max 
  @@ tr_seq fdef.code
  @@ tr_cleaning ()
  @@ li t0 0 
  @@ jr ra

(* Generate Mips code for an Imp program. *)
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

    @@ comment "built-in print_int"
    @@ label "print_int"
    @@ lw a0 4 sp
    @@ li v0 1
    @@ syscall
    @@ sw a0 0 sp
    @@ subi sp sp 4
    @@ jr ra
  
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

