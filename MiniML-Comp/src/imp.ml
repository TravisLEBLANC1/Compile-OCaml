(**
   Extended abstract syntax for the IMP language.
   Added: pointers on data and on functions; more unary/binary operators.
 *)

type expression =
  | Int   of int
  | Bool  of bool
  | Var   of string
  | Unop  of Ops.unop * expression
  | Binop of Ops.binop * expression * expression
  | Call  of string * expression list
  (* pointers *)
  | Deref of expression (* read a memory address *)
  | Addr  of string     (* & -> gives the address of a variable,
                                used here to get function pointers *)
  | PCall of expression * expression list (* function call, by pointer *)
  | Sbrk  of expression (* primitive for heap extension *)
      
type instruction =
  | Putint  of expression (*added putint*)
  | Putchar of expression
  | Set     of string * expression
  | If      of expression * sequence * sequence
  | While   of expression * sequence
  | Return  of expression
  | Expr    of expression
  (* pointers *)
  | Write   of expression * expression (* write a memory address *)
      
and sequence = instruction list

(* The abstract syntax contains no constructors for explicit handling arrays,
   but the following four functions provide macros that simulate arrays using
   simple pointer arithmetic *)
let array_access (a: expression) (i: expression): expression =
  (* compute the pointer to index  i  of the array  a  *)
  if i = Int(0) then a else 
  Binop(Ops.Add, a, Binop(Ops.Mul, i, Int 4))
let array_get (a: expression) (i: expression): expression =
  (* read operation  a[i]  *)
  Deref(array_access a i)
let array_set (a: expression) (i: expression) (e: expression): instruction =
  (* write operation  a[i] = e  *)
  Write(array_access a i, e)
let array_create (n: expression): expression =
  (* allocation of an array of size  n  *)
  Call("malloc", [Binop(Mul, n, Int 4)])
    
type function_def = {
  name: string;
  code: sequence;
  params: string list;
  locals: string list;
}
    
type program = {
  (* minor variation with respect to IMP:
     we introducea main sequence of instructions instead of requiring
     a function called  main  *)
  main: sequence;
  functions: function_def list;
  globals: string list;
}

(* Merge several programs (cheap way of including libraries) *)
let merge lib prog = {
  main = lib.main @ prog.main;
  functions = lib.functions @ prog.functions;
  globals = lib.globals @ prog.globals;
}

(* (**
   An interpreter for IMP.
 *)

 type value =
 | VInt of int
 | VBool of bool
 | Undef

exception EReturn of value

let exec_prog prog arg =
 let global_env = Hashtbl.create 16 in
 List.iter (fun id -> Hashtbl.add global_env id Undef) prog.globals;

 let rec exec_call f args =
   let fdef = List.find (fun fdef -> fdef.name = f) prog.functions in
   let local_env = Hashtbl.create 16 in
   List.iter2 (fun id arg -> Hashtbl.add local_env id arg) fdef.params args;
   List.iter (fun id -> Hashtbl.add local_env id Undef) fdef.locals;

   let rec eval_int e = match eval_expr e with
     | VInt n -> n
     | _ -> failwith "not an int"
   and eval_bool e = match eval_expr e with
     | VBool b -> b
     | _ -> failwith "not a boolean"
   and eval_expr = function
     | Int n -> VInt n
     | Bool b -> VBool b
     | Var id -> begin
         match Hashtbl.find_opt local_env id with
         | Some v -> v
         | None -> Hashtbl.find global_env id
       end
     | Binop(op, e1, e2) ->
        let n1 = eval_int e1 in
        let n2 = eval_int e2 in
        begin match op with
          | Add -> VInt (n1 + n2)
          | Sub -> VInt (n1 - n2)
          | Mul -> VInt (n1 * n2)
          | Div -> VInt (n1 / n2)
          | Rem -> VInt (n1 % n2)
          | Lsl -> VInt (Int.shift_left n1 n2)
          | Lsr -> VInt (Int.shift_right_logical n1 n2)
          | Lt  -> VBool (n1 < n2)
          | Le  -> VBool (n1 <= n2)
          | Gt  -> VBool (n1 > n2)
          | Ge  -> VBool (n1 >= n2)
          | Eq  -> VBool (n1 = n2)
          | Neq -> VBool (n1 <> n2)
          | And -> VBool (n1 && n2)
          | Or  -> VBool (n1 || n2)
          | Pair -> "pair should not happen"
        end
     | Call(f, args) ->
        exec_call f (List.map eval_expr args)
   in

   let rec exec_seq s =
     List.iter exec_instr s

   and exec_instr = function
     | Putint e -> print_int (eval_int e)
     | Putchar e -> print_char (char_of_int (eval_int e))
     | Set(id, e) ->
        let v = eval_expr e in
        if Hashtbl.mem local_env id then
          Hashtbl.replace local_env id v
        else
          Hashtbl.replace global_env id v
     | If(e, s1, s2) ->
        if eval_bool e then
          exec_seq s1
        else
          exec_seq s2
     | While(e, s) as i ->
        if eval_bool e then
          (exec_seq s; exec_instr i)
     | Return e -> raise (EReturn(eval_expr e))
     | Expr e -> ignore (eval_expr e)
     | Write(e1, e2) -> 

   in

   try
     exec_seq fdef.code; Undef
   with
     EReturn v -> v
   
 in

 exec_call "main" [arg] *)
