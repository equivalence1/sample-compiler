type i =
| S_READ
| S_WRITE
| S_PUSH  of int
| S_LD    of string
| S_ST    of string
| S_BINOP of string
| S_JMP   of string
| S_JMPC  of string * string (* jmpc compares with zero *)
| S_LABLE of string
| S_CALL  of string
| S_RET
| S_DROP (* drop 1 element from top of stack *)
| S_PARAM of string

module Interpreter =
  struct

    open Utils.Operation
    open Utils

    let rec jump_to_lable l code =
        match code with
        | [] -> failwith (Printf.sprintf "No lable '%s' was found, cant perform jump." l)
        | (S_LABLE l1)::code' ->
            if l1 = l
            then code'
            else jump_to_lable l code'
        | i::code' -> jump_to_lable l code'

    let rec go_to_insruction_no num code =
        if num = 0
        then code
        else go_to_insruction_no (num - 1) (List.tl code)

    let run code =
      let rec run' env_stack stack code full_code =
        let env::env_stack' = env_stack in
    	match code with
    	| []       -> ()
    	| i::code' ->
          match i with
          | S_READ ->
            Printf.printf "> ";
            let x = read_int() in
            run' env_stack (x::stack) code' full_code

          | S_WRITE ->
            let y::stack' = stack in
            Printf.printf "%d\n" y;
            run' env_stack stack' code' full_code

          | S_PUSH n ->
            run' env_stack (n::stack) code' full_code

          | S_LD x ->
            let value = StackMachineEnv.get_var x env in
            run' env_stack (value::stack) code' full_code
          
          | S_ST x ->
            let y::stack' = stack in
            let env' = StackMachineEnv.set_var x y env in
            run' (env'::env_stack') stack' code' full_code

            (* same as S_ST here, but will be different in x86
             *
             * we cant use S_ST here, because it is not right in x86.
             * So we need new instruction, which we will treat as S_ST
             * here, but will just skip in x86.*)
          | S_PARAM x ->
            let y::stack' = stack in
            let env' = StackMachineEnv.set_var x y env in
            run' (env'::env_stack') stack' code' full_code

          | S_BINOP op ->
            let r::l::stack' = stack in
            run' env_stack ((perform_op op l r)::stack') code' full_code

          | S_LABLE l -> 
            run' env_stack stack code' full_code

          | S_JMP l ->
            run' env_stack stack (jump_to_lable l full_code) full_code

          | S_JMPC (cmp, l) ->
            (let y::stack' = stack in
            match (perform_op (name_to_cmp cmp) y 0) with
            | 0 -> run' env_stack stack' code' full_code
            | _ -> run' env_stack stack' (jump_to_lable l full_code) full_code)

          | S_CALL name ->
            let (addr', state') = StackMachineEnv.init_env in
            let next_instr_no = (List.length full_code) - (List.length code') in
            let fun_env = (next_instr_no, state') in
            run' (fun_env::env_stack) stack (jump_to_lable name full_code) full_code

          | S_RET ->
            let addr = StackMachineEnv.get_ret env in
            run' env_stack' stack (go_to_insruction_no addr full_code) full_code

          | S_DROP ->
            let y::stack' = stack in
            run' env_stack stack' code' full_code

          | _ -> failwith "run: no matching"

    in
    run' [StackMachineEnv.init_env] [] (jump_to_lable "main" code) code

    let rec debug_print code =
        match code with
        | [] -> Printf.eprintf "\n"
        | i::code' ->
          match i with
          | S_READ                -> Printf.eprintf "S_READ\n";                      debug_print code'
          | S_WRITE               -> Printf.eprintf "S_WRITE\n";                     debug_print code'
          | S_PUSH n              -> Printf.eprintf "S_PUSH %d\n" n;                 debug_print code'
          | S_LD x                -> Printf.eprintf "S_LD %s\n" x;                   debug_print code'
          | S_ST x                -> Printf.eprintf "S_ST %s\n" x;                   debug_print code'
          | S_BINOP op            -> Printf.eprintf "S_BINOP %s\n" op;               debug_print code'
          | S_LABLE l             -> Printf.eprintf "S_LABLE %s\n" l;                debug_print code'
          | S_JMP l               -> Printf.eprintf "S_JMP %s\n" l;                  debug_print code'
          | S_JMPC (cmp, l)       -> Printf.eprintf "S_JMPC (%s, %s)\n" cmp l;       debug_print code'
          | S_CALL (name)         -> Printf.eprintf "S_CALL %s\n" name;              debug_print code'
          | S_RET                 -> Printf.eprintf "S_RET\n";                       debug_print code'
          | S_DROP                -> Printf.eprintf "S_DROP\n";                      debug_print code'
          | S_PARAM x             -> Printf.eprintf "S_PARAM %s\n" x;                debug_print code'

  end

class labler =
  object(self)
    val    if_lable      = ref 0
    method inc_if_lable  = if_lable := !if_lable + 1
    method next_if_lable  = self#inc_if_lable; string_of_int (!if_lable)

    val    while_lable      = ref 0
    method inc_while_lable  = while_lable := !while_lable + 1
    method next_while_lable  = self#inc_while_lable; string_of_int (!while_lable)
  end

(* I separate `if` and `while` lables only to make .s files clearer for debugging *)

module Compile =
    struct

        open Language.Expr
        open Language.Stmt

        let rec compile_expr = function
        | Var   x -> [S_LD   x]
        | Const n -> [S_PUSH n]
        | BinOp (op, l, r) ->
            let l' = compile_expr l in
            let r' = compile_expr r in
                l' @ r' @ [S_BINOP op]
        | Call (name, args) ->
            let args' = List.rev args in
            let compile_arg code arg = code @ (compile_expr arg) in
            (List.fold_left compile_arg [] args') @ [S_CALL name] (* f (a, b) -> [a, b] *)
        | _ -> failwith "compile_expr: no matching"             (*                 ^ stack top *)

        let rec stmt' labler = function
        | Skip          -> []
        | Assign (x, e) -> compile_expr e @ [S_ST x]
        | Read    x     -> [S_READ; S_ST x]
        | Write   e     -> compile_expr e @ [S_WRITE]
        | Seq    (l, r) -> stmt' labler l @ stmt' labler r
        | If (e, s1, s2) -> 
            let cur_if_number = labler#next_if_lable in
            let l1            = "else_" ^ cur_if_number in
            let l2            = "fi_"          ^ cur_if_number in 
                compile_expr e @ [S_JMPC ("e", l1)] @ stmt' labler s1 @ [S_JMP l2; S_LABLE l1] @ stmt' labler s2 @ [S_LABLE l2]
        | While (e, s1) -> 
            let cur_while_number = labler#next_while_lable in
            let l1               = "start_while_" ^ cur_while_number in
            let l2               = "end_while_"   ^ cur_while_number in 
            [S_LABLE l1] @ compile_expr e @ [S_JMPC ("e", l2)] @ stmt' labler s1 @ [S_JMP l1; S_LABLE l2]
        | Call (name, args) ->
            (compile_expr (Call (name, args))) @ [S_DROP]
        | Return e ->
            (compile_expr e) @ [S_RET]
        
        let stmt = stmt' (new labler)

    end

module Prog = 
    struct

        open Language.Unit
        open Language.Stmt
        open Utils

        let compile prog =
            match prog with
            | (funcs, main) ->
                let compile_func code (name, (params, stmt)) = 
                    let show_params = List.map (fun x -> S_PARAM x) params in
                    code @ [S_LABLE name] @ show_params @ (Compile.stmt stmt)
                in
                (List.fold_left compile_func [] funcs) @ [S_LABLE "main"] @ (Compile.stmt main)

    end
