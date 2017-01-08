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

| S_REF   of string * string
| S_MCALL of (string * string)
| S_NEW   of string
| S_FIELD of (string * string)
| S_FASSIGN of string * string

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
          | S_REF (tp, x)         -> Printf.eprintf "S_REF (%s, %s)\n" tp x;         debug_print code'
          | S_MCALL (t, name)     -> Printf.eprintf "S_MCALL (%s, %s)\n" t name;     debug_print code'
          | S_NEW name            -> Printf.eprintf "S_NEW %s\n" name;               debug_print code'
          | S_FIELD (tp, name)    -> Printf.eprintf "S_FIELD (%s, %s)\n" tp name;    debug_print code'
          | S_FASSIGN (obj, f)    -> Printf.eprintf "S_FASSIGN (%s, %s)\n" obj f;    debug_print code'

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
        open Utils


        let find_method_type' meth methods =
            let fil_meths = List.filter (fun (name, _, _, _) -> name = meth) methods in
            match fil_meths with
            | [] -> None
            | (_, t, _, _)::rest -> Some t

        let rec find_method_type meth cls_name env =
            let (_, parent, _, methods) = SMCompileEnv.get_class cls_name env in
            let mt = find_method_type' meth methods in
            match mt with
            | None ->
                (match parent with
                 | None -> failwith (Printf.sprintf "method %s not found" meth)
                 | Some p -> find_method_type meth p env)
            | Some t -> t


        let find_field_type' field fields =
            let fil_fields = List.filter (fun (tp, name) -> name = field) fields in
            match fil_fields with
            | [] -> None
            | (tp, name)::rest -> Some tp

        let rec find_field_type field cls_name env = 
            let (_, parent, fields, _) = SMCompileEnv.get_class cls_name env in
            let ft = find_field_type' field fields in
            match ft with
            | None ->
                (match parent with
                 | None -> failwith (Printf.sprintf "field %s not found" field)
                 | Some p -> find_field_type field p env)
            | Some t -> t



        let rec compile_expr env = 
            let compile_arg env code arg = code @ (snd @@ compile_expr env arg) in
        function
        | Var   x -> 
            (match SMCompileEnv.get_var x env with
            | Some s -> (s, [S_LD   x])
            | None -> failwith (Printf.sprintf "variable %s is not yet defined" x))
        | Const n -> ("int", [S_PUSH n])
        | BinOp (op, l, r) ->
            let (l_type, l_code) = compile_expr env l in
            let (r_type, r_code) = compile_expr env r in
                (match (l_type, r_type) with
                | ("int", "int") -> ("int", l_code @ r_code @ [S_BINOP op])
                | _ -> failwith "Can't perform binop on non-ints")
        | Call (name, args) ->
            let (fun_name, fun_type, fun_params, _) = SMCompileEnv.get_func name env in
            let args' = List.rev args in
            (fun_type, (List.fold_left (compile_arg env) [] args') @ [S_CALL name])
        | MCall (obj, meth, args) ->
            let (tp, code') = compile_expr env obj in
            let m_type = find_method_type meth tp env in
            let args' = List.rev args in
            (m_type, (List.fold_left (compile_arg env) [] args')
                     @ code'
                     @ [S_MCALL (tp, meth)])
        | Field (obj, f) ->
            let (tp, code') = compile_expr env obj in
            let f_type = find_field_type f tp env in
            (f_type, code' @ [S_FIELD (tp, f)])
        | New (cls_name, args) ->
            let args' = List.rev args in
            (cls_name, (List.fold_left (compile_arg env) [] args') @ [S_NEW cls_name])
        | _ -> failwith "compile_expr: no matching"             


        let rec stmt' labler env = function
        | Skip          -> (env, [])
        | Assign (x, e) -> 
            (match SMCompileEnv.get_var x env with
                | Some s -> (env, (snd @@ compile_expr env e) @ [S_ST x])
            | None -> failwith (Printf.sprintf "variable %s is not yet defined" x))
        | Read    x     ->
            (match SMCompileEnv.get_var x env with
            | Some s ->
                if (s = "int")
                then (env, [S_READ; S_ST x])
                else failwith (Printf.sprintf "variable %s should be int" x)
            | None -> failwith (Printf.sprintf "variable %s is not yet defined" x))
        | Write   e     -> (env, (snd @@ compile_expr env e) @ [S_WRITE])
        | Seq    (l, r) -> 
            let (env' , code')  = stmt' labler env  l in
            let (env'', code'') = stmt' labler env' r in
            (env'', code' @ code'')
        | If (e, s1, s2) -> 
            let cur_if_number = labler#next_if_lable in
            let l1            = "else_" ^ cur_if_number in
            let l2            = "fi_"   ^ cur_if_number in 
            (env,   snd (compile_expr env e)
                  @ [S_JMPC ("e", l1)]
                  @ snd (stmt' labler env s1)
                  @ [S_JMP l2; S_LABLE l1]
                  @ snd (stmt' labler env s2)
                  @ [S_LABLE l2])
        | While (e, s1) -> 
            let cur_while_number = labler#next_while_lable in
            let l1               = "start_while_" ^ cur_while_number in
            let l2               = "end_while_"   ^ cur_while_number in 
            (env,   [S_LABLE l1]
                  @ snd (compile_expr env e)
                  @ [S_JMPC ("e", l2)]
                  @ snd (stmt' labler env s1)
                  @ [S_JMP l1; S_LABLE l2])
        | Call (name, args) ->
            (env, snd (compile_expr env (Call (name, args))) @ [S_DROP])
        | Return e ->
            (env, snd (compile_expr env e) @ [S_RET])
        | Ref (tp, x) ->
            (SMCompileEnv.add_var x tp env, [S_REF (tp, x)])
        | MCall (a, b, c) ->
            (env, snd (compile_expr env (MCall (a, b, c))) @ [S_DROP])
        | FieldAssign (obj, f, e) -> 
            (match SMCompileEnv.get_var obj env with
            | Some s -> (env, (snd @@ compile_expr env e) @ [S_FASSIGN (obj, f)])
            | None -> failwith (Printf.sprintf "variable %s is not yet defined" obj))


        let stmt env = fun a -> snd @@ stmt' (new labler) env a

    end

module Prog = 
    struct

        open Language.Unit
        open Language.Stmt
        open Utils


        let compile_func env code (name, tp, params, stmt) = 
            let show_params = List.map (fun (tp, x) -> S_PARAM x) params in
            let env' = List.fold_left (fun env -> fun (tp, x) -> SMCompileEnv.add_var x tp env) env params in
            code @ [S_LABLE name] @ show_params @ (Compile.stmt env' stmt)

        let compile_cls env code (cls_name, parent, fields, methods) =
            List.fold_left (fun code -> fun (name, tp, params, stmt) -> compile_func env code (cls_name ^ "_" ^ name, tp, (cls_name, "self")::params, stmt)) code methods

        let compile' prog env =
            let (classes, funcs, main) = prog in
              (List.fold_left (compile_cls env) [] classes)
            @ (List.fold_left (compile_func env) [] funcs)
            @ [S_LABLE "main"]
            @ (Compile.stmt env main)

        let compile prog =
            let (classes, funcs, main) = prog in
            let env = SMCompileEnv.init in
            let env' = List.fold_left (fun env -> fun (cls_name, parent, fields, methods) as cls -> SMCompileEnv.add_class cls_name cls env) env classes in
            let env'' = List.fold_left (fun env -> fun (f_name, tp, params, stmt) as f -> SMCompileEnv.add_func f_name f env) env' funcs in
            compile' prog env''

    end
