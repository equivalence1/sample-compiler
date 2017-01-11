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
| S_CALL  of (int * string)
| S_RET
| S_DROP (* drop 1 element from top of stack *)

(* meta info *)
        (*     name        parent              fields                             methods    *)
| S_CLASS of (string * string option * (string * string) list * (string * string * ((string * string) list)) list)
        (*  name          params                   locals  *)
| S_FUN of string * (string * string) list * (string * string) list
| S_FUN_END

| S_REF   of string * string
| S_MCALL of int * string * string
| S_NEW   of string
| S_INIT_VTABLE of string
| S_FIELD of string * string
| S_FASSIGN of string * string

module Interpreter =
  struct

    open Utils.Operation
    open Utils
    open Utils.StackMachineEnv

    let rec jump_to_lable l code =
        match code with
        | [] -> failwith (Printf.sprintf "No lable '%s' was found, cant perform jump." l)
        | (S_LABLE l1)::code' ->
            if l1 = l
            then (S_LABLE l1)::code'
            else jump_to_lable l code'
        | i::code' -> jump_to_lable l code'

    let rec go_to_insruction_no num code =
        if num = 0
        then code
        else go_to_insruction_no (num - 1) (List.tl code)

    let rec collect_classes_meta code meta_env =
        match code with
        | [] -> meta_env
        | i::code' ->
            match i with
            | S_CLASS cls ->
                collect_classes_meta code' (ClassMetaEnv.add_cls cls meta_env)
            | _ ->
                collect_classes_meta code' meta_env

    let bin_op op o_l o_r =
        match (o_l, o_r) with
        | (Int l, Int r) -> perform_op op l r
        | (_, _) ->
            if op = "!="
            then 1
            else 0

    let run code =
      let rec run' env_stack stack code full_code meta_env =
        let env::env_stack' = env_stack in
    	match code with
    	| []       -> ()
    	| i::code' ->
          match i with
          | S_READ ->
            Printf.printf "> ";
            let x = read_int() in
            run' env_stack ((Int x)::stack) code' full_code meta_env

          | S_WRITE ->
            (match stack with
            | [] -> failwith "nothing to print"
            | (Object (_, _))::_ -> failwith "can not print object"
            | (Int y)::stack' ->
                Printf.printf "%d\n" y;
                run' env_stack stack' code' full_code meta_env)

          | S_PUSH n ->
            run' env_stack ((Int n)::stack) code' full_code meta_env

          | S_LD x ->
            let value = StackMachineEnv.get_var x env in
            run' env_stack (value::stack) code' full_code meta_env
          
          | S_ST x ->
            let y::stack' = stack in
            let env' = StackMachineEnv.set_var x y env in
            run' (env'::env_stack') stack' code' full_code meta_env

          | S_FUN (name, params, locals) ->
            let (env', stack') = List.fold_left (fun (env, y::stack') -> fun (t, x) -> (StackMachineEnv.set_var x y env, stack')) (env, stack) params in
            run' (env'::env_stack') stack' code' full_code meta_env

          | S_BINOP op ->
            let o_r::o_l::stack' = stack in
            run' env_stack ((Int (bin_op op o_l o_r))::stack') code' full_code meta_env

          | S_LABLE l ->
            run' env_stack stack code' full_code meta_env

          | S_JMP l ->
            run' env_stack stack (jump_to_lable l full_code) full_code meta_env

          | S_JMPC (cmp, l) ->
            (let (Int y)::stack' = stack in
            match (perform_op (name_to_cmp cmp) y 0) with
            | 0 -> run' env_stack stack' code' full_code meta_env
            | _ -> run' env_stack stack' (jump_to_lable l full_code) full_code meta_env)

          | S_CALL (n, name) ->
            let (_, state') = StackMachineEnv.init_env in
            let next_instr_no = (List.length full_code) - (List.length code') in
            let fun_env = (next_instr_no, state') in
            run' (fun_env::env_stack) stack (jump_to_lable name full_code) full_code meta_env

          | S_RET ->
            let addr = StackMachineEnv.get_ret env in
            run' env_stack' stack (go_to_insruction_no addr full_code) full_code meta_env

          | S_DROP ->
            let y::stack' = stack in
            run' env_stack stack' code' full_code meta_env

          | S_REF _ ->
            run' env_stack stack code' full_code meta_env

          | S_FUN_END ->
            run' env_stack stack code' full_code meta_env

          | S_NEW t ->
            let new_obj = Object (StringMap.empty, StringMap.empty) in
            run' env_stack (new_obj::stack) code' full_code meta_env

          | S_INIT_VTABLE t ->
            let (Object (fields, methods))::stack' = stack in
            let known_classes = ClassMetaEnv.get_all_classes meta_env in
            let (fields, methods) = Meta.fill_in_vtable (fields, methods) t known_classes in
            run' env_stack ((Object (fields, methods))::stack') code' full_code meta_env

          | S_MCALL (n, t, name) ->
            let (Object (fields, methods))::stack' = stack in
            let (_, state') = StackMachineEnv.init_env in
            let next_instr_no = (List.length full_code) - (List.length code') in
            let meth_env = (next_instr_no, state') in
            let full_meth_name = Meta.get_meth_full_name (fields, methods) name in
            run' (meth_env::env_stack) stack (jump_to_lable full_meth_name full_code) full_code meta_env

          | S_FIELD (t, f) ->
            let (Object (fields, methdos))::stack' = stack in
            run' env_stack ((StringMap.find f fields)::stack') code' full_code meta_env

          | S_FASSIGN (t, f) ->
            let (Object (fields, methods))::value::stack' = stack in
            let fields = StringMap.add f value fields in
            run' env_stack ((Object (fields, methods))::stack') code' full_code meta_env

          | _ -> failwith "run: no matching"
    
    in
    run' [StackMachineEnv.init_env] [] (jump_to_lable "main" code) code (collect_classes_meta code ClassMetaEnv.init)

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
          | S_CALL (n, name)         -> Printf.eprintf "S_CALL (%d, %s)\n" n name;              debug_print code'
          | S_RET                 -> Printf.eprintf "S_RET\n";                       debug_print code'
          | S_DROP                -> Printf.eprintf "S_DROP\n";                      debug_print code'
          | S_REF (tp, x)         -> Printf.eprintf "S_REF (%s, %s)\n" tp x;         debug_print code'
          | S_MCALL (n, t, name)  -> Printf.eprintf "S_MCALL (%d, %s, %s)\n" n t name;          debug_print code'
          | S_NEW name            -> Printf.eprintf "S_NEW %s\n" name;               debug_print code'
          | S_INIT_VTABLE name            -> Printf.eprintf "S_INIT_VTABLE %s\n" name;               debug_print code'
          | S_FIELD (tp, name)    -> Printf.eprintf "S_FIELD (%s, %s)\n" tp name;    debug_print code'
          | S_FASSIGN (obj, f)    -> Printf.eprintf "S_FASSIGN (%s, %s)\n" obj f;    debug_print code'
          | S_CLASS (name, _, _, _) -> Printf.eprintf "S_CLASS %s\n" name;           debug_print code'
          | S_FUN (name, _, locals)      -> Printf.eprintf "S_FUN %s\n" name ;             debug_print code'
          | S_FUN_END              -> Printf.eprintf "S_FUN_END\n";                  debug_print code'

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
                ("int", l_code @ r_code @ [S_BINOP op])
        | Call (name, args) ->
            let (_, fun_type, _, _) = SMCompileEnv.get_func name env in
            let args' = List.rev args in
            (fun_type, (List.fold_left (compile_arg env) [] args') @ [S_CALL (List.length args, name)])
        | MCall (obj, meth, args) ->
            let (tp, code') = compile_expr env obj in
            let m_type = find_method_type meth tp env in
            let args' = List.rev args in
            (m_type, (List.fold_left (compile_arg env) [] args')
                     @ code'
                     @ [S_MCALL ((List.length args) + 1, tp, meth)]) (* +1 for self *)
        | Field (obj, f) ->
            let (tp, code') = compile_expr env obj in
            let f_type = find_field_type f tp env in
            (f_type, code' @ [S_FIELD (tp, f)])
        | New (cls_name, args) ->
            let args' = List.rev args in
            (cls_name, (List.fold_left (compile_arg env) [] args') @ [S_NEW cls_name; S_CALL (1, cls_name ^ "_init")])
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
        | Expr e ->
            (env, snd (compile_expr env e) @ [S_DROP])
        | FieldAssign (obj, f, e) -> 
            (match SMCompileEnv.get_var obj env with
            | Some t -> (env, (snd @@ compile_expr env e) @ [S_LD obj; S_FASSIGN (t, f); S_ST obj])
            | None -> failwith (Printf.sprintf "variable %s is not yet defined" obj))


        let stmt env = fun a -> snd @@ stmt' (new labler) env a

    end

module Prog = 
    struct

        open Language.Unit
        open Language.Stmt
        open Utils


        let compile_func env labler code (name, tp, params, stmt) as f = 
            let (name, (params, locals)) = Meta.get_func_meta f in
            let show_meta = [S_FUN (name, params, locals)] in
            let env' = List.fold_left (fun env -> fun (tp, x) -> SMCompileEnv.add_var x tp env) env params in
            code @ [S_LABLE name] @ show_meta @ (snd @@ Compile.stmt' labler env' stmt) @ [S_FUN_END]

        let compile_const env labler ((name, tp, params, stmt) as c) cls_name = 
            let (name, (params, locals)) = Meta.get_func_meta c in
            let show_meta = [S_FUN (cls_name ^ "_" ^ name, params, locals)] in
            let env' = List.fold_left (fun env -> fun (tp, x) -> SMCompileEnv.add_var x tp env) env params in
            let (_, p, _, _) = SMCompileEnv.get_class cls_name env in
            let p_init_code = 
                (match p with
                 | None -> []
                 | Some s ->         
                    let env'' = SMCompileEnv.add_func (s^"_init") (s^"_init", s, [(s, "self")], Skip) env' in (* provide current env with stub of parent's *)
                    (snd @@ Compile.stmt' labler env'' (Call (s^"_init", [Var ("self")]))))
            in
              [S_LABLE (cls_name ^ "_" ^ name)] 
            @ show_meta
            @ p_init_code
            @ [S_LD "self"]
            @ [S_INIT_VTABLE cls_name]
            @ [S_ST "self"]
            @ (snd @@ Compile.stmt' labler env' stmt)
            @ [S_FUN_END]

        let compile_cls env labler (cls_name, parent, fields, meths) =
            let const::methods = meths in
            let code = List.map (fun (name, tp, params, stmt) -> compile_func env labler [] (cls_name ^ "_" ^ name, tp, params, stmt)) methods in
            let code' = compile_const env labler const cls_name in
            let show_class_meta = [S_CLASS (cls_name, parent, fields, List.map (fun (name, t, params, stmt) -> (name, t, params)) methods)] in
            show_class_meta @ code' @ (List.concat code)

        let compile' prog env labler =
            let (classes, funcs, main) = prog in
            let (_, (_, main_locals)) = Meta.get_func_meta ("main", "", [], main) in
              (List.concat @@ List.map (compile_cls env labler) classes)
            @ (List.fold_left (compile_func env labler) [] funcs)
            @ [S_LABLE "main"]
            @ [S_FUN ("main", [], main_locals)]
            @ (snd @@ Compile.stmt' labler env main)
            @ [S_FUN_END]

        let construct_env (classes, funcs) = 
            let env = SMCompileEnv.init in
            let env' = List.fold_left (fun env -> fun (cls_name, _, _, _) as cls -> SMCompileEnv.add_class cls_name cls env) env classes in
            List.fold_left (fun env -> fun (f_name, _, _, _) as f -> SMCompileEnv.add_func f_name f env) env' funcs

        let compile prog =
            let (classes, funcs, main) = prog in
            let env = construct_env (classes, funcs) in
            let labler = new labler in
            compile' prog env labler

    end
