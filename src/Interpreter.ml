module Expr =
    struct

        open Language.Expr
        open Utils
        open Utils.InterpreterEnv
    
        let bin_op op o_l o_r =
            match (o_l, o_r) with
            | (Int l, Int r) -> Operation.perform_op op l r
            | (_, _) ->
                if op = "!="
                then 1
                else 0


        let rec eval ((fmap, state) as env) meta_env = function
        | Const n -> (Int n)
        | Var   x -> InterpreterEnv.get_var x env
        | BinOp (op, l, r) ->
            let o_l = eval env meta_env l in
            let o_r = eval env meta_env r in
                Int (bin_op op o_l o_r)
        | Call (name, args) ->
            let func = InterpreterEnv.get_func name env in
            let args' = List.map (eval env meta_env) args in
            func args' (fmap, StringMap.empty) meta_env
        | New (t, args) ->
            let (_, vtable, _) = ClassMetaEnv.get_cls_meta t meta_env in
            let methods = List.fold_left (fun methods -> fun (cls_name, meth) -> StringMap.add meth cls_name methods) StringMap.empty vtable in
            let obj = (Object (ref StringMap.empty, methods)) in
            let constructor = InterpreterEnv.get_func (t ^ "_init") env in
            constructor [obj] (fmap, StringMap.empty) meta_env
        | MCall (obj, name, args) ->
            let (Object (fields, methods)) as obj = eval env meta_env obj in
            let meth_full_name = (StringMap.find name methods) ^ "_" ^ name in
            let func = InterpreterEnv.get_func meth_full_name env in
            let args' = obj::(List.map (eval env meta_env) args) in
            func args' (fmap, StringMap.empty) meta_env
        | Field (obj, name) ->
            let Object (fields, methods) = eval env meta_env obj in
            StringMap.find name !fields

    end

module Stmt =
    struct

        open Language.Stmt
        open Utils
        open Utils.InterpreterEnv

        let rec eval' env meta_env stmt =
            match stmt with
            | Skip          -> (`Continue, env)
            | Seq    (l, r) -> 
                (let (result, env') = eval' env meta_env l in
                match result with
                | `Return x -> (`Return x, env')
                | `Continue -> eval' env' meta_env r)
            | Assign (x, e) ->
                let value = Expr.eval env meta_env e in
                (`Continue, InterpreterEnv.set_var x value env)
            | Write   e     ->
                let (Int value) = Expr.eval env meta_env e in
                Printf.printf "%d\n" value;
                (`Continue, env)
            | Read    x     ->
                Printf.printf "> ";
                let value = read_int () in
                (`Continue, InterpreterEnv.set_var x (Int value) env)
            | If (e, s1, s2) ->
                let (Int result) = Expr.eval env meta_env e in
                if result <> 0 then
                    eval' env meta_env s1
                else
                    eval' env meta_env s2
            | While (e, s)  ->
                let (Int result) = Expr.eval env meta_env e in
                if result <> 0 then
                    eval' env meta_env (Seq (s, While (e, s)))
                else
                    (`Continue, env)
            | Return  e     ->
                let result = Expr.eval env meta_env e in
                (`Return result, env)
            | Call (name, args) ->
                let result = Expr.eval env meta_env (Call (name, args)) in
                (`Continue, env)
            | Ref _ ->
                (`Continue, env)
            | FieldAssign (obj, name, e) ->
                let value = Expr.eval env meta_env e in
                let Object (fields, _) = InterpreterEnv.get_var obj env in
                fields := StringMap.add name value !fields;
                (`Continue, env)
            | Expr e ->
                Expr.eval env meta_env e;
                (`Continue, env)


        let eval (classes, funcs, main) =
            let env = InterpreterEnv.init_env in
            let func_adder cur_env (name, t, params, func_stmt) =
                let func args (fmap, _) meta_env =
                    let func_state = List.fold_left (fun m -> fun (n, a) -> StringMap.add n a m) StringMap.empty (List.combine (List.map snd params) args) in
                    let env_in_func = (fmap, func_state) in
                    let (res, env_after_call) = eval' env_in_func meta_env func_stmt in
                    match res with
                    | `Continue -> failwith "void functions are not supported"
                    | `Return x -> x
                in
                InterpreterEnv.add_func name func cur_env
            in
            let class_adder cls meta_env =
                let (cls_name, parent, fields, methods) = cls in
                let methods = List.map (fun (name, t, params, stmt) -> (name, t, params)) methods in
                ClassMetaEnv.add_cls (cls_name, parent, fields, methods) meta_env
            in
            let meta_env = List.fold_left (fun env -> fun cls -> class_adder cls env) ClassMetaEnv.init classes in
            let get_cls_methods (cls_name, _, _, methods) =
                List.map (fun (name, t, params, stmt) -> (cls_name ^ "_" ^ name, t, params, stmt)) methods
            in
            let all_methods = List.concat (List.map get_cls_methods classes) in
            let funcs_list = funcs @ all_methods in
            let env' = List.fold_left func_adder env funcs_list in
            eval' env' meta_env main

    end

module Prog =
    struct

        open Language.Unit

        let eval prog = Stmt.eval prog

    end
