module Expr =
    struct

        open Language.Expr
        open Utils.Operation
        open Utils.InterpreterEnv

        let rec eval ((fmap, state, input, output) as env) = function
        | Const n -> (n, env)
        | Var   x -> (get_var x env, env)
        | BinOp (op, l, r) ->
            let (result_l, env') = eval env l in
            let (result_r, env'') = eval env' r in
                (perform_op op result_l result_r, env'')
        | Call (name, args) ->
            let eval_one (evaluated_args, env) arg =
                let (result, env') = eval env arg in
                (evaluated_args @ [result], env')
            in
            let (args', env') = List.fold_left eval_one ([], env) args in (* (string, expr) list -> (string, int) list *)
            let (fmap, _, input', output') = env' in
            let func = get_func name env in
            let (result, (fmap, _, input'', output'')) = func args' (fmap, [], input', output') in (* evaluate function *)
                (result, (fmap, state, input'', output'')) (* restore original state *)

    end
  
module Stmt =
    struct

        open Language.Stmt

        let rec eval' env stmt =
                match stmt with
                | Skip          -> (`Continue, env)
                | Seq    (l, r) -> 
                    (let (result, env') = eval' env l in
                    match result with
                    | `Return x -> (`Return x, env')
                    | `Continue -> eval' env' r)
                | Assign (x, e) ->
                    let (value, env') = Expr.eval env e in
                    (`Continue, Utils.InterpreterEnv.set_var x value env')
                | Write   e     ->
                    let (value, env') = Expr.eval env e in
                    (`Continue, Utils.InterpreterEnv.write_value value env')
                | Read    x     ->
                    (`Continue, Utils.InterpreterEnv.read_var x env)
                | If (e, s1, s2) ->
                    let (result, env') = Expr.eval env e in
                    if result <> 0 then
                        eval' env' s1
                    else
                        eval' env' s2
                | While (e, s)  ->
                    let (result, env') = Expr.eval env e in
                    if result <> 0 then
                        eval' env' (Seq (s, While (e, s)))
                    else
                        (`Continue, env')
                | Return  e     ->
                    let (result, env') = Expr.eval env e in
                    (`Return result, env')
                | Call (name, args) ->
                    let (result, env') = Expr.eval env (Call (name, args)) in
                    (`Continue, env')

        let eval input func_list stmt =
            let env = Utils.InterpreterEnv.init_env input in
            let func_adder cur_env (name, (params, func_stmt)) = 
                let func args (fmap, _, input_before_call, output_before_call) = 
                    let func_state = List.combine params args in
                    let env_in_func = (fmap, func_state, input_before_call, output_before_call) in
                    let (res, env_after_call) = eval' env_in_func func_stmt in
                    match res with
                    | `Continue -> failwith "void functions are not supported"
                    | `Return x -> (x, env_after_call)
                in
                Utils.InterpreterEnv.add_func name func cur_env
            in
            let env' = List.fold_left func_adder env func_list in
            let (_, (_, _, _, output)) = eval' env' stmt in
            output

    end

module Prog =
    struct
        open Language.Unit

        let eval input prog = 
        match prog with
        | (funcs, main) -> Stmt.eval input funcs main

    end
