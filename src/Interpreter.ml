module Expr =
    struct

        open Language.Expr
        open Utils

        let rec eval ((fmap, state) as env) = function
        | Const n -> n
        | Var   x -> InterpreterEnv.get_var x env
        | BinOp (op, l, r) ->
            let l' = eval env l in
            let r' = eval env r in
                Operation.perform_op op l' r'
        | Call (name, args) ->
            let func = InterpreterEnv.get_func name env in
            let args' = List.map (eval env) args in
            func args' (fmap, [])

    end
  
module Stmt =
    struct

        open Language.Stmt
        open Utils

        let rec eval' env stmt =
                match stmt with
                | Skip          -> (`Continue, env)
                | Seq    (l, r) -> 
                    (let (result, env') = eval' env l in
                    match result with
                    | `Return x -> (`Return x, env')
                    | `Continue -> eval' env' r)
                | Assign (x, e) ->
                    let value = Expr.eval env e in
                    (`Continue, InterpreterEnv.set_var x value env)
                | Write   e     ->
                    let value = Expr.eval env e in
                    Printf.printf "%d\n" value;
                    (`Continue, env)
                | Read    x     ->
                    Printf.printf "> ";
                    let value = read_int () in
                    (`Continue, InterpreterEnv.set_var x value env)
                | If (e, s1, s2) ->
                    let result = Expr.eval env e in
                    if result <> 0 then
                        eval' env s1
                    else
                        eval' env s2
                | While (e, s)  ->
                    let result = Expr.eval env e in
                    if result <> 0 then
                        eval' env (Seq (s, While (e, s)))
                    else
                        (`Continue, env)
                | Return  e     ->
                    let result = Expr.eval env e in
                    (`Return result, env)
                | Call (name, args) ->
                    let result = Expr.eval env (Call (name, args)) in
                    (`Continue, env)

        let eval func_list main =
            let env = InterpreterEnv.init_env in
            let func_adder cur_env (name, (params, func_stmt)) = 
                let func args (fmap, _) = 
                    let func_state = List.combine params args in
                    let env_in_func = (fmap, func_state) in
                    let (res, env_after_call) = eval' env_in_func func_stmt in
                    match res with
                    | `Continue -> failwith "void functions are not supported"
                    | `Return x -> x
                in
                InterpreterEnv.add_func name func cur_env
            in
            let env' = List.fold_left func_adder env func_list in
            eval' env' main

    end

module Prog =
    struct
        open Language.Unit

        let eval prog = 
            match prog with
            | (funcs, main) -> Stmt.eval funcs main; ()

    end
