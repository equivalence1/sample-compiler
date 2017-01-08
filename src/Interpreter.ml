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
                    | `Continue -> 0
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
            | (classes, funcs, main) -> Stmt.eval funcs main; ()


    (* Debug print to check parsing *)

        open Language.Expr
        open Language.Stmt

        let rec print_expr e =
            match e with
            | Const n -> Printf.printf "%d" n;
            | Var x   -> Printf.printf "%s" x;
            | BinOp (op, x, y) -> print_expr x; Printf.printf "%s" op; print_expr y
            | Call (name, args) -> Printf.printf "%s(" name; List.iter (fun arg -> print_expr arg; Printf.printf ", ") args; Printf.printf ")"
            | New (cls, args) -> Printf.printf "new %s(" cls; List.iter (fun arg -> print_expr arg; Printf.printf ", ") args; Printf.printf ")"
            | MCall (obj, m, args) -> Printf.printf "%s.%s(" obj m; List.iter (fun arg -> print_expr arg; Printf.printf ", ") args; Printf.printf ")"

        let rec print_stmt stmt =
            match stmt with
            | Skip           -> Printf.printf "\n"
            | Seq    (l, r)  -> print_stmt l; print_stmt r
            | Ref (tp, x)    -> Printf.printf "%s %s" tp x
            | Assign (x, e)  -> Printf.printf "%s := " x; print_expr e; Printf.printf "\n"
            | Write e        -> Printf.printf "write("; print_expr e; Printf.printf ")\n"
            | Read x         -> Printf.printf "read(%s)\n" x
            | If (e, s1, s2) -> Printf.printf "if ("; print_expr e; Printf.printf ")\n"; print_stmt s1; Printf.printf "else\n"; print_stmt s2; Printf.printf "fi\n"
            | While (e, s)   -> Printf.printf "while ("; print_expr e; Printf.printf ")\n"; print_stmt s
            | Return e       -> Printf.printf "return "; print_expr e; Printf.printf "\n"
            | Call (name, args) -> Printf.printf "%s(" name; List.iter (fun arg -> print_expr arg; Printf.printf ", ") args; Printf.printf ")\n"
            | MCall (obj, m, args) -> Printf.printf "%s.%s(" obj m; List.iter (fun arg -> print_expr arg; Printf.printf ", ") args; Printf.printf ")\n"

        let print_fun f =
            let (name, tp, params, stmt) = f in
            Printf.printf "fun %s(" name;
            List.iter (fun (tp, x) -> Printf.printf "%s %s, " tp x) params;
            Printf.printf "): %s\nbegin\n" tp;
            print_stmt stmt;
            Printf.printf "end\n\n"
        
        let print_cons c =
            let (name, params, stmt) = c in
            Printf.printf "%s(" name;
            List.iter (fun (tp, x) -> Printf.printf "%s %s, " tp x) params;
            print_stmt stmt;
            Printf.printf "end\n\n"

        let print_class cls =
            let (name, constructors, fields, methods, ext) = cls in
            Printf.printf "Class %s" name;
            (match ext with
            | Some s -> Printf.printf " extends %s\n" s
            | _      -> Printf.printf "\n");
            Printf.printf "begin\n";
            List.iter (fun (tp, name) -> Printf.printf "    %s %s\n" tp name) fields;
            Printf.printf "\n";
            List.iter (fun cons -> print_cons cons) constructors;
            Printf.printf "\n";
            List.iter (fun meth -> print_fun meth) methods;
            Printf.printf "end\n\n\n"

        let print prog =
            let (classes, funcs, main) = prog in
            List.iter (fun cls -> print_class cls) classes;
            List.iter (fun f -> print_fun f) funcs;
            Printf.printf "\n\nMain:\n\n";
            print_stmt main

    (* Debug print to check parsing *)

    end
