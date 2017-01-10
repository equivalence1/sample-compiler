module Operation =
  struct

    open Language.Expr

    let perform_op binop =
      match binop with
      | "+"  -> fun l r -> l + r
      | "-"  -> fun l r -> l - r
      | "*"  -> fun l r -> l * r
      | "/"  -> fun l r -> l / r
      | "%"  -> fun l r -> l mod r
      | "<"  -> fun l r -> if l <  r then 1 else 0
      | ">"  -> fun l r -> if l >  r then 1 else 0
      | "==" -> fun l r -> if l =  r then 1 else 0
      | "!=" -> fun l r -> if l <> r then 1 else 0
      | "<=" -> fun l r -> if l <= r then 1 else 0
      | ">=" -> fun l r -> if l >= r then 1 else 0
      | "&&" -> fun l r -> if (l <> 0) && (r <> 0) then 1 else 0
      | "!!" -> fun l r -> if (l <> 0) || (r <> 0) then 1 else 0

    let cmp_to_name cmp =
      match cmp with
      | "<"  -> "l"
      | ">"  -> "g"
      | "==" -> "e"
      | "!=" -> "ne"
      | ">=" -> "ge"
      | "<=" -> "le"

    let name_to_cmp cmp =
      match cmp with
      | "l"  -> "<"
      | "g"  -> ">"
      | "e"  -> "=="
      | "ne" -> "!="
      | "ge" -> ">="
      | "le" -> "<="

  end

module Meta =
    struct

        open Language.Stmt
        
        let rec get_meta acc func_list =
            match func_list with
            | [] -> acc
            | (name, t, params, stmt)::func_list' ->
                let rec get_locals params locals stmt =
                    match stmt with
                    | Ref (t, name) ->
                        (t, name)::locals
                    | Seq (l, r) ->
                        let locals' = get_locals params locals l in
                         get_locals params locals' r
                    | If (e, s1, s2) ->
                        let locals' = get_locals params locals s1 in
                        get_locals params locals' s2
                    | While (e, s) ->
                        get_locals params locals s
                    | _ -> locals
                in
                if name <> "main"
                then get_meta ((name, (params, get_locals params [] stmt))::acc) func_list'
                else get_meta ((name, (params, []))::acc) func_list' (* main function has no locals *)

        (* (method name, class name) list *)
        let rec get_vtable cls_name classes =
            let (_, parent, _, methods) = List.find (fun (name, _, _, _) -> name = cls_name) classes in
            let add_method vtable meth =
                let rec add_method' acc vtable' meth =
                    match vtable' with
                    | [] -> acc @ [(cls_name, meth)]
                    | (cls_name', name)::rest ->
                        if name = meth
                        then acc @ [(cls_name, meth)] @ rest
                        else add_method' (acc @ [(cls_name', name)]) rest meth
                in
                add_method' [] vtable meth
            in
            let p_table = 
                (match parent with
                 | None -> [] 
                 | Some p -> get_vtable p classes)
            in
            List.fold_left (fun vtable -> fun (m_name, _, _, _) -> add_method vtable m_name) p_table methods


        (* (field name, class name) list *)
        let rec get_obj_layout cls_name classes =
            let (_, parent, fields, _) = List.find (fun (name, _, _, _) -> name = cls_name) classes in
            let p_layout = 
                (match parent with
                 | None -> []
                 | Some p -> get_obj_layout p classes)
            in
            List.fold_left (fun layout -> fun (t, name) -> layout @ [(name, cls_name)]) p_layout fields


        (* functions in this module below are for debugging *)

        
        let rec print_meta funcs_meta =
            match funcs_meta with
            | [] -> Printf.eprintf "\n"; ()
            | (name, (params, locals))::funcs_meta' ->
                Printf.eprintf "<%s>:\n" name;
                Printf.eprintf "  params:\n";
                List.iter (fun (t, x) -> Printf.eprintf "\t(%s, %s)\n" t x) params;
                Printf.eprintf "  locals:\n";
                List.iter (fun (t, x) -> Printf.eprintf "\t(%s, %s)\n" t x) locals;
                Printf.eprintf "\n\n";
                print_meta funcs_meta'

        let rec print_vtable vtable =
            Printf.eprintf "vtable:\n";
            List.iter (fun (name, cls) -> Printf.eprintf "(%s, %s)\n" name cls) vtable;
            Printf.eprintf "\n";
            ()

        let rec print_layout layout =
            Printf.eprintf "layout:\n";
            Printf.eprintf "(vtable)\n";
            List.iter (fun (name, cls) -> Printf.eprintf "(%s, %s)\n" name cls) layout;
            Printf.eprintf "\n";
            ()


    end

module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

module InterpreterEnv =
    struct
        (* Map of functions: f(args, cur_env) -> (result, new_env), state *)
        type t = (int list -> t -> int) StringMap.t * (string * int) list

        let init_env = (StringMap.empty, [])

        let add_func name f (fmap, state) = (StringMap.add name f fmap, state)
        let get_func name   (fmap, state) = StringMap.find name fmap

        let set_var name value (fmap, state) = (fmap, (name, value)::state)
        let get_var name       (fmap, state) = List.assoc name state
    end

module StackMachineEnv =
    struct
        (* return address, state *)
        type t = int * (string * int) list

        let init_env = (0, [])

        let set_ret addr (_, state) = (addr, state)
        let get_ret (addr, state)   = addr

        let set_var name value (addr, state) = (addr, (name, value)::state)
        let get_var name       (addr, state) = List.assoc name state
    end

module SMCompileEnv =
    struct
        open Language

        (*   (variable -> type), (fun name -> fun info = (name, return type, params, stmt)), (class name -> class info = (name, parent, fields, constructros, methods))   *)
        type t = string StringMap.t 
                 *                                                      ((string * string * ((string * string) list) * Stmt.t) list) StringMap.t
                 * (string * string option * ((string * string) list) * ((string * string * ((string * string) list) * Stmt.t) list)) StringMap.t

        let init = (StringMap.empty, StringMap.empty, StringMap.empty)
        let init2 (a, b, c) = (StringMap.empty, b, c)
        
        let add_var name tp    (vars, a, b) = (StringMap.add name tp vars, a, b)
        let get_var name       (vars, a, b) = 
            try Some (StringMap.find name vars) with
            | Not_found -> None

        let add_func name info (a, funcs, b) = (a, StringMap.add name info funcs, b)
        let get_func name      (a, funcs, b) = StringMap.find name funcs

        let add_class name info (a, b, classes) = 
            let (cls_name, parent, fields, methods) = info in
            let new_methods = List.map (fun (name, tp, params, stmt) -> (name, tp, (cls_name, "self")::params, stmt)) methods in
            (a, b, StringMap.add name (cls_name, parent, fields, new_methods) classes)
        let get_class name      (a, b, classes) = StringMap.find name classes

    end

module X86MetaEnv =
    struct
            (*    parent         vtable                  objlayout                          params         locals     *)
        type t = (string * (string * string) list * (string * string) list) StringMap.t * ((string * string) list * (string * string) list) StringMap.t

        let init = (StringMap.empty, StringMap.empty)

        let add_cls_meta name cls_meta (a, b) = (StringMap.add name cls_meta a, b)
        let get_cls_meta name          (a, b) = StringMap.find name a

        let add_fun_meta name fun_meta (a, b) = (a, StringMap.add name fun_meta b)
        let get_fun_meta name          (a, b) = StringMap.find name b

    end
