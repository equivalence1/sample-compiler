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

module StringMap = Map.Make (String)

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
