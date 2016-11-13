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
        (* Map of functions, state, input, output *)
        type t = (int list -> t -> (int * t)) StringMap.t * (string * int) list * int list * int list

        let init_env input = (StringMap.empty, [], input, [])

        let add_func name f (fmap, state, input, output) = (StringMap.add name f fmap, state, input, output)
        let get_func name   (fmap, state, input, output) = StringMap.find name fmap

        let set_var name value (fmap, state, input, output) = (fmap, (name, value)::state,  input, output)
        let get_var name       (fmap, state, input, output) = List.assoc name state

        let write_value value (fmap, state, input, output) = (fmap, state, input, output @ [value])
        let read_var name (fmap, state, input, output) = 
            match input with
            | [] -> failwith "Cannot read. Input is empty"
            | i::input' -> (fmap, (name, i)::state, input', output)
    end
