module Expr =
  struct

    open Language.Expr
    open Utils.Operation

    let rec eval state = function
    | Const n -> n
    | Var   x -> state x
    | BinOp (op, l, r) -> perform_op op (eval state l) (eval state r)
 
  end
  
module Stmt =
  struct

    open Language.Stmt

    let eval input stmt =
      let rec eval' ((state, input, output) as c) stmt =
	      let state' x = List.assoc x state in
	      match stmt with
	      | Skip          -> c
	      | Seq    (l, r) -> eval' (eval' c l) r
	      | Assign (x, e) -> ((x, Expr.eval state' e) :: state, input, output)
	      | Write   e     -> (state, input, output @ [Expr.eval state' e])
	      | Read    x     ->
	        let y::input' = input in
	        ((x, y) :: state, input', output)
          | If (e, s1, s2) -> 
            let e_val = Expr.eval state' e in
            if e_val <> 0 then
              eval' c s1
            else
              eval' c s2
          | While (e, s) ->
            let e_val = Expr.eval state' e in
            if e_val <> 0 then
              eval' c @@ Seq (s, While (e, s))
            else
              c
      in
      let (_, _, result) = eval' ([], input, []) stmt in
      result

  end
