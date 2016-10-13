type i =
| S_READ
| S_WRITE
| S_PUSH  of int
| S_LD    of string
| S_ST    of string
| S_BINOP of string

module Interpreter =
  struct

    open Utils.Operation

    let run input code =
      let rec run' (state, stack, input, output) code =
	match code with
	| []       -> output
	| i::code' ->
	    run'
              (match i with
              | S_READ ->
		  let y::input' = input in
		  (state, y::stack, input', output)
              | S_WRITE ->
		  let y::stack' = stack in
		  (state, stack', input, output @ [y])
              | S_PUSH n ->
		  (state, n::stack, input, output)
              | S_LD x ->
		  (state, (List.assoc x state)::stack, input, output)
              | S_ST x ->
		  let y::stack' = stack in
		  ((x, y)::state, stack', input, output)
              | S_BINOP op ->
		  let y::x::stack' = stack in
		  (state, (perform_op op x y)::stack', input, output)
              )
              code'
      in
      run' ([], [], input, []) code
	
  end

module Compile =
  struct

    open Language.Expr
    open Language.Stmt

    let rec compile_expr = function
    | Var   x -> [S_LD   x]
    | Const n -> [S_PUSH n]
    | BinOp (op, l, r) ->
      let l' = compile_expr l in
      let r' = compile_expr r in
        l' @ r' @ [S_BINOP op]

    let rec stmt = function
    | Skip          -> []
    | Assign (x, e) -> compile_expr e @ [S_ST x]
    | Read    x     -> [S_READ; S_ST x]
    | Write   e     -> compile_expr e @ [S_WRITE]
    | Seq    (l, r) -> stmt l @ stmt r

  end
