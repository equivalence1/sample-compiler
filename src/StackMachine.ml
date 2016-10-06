type i =
| S_READ
| S_WRITE
| S_PUSH  of int
| S_LD    of string
| S_ST    of string
| S_BINOP of string

module Interpreter =
  struct

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
              | S_BINOP "+" ->
		  let y::x::stack' = stack in
		  (state, (x + y)::stack', input, output)
              | S_BINOP "-" ->
		  let y::x::stack' = stack in
		  (state, (x - y)::stack', input, output)
              | S_BINOP "*" ->
		  let y::x::stack' = stack in
		  (state, (x * y)::stack', input, output)
              | S_BINOP "/" ->
		  let y::x::stack' = stack in
		  (state, (x / y)::stack', input, output)
              | S_BINOP "%" ->
      let y::x::stack' = stack in
      (state, (x mod y)::stack', input, output)
              | S_BINOP "<" ->
      let y::x::stack' = stack in
      if x < y then
        (state, 1::stack', input, output)
      else
        (state, 0::stack', input, output)
              | S_BINOP ">" ->
      let y::x::stack' = stack in
      if x > y then
        (state, 1::stack', input, output)
      else
        (state, 0::stack', input, output)
              | S_BINOP "==" ->
      let y::x::stack' = stack in
      if x == y then
        (state, 1::stack', input, output)
      else
        (state, 0::stack', input, output)
              | S_BINOP "!=" ->
      let y::x::stack' = stack in
      if x != y then
        (state, 1::stack', input, output)
      else
        (state, 0::stack', input, output)
              | S_BINOP "<=" ->
      let y::x::stack' = stack in
      if x <= y then
        (state, 1::stack', input, output)
      else
        (state, 0::stack', input, output)
              | S_BINOP ">=" ->
      let y::x::stack' = stack in
      if x >= y then
        (state, 1::stack', input, output)
      else
        (state, 0::stack', input, output)
  (* && and !! operations are represented through operations above. See `compile_expr` for more details.  *)
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
    | BinOp  ("+",  l, r) -> compile_expr l @ compile_expr r @ [S_BINOP "+"]
    | BinOp  ("-",  l, r) -> compile_expr l @ compile_expr r @ [S_BINOP "-"]
    | BinOp  ("*",  l, r) -> compile_expr l @ compile_expr r @ [S_BINOP "*"]
    | BinOp  ("/",  l, r) -> compile_expr l @ compile_expr r @ [S_BINOP "/"]
    | BinOp  ("%",  l, r) -> 
      let l' = compile_expr l in
      let r' = compile_expr r in
        l' @ r' @ l' @ r' @ [S_BINOP "/"; S_BINOP "*"; S_BINOP "-"]
    | BinOp  ("<",  l, r) -> compile_expr l @ compile_expr r @ [S_BINOP "<"]
    | BinOp  (">",  l, r) -> compile_expr l @ compile_expr r @ [S_BINOP ">"]
    | BinOp  ("==", l, r) -> compile_expr l @ compile_expr r @ [S_BINOP "=="]
    | BinOp  ("!=", l, r) -> compile_expr l @ compile_expr r @ [S_BINOP "!="]
    | BinOp  ("<=", l, r) -> compile_expr l @ compile_expr r @ [S_BINOP "<="]
    | BinOp  (">=", l, r) -> compile_expr l @ compile_expr r @ [S_BINOP ">="]
    | BinOp  ("&&", l, r) -> 
      let l'  = compile_expr l in
      let r'  = compile_expr r in
      let z   = compile_expr @@ Const 0 in
      let two = compile_expr @@ Const 2 in
        two @ z @ l' @ [S_BINOP "!="] @ z @ r' @ [S_BINOP "!="] @ [S_BINOP "+"] @ [S_BINOP "=="]
    | BinOp  ("!!", l, r) -> 
      let l'  = compile_expr l in
      let r'  = compile_expr r in
      let z   = compile_expr @@ Const 0 in
        z @ z @ l' @ [S_BINOP "!="] @ z @ r' @ [S_BINOP "!="] @ [S_BINOP "+"] @ [S_BINOP "<"]

    let rec stmt = function
    | Skip          -> []
    | Assign (x, e) -> compile_expr e @ [S_ST x]
    | Read    x     -> [S_READ; S_ST x]
    | Write   e     -> compile_expr e @ [S_WRITE]
    | Seq    (l, r) -> stmt l @ stmt r

  end
