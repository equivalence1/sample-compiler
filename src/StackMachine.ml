type i =
| S_READ
| S_WRITE
| S_PUSH  of int
| S_LD    of string
| S_ST    of string
| S_ASSIGN  of string * int
| S_BINOP of string
| S_JMP   of string
| S_JMPC  of string * string (* jmpc compares with zero *)
| S_LABLE of string

module Interpreter =
  struct

    open Utils.Operation

    let rec find_lable l code =
      match code with
      | [] -> []
      | (S_LABLE l1)::code' ->
        if l1 = l then
          code'
        else
          find_lable l code'
      | i::code' -> find_lable l code'
      | _ -> failwith "find_lable: no matching"

    let run input code =
      let rec run' (state, stack, input, output) code full_code =
    	match code with
    	| []       -> output
    	| i::code' ->
          match i with
          | S_READ ->
    		let y::input' = input in
    		run' (state, y::stack, input', output) code' full_code

          | S_WRITE ->
    		let y::stack' = stack in
    	    run' (state, stack', input, output @ [y]) code' full_code

          | S_PUSH n ->
    		run' (state, n::stack, input, output) code' full_code

          | S_LD x ->
    		run' (state, (List.assoc x state)::stack, input, output) code' full_code
          
          | S_ST x ->
    		let y::stack' = stack in
    		run' ((x, y)::state, stack', input, output) code' full_code
          
          | S_ASSIGN (x, n) ->
    		run' ((x, n)::state, stack, input, output) code' full_code
          
          | S_BINOP op ->
    		let y::x::stack' = stack in
    		run' (state, (perform_op op x y)::stack', input, output) code' full_code

          | S_LABLE _ -> 
            run' (state, stack, input, output) code' full_code

          | S_JMP l ->
            run' (state, stack, input, output) (find_lable l full_code) full_code

          | S_JMPC (cmp, l) ->
            let x::stack' = stack in
            match (perform_op (name_to_cmp cmp) x 0) with
            | 0 -> run' (state, stack', input, output) code' full_code
            | _ -> run' (state, stack', input, output) (find_lable l full_code) full_code

          | _ -> failwith "run: no matching"

    in
    run' ([], [], input, []) code code

    let rec debug_print code =
        match code with
        | [] -> Printf.printf "\n"
        | i::code' ->
          match i with
          | S_READ -> Printf.printf "S_READ; "; debug_print code'
          | S_WRITE -> Printf.printf "S_WRITE; "; debug_print code'
          | S_PUSH n -> Printf.printf "S_PUSH %d; " n; debug_print code'
          | S_LD x -> Printf.printf "S_LD %s; " x; debug_print code'
          | S_ST x -> Printf.printf "S_ST %s; " x; debug_print code'
          | S_BINOP op -> Printf.printf "S_BINOP %s; " op; debug_print code'
          | S_LABLE l -> Printf.printf "S_LABLE %s; " l; debug_print code'
          | S_JMP l -> Printf.printf "S_JMP %s; " l; debug_print code'
          | S_JMPC (cmp, l) -> Printf.printf "S_JMPC (%s, %s); " cmp l; debug_print code'

  end

class labler =
  object(self)
    val    if_lable      = ref 0
    method inc_if_lable  = if_lable := !if_lable + 1
    method cur_if_lable  = "if_lable_" ^ string_of_int (!if_lable + 1)
    method next_if_lable = self#inc_if_lable; self#cur_if_lable

    val    while_lable      = ref 0
    method inc_while_lable  = while_lable := !while_lable + 1
    method cur_while_lable  = "while_lable_" ^ string_of_int (!while_lable + 1)
    method next_while_lable = self#inc_while_lable; self#cur_while_lable
  end

(* I separate `if` and `while` lables only to make .s files clearer for debugging *)

module Compile =
  struct

    open Language.Expr
    open Language.Stmt

    let lable_counter = ref 0

    let rec compile_expr = function
    | Var   x -> [S_LD   x]
    | Const n -> [S_PUSH n]
    | BinOp (op, l, r) ->
      let l' = compile_expr l in
      let r' = compile_expr r in
        l' @ r' @ [S_BINOP op]
    | _ -> failwith "compile_expr: no matching"

    let stmt =
      let rec stmt' labler = function
      | Skip          -> []
      | Assign (x, e) -> 
        (match e with
        | Const n -> [S_ASSIGN (x, n)]
        | _       -> compile_expr e @ [S_ST x])
      | Read    x     -> [S_READ; S_ST x]
      | Write   e     -> compile_expr e @ [S_WRITE]
      | Seq    (l, r) -> stmt' labler l @ stmt' labler r
      | If (e, s1, s2) -> 
        let l1 = labler#next_if_lable in
        let l2 = labler#next_if_lable in 
          compile_expr e @ [S_JMPC ("e", l1)] @ stmt' labler s1 @ [S_JMP l2; S_LABLE l1] @ stmt' labler s2 @ [S_LABLE l2]
      | While (e, s1) -> 
        let l1 = labler#next_while_lable in
        let l2 = labler#next_while_lable in 
          [S_LABLE l1] @ compile_expr e @ [S_JMPC ("e", l2)] @ stmt' labler s1 @ [S_JMP l1; S_LABLE l2]
      | _ -> failwith "stmt: no matching"
    in stmt' (new labler)

  end
