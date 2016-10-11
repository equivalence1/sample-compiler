module Operation =
  struct

    open Language.Expr
    
    type t = 
    | IntBinOp of string * int * int

    let perform_op binop =
      match binop with
      | IntBinOp ("+",  l, r) -> l + r
      | IntBinOp ("-",  l, r) -> l - r
      | IntBinOp ("*",  l, r) -> l * r
      | IntBinOp ("/",  l, r) -> l / r
      | IntBinOp ("%",  l, r) -> l mod r
      | IntBinOp ("<",  l, r) -> if l <  r then 1 else 0
      | IntBinOp (">",  l, r) -> if l >  r then 1 else 0
      | IntBinOp ("==", l, r) -> if l == r then 1 else 0
      | IntBinOp ("!=", l, r) -> if l != r then 1 else 0
      | IntBinOp ("<=", l, r) -> if l <= r then 1 else 0
      | IntBinOp (">=", l, r) -> if l >= r then 1 else 0
      | IntBinOp ("&&", l, r) -> if (l != 0) && (r != 0) then 1 else 0
      | IntBinOp ("!!", l, r) -> if (l != 0) || (l != 0) then 1 else 0

  end
