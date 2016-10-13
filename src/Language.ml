open Ostap 
open Matcher

module Expr =
  struct

    type t =
    | Const of int
    | Var   of string
    | BinOp of string * t * t

    let make_foldl l suf = List.fold_left (fun l (op, r) -> BinOp (Token.repr op, l, r)) l suf

    ostap (
      parse:
        ori;

      ori:
        l:andi suf:(("!!") andi)* {
          make_foldl l suf
        }
      | andi;

      andi:
        l:cmpi suf:(("&&") cmpi)* {
          make_foldl l suf
        }
      | cmpi;

      cmpi:
        l:addi suf:(("<=" | "<" | "==" | "!=" | ">=" | ">" | "!!" | "&&") addi)* {
          make_foldl l suf
        }
      | addi;

      addi:
        l:mulli suf:(("+" | "-") mulli)* {
          make_foldl l suf
        }
      | mulli;

      mulli:
        l:primary suf:(("*" | "/" | "%") primary)* {
          make_foldl l suf
        }
      | primary;

      primary:
        n:DECIMAL {Const n}
      | x:IDENT   {Var   x}
      | -"(" ori -")"
    )

  end

module Stmt =
  struct

    type t =
    | Skip
    | Read   of string
    | Write  of Expr.t
    | Assign of string * Expr.t
    | Seq    of t * t

    ostap (
      parse: s:simple d:(-";" parse)? {
	match d with None -> s | Some d -> Seq (s, d)
      };
      simple:
        x:IDENT ":=" e:!(Expr.parse)     {Assign (x, e)}
      | %"read"  "(" x:IDENT ")"         {Read x}
      | %"write" "(" e:!(Expr.parse) ")" {Write e}
      | %"skip"                          {Skip}
    )

  end
