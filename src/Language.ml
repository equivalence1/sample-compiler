open Ostap
open Matcher

module Expr =
  struct
    
    type t =
    | Const of int
    | Var   of string
    | BinOp of string * t * t
    | Call  of string * t list

    ostap (
      parse:
	  !(Ostap.Util.expr 
             (fun x -> x)
	     (Array.map (fun (a, s) -> a, 
                         List.map  (fun s -> ostap(- $(s)), (fun x y -> BinOp (s, x, y))) s
                        ) 
              [|                
		`Lefta, ["!!"];
		`Lefta, ["&&"];
		`Nona , ["=="; "!="; "<="; "<"; ">="; ">"];
		`Lefta, ["+" ; "-"];
		`Lefta, ["*" ; "/"; "%"];
              |] 
	     )
	     primary);
      primary:
        n:DECIMAL {Const n}
      | f:IDENT args:(-"(" !(Util.list0 parse) -")")? {
	  match args with 
	  | None      -> Var f 
	  | Some args -> Call (f, args)
        }      
      | -"(" parse -")"
    )

  end

module Stmt =
  struct

    type t =
    | Skip
    | Assign of string * Expr.t
    | Read   of string
    | Write  of Expr.t
    | Seq    of t * t
    | If     of Expr.t * t * t
    | While  of Expr.t * t
    | Call   of string * Expr.t list
    | Return of Expr.t

    ostap (
      parse: s:simple d:(-";" parse)? {match d with None -> s | Some d -> Seq (s, d)};
      expr : !(Expr.parse);
      simple:
        x:IDENT s:(":=" e:expr {Assign (x, e)} | 
                   "(" args:!(Util.list0 expr) ")" {Call (x, args)}
                  ) {s}
      | %"read"  "(" x:IDENT ")" {Read x}
      | %"write" "(" e:expr  ")" {Write e}
      | %"skip"                  {Skip}
      | %"return" e:expr         {Return e} 
      | %"if" e:expr 
	  %"then" the:parse 
          elif:(%"elif" expr %"then" parse)*
	  els:(%"else" parse)? 
        %"fi" {
          If (e, the, 
	         List.fold_right 
		   (fun (e, t) elif -> If (e, t, elif)) 
		   elif
		   (match els with None -> Skip | Some s -> s)
          )
        }
      | %"while"  e:expr %"do"  body:parse %"od" {While (e, body)}
      | %"repeat" s:parse %"until" e:expr {Seq (s, While (BinOp ("==", e, Const 0) , s))}
      | %"for" i:parse "," c:expr "," s:parse %"do" b:parse %"od" {
	  Seq (i, While (c, Seq (b, s)))
        }
    )

  end

module Def =
  struct

    type t = string * (string list * Stmt.t)

    ostap (
      arg  : IDENT;
      parse: %"fun" name:IDENT "(" args:!(Util.list0 arg) ")" %"begin" body:!(Stmt.parse) %"end" {
        (name, (args, body))
      }
    )

  end

module Unit =
  struct

    type t = Def.t list * Stmt.t

    ostap (
      parse: !(Def.parse)* !(Stmt.parse)
    )

  end
