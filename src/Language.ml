open Ostap
open Matcher

module Expr =
  struct
    
    type t =
    | Const of int
    | Var   of string
    | BinOp of string * t * t
    | Call  of string * t list
    | New   of string * t list
    | MCall of string * string * t list

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
      | %"new" c:IDENT "(" args:!(Util.list0 parse) ")" {
            New (c, args)
        }
      | obj:IDENT "." m:IDENT "(" args:!(Util.list0 parse) ")" {
            MCall (obj, m, args)
        }
      | -"(" parse -")"
    )

  end

module ReferenceDef =
    struct

        type t = string * string

        ostap (
            parse: tp:IDENT x:IDENT {(tp, x)}
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
    | Ref    of string * string
    | MCall  of string * string * Expr.t list

    ostap (
      parse: s:simple d:(-";" parse)? {match d with None -> s | Some d -> Seq (s, d)};
      expr : !(Expr.parse);
      simple:
        x:IDENT s:(":=" e:expr {Assign (x, e)} | 
                   "(" args:!(Util.list0 expr) ")" {Call (x, args)}
                  ) {s}
      | obj:IDENT "." m:IDENT "(" args:!(Util.list0 Expr.parse) ")" {
            MCall (obj, m, args)
        }
      | rf:!(ReferenceDef.parse) {let (a, b) = rf in Ref (a, b)}
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

module FunDef =
  struct

    type t = string * (ReferenceDef.t list * Stmt.t)

    ostap (
      arg  : !(ReferenceDef.parse);
      parse: %"fun" name:IDENT "(" args:!(Util.list0 arg) ")" %"begin" body:!(Stmt.parse) %"end" {
        (name, (args, body))
      }
    )

  end

module ClassDef =
    struct

        type t = string * ReferenceDef.t list * FunDef.t list * string option

        ostap (
            parse:
            %"class" name:IDENT ext:(-"extends" parent:IDENT)?
            %"begin"
            fields: (!(ReferenceDef.parse) -";")*
            _methods: !(FunDef.parse)*
            %"end"
            {
                (name, fields, _methods, ext)
            }
        )

    end

module Unit =
  struct

    type t = ClassDef.t list * FunDef.t list * Stmt.t

    ostap (
      parse: !(ClassDef.parse)* !(FunDef.parse)* !(Stmt.parse)
    )

  end
