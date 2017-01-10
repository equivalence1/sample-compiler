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
    | MCall of t * string * t list
    | Field of t * string

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
      | %"new" c:IDENT "(" args:!(Util.list0 parse) ")" {
            New (c, args)
        }
(*
      | obj:IDENT "." m:IDENT args:(-"(" !(Util.list0 parse) -")")? {
            match args with
            | None -> Field (Var obj, m)
            | Some args -> MCall (Var (obj), m, args)
        }
      | f:IDENT args:(-"(" !(Util.list0 parse) -")")? {
          match args with 
          | None      -> Var f 
          | Some args -> Call (f, args)
        }
      | -"(" parse -")"
*)
      | chain
      | -"(" parse -")";

      chain: start:chain_start calls:( -"." f:IDENT args:(-"(" !(Util.list0 parse) -")")? )*
          {
              List.fold_left (fun acc (f, args) -> match args with None -> Field (acc, f) | Some args -> MCall (acc, f, args))
                              start
                              calls
          };

      chain_start: f:IDENT args:(-"(" !(Util.list0 parse) -")")?
          {
              match args with 
              | None      -> Var f 
              | Some args -> Call (f, args)
          }
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
    | MCall  of Expr.t * string * Expr.t list
    | FieldAssign of string * string * Expr.t

    ostap (
      parse: s:simple d:(-";" parse)? {match d with None -> s | Some d -> Seq (s, d)};
      expr : !(Expr.parse);
      simple:
       obj:IDENT "." x:IDENT ":=" e:expr {FieldAssign (obj, x, e)}
      | x:IDENT ":=" e:expr {Assign (x, e)}
      | f:IDENT "(" args:!(Util.list0 expr) ")" {Call (f, args)}
      | obj:IDENT "." m:IDENT "(" args:!(Util.list0 Expr.parse) ")" {
            MCall (Var (obj), m, args)
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

    type t = string * string * ReferenceDef.t list * Stmt.t

    ostap (
      arg  : !(ReferenceDef.parse);
      parse: %"fun" name:IDENT "(" args:!(Util.list0 arg) ")" ":" tp:IDENT %"begin" body:!(Stmt.parse) %"end" {
        (name, tp, args, body)
      }
    )

  end

module ConstructorDef =
  struct

    type t = string * string * ReferenceDef.t list * Stmt.t

    ostap (
      arg  : !(ReferenceDef.parse);
      parse: %"fun" %"init" "(" ")" %"begin" body:!(Stmt.parse) %"end" {
          ("init", "Constructor", [], body)
      }
    )

  end

module ClassDef =
    struct

        type t = string * string option * ReferenceDef.t list * FunDef.t list

        ostap (
            parse:
            %"class" name:IDENT ext:(-"extends" parent:IDENT)?
            %"begin"
            fields: (!(ReferenceDef.parse) -";")*
            constructor: (!(ConstructorDef.parse))?
            _methods: !(FunDef.parse)*
            %"end"
            {
                match constructor with
                | None -> failwith (Printf.sprintf "class %s should have constructor" name)
                | Some c -> 
                    let methods = List.map (fun (f_name, tp, args, body) -> (f_name, tp, (name, "self")::args, body)) (c::_methods) in
                    (name, ext, fields, methods)
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
