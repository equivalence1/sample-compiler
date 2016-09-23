open Expr

(*
read (x);
read (y);
z := x * x;
write (z+y)
*)
let p =
  Seq (
      Read "x",
      Seq (
          Read "y",
          Seq (
              Assign ("z", Mul (Var "x", Var "x")),
              Write (Add (Var "z", Var "y"))
          )
      )
    )

(* let _ = *)
(*   let [r] = run [3; 4] p in *)
(*   Printf.printf "%d\n" r *)

let ( !! )       = (!)
let ( !  ) x     = Var x
let ( $  ) n     = Const n
let ( +  ) e1 e2 = Add (e1, e2)
let ( *  ) e1 e2 = Mul (e1, e2)
let ( -  ) e1 e2 = Sub (e1, e2)
let ( /  ) e1 e2 = Div (e1, e2)
let ( %  ) e1 e2 = Mod (e1, e2)
let ( <  ) e1 e2 = Lt  (e1, e2)
let ( >  ) e1 e2 = Gt  (e1, e2)
let ( <= ) e1 e2 = Le  (e1, e2)
let ( >= ) e1 e2 = Ge  (e1, e2)
let ( == ) e1 e2 = Eq  (e1, e2)
let ( != ) e1 e2 = Neq (e1, e2)
let ( && ) e1 e2 = And (e1, e2)
let ( || ) e1 e2 = Or  (e1, e2)

let skip     = Skip
let (:=) x e = Assign (x, e)
let read x   = Read x
let write x  = Write x
let (|>) l r = Seq (l, r)

(*
read (x);
read (y);
z := x * x;
write (z+y)
*)

let p =
  read "x" |>
  read "y" |>
  ("z" := !"x" * !"x") |>
  write (!"z" / !"y") |>
  write (!"z" % !"y") |>
  write (!"x" < !"y") |>
  write (!"x" > !"y") |>
  write (!"x" <= !"y") |>
  write (!"x" >= !"y") |>
  write (!"x" == !"y") |>
  write (!"x" != !"y") |>
  write (!"x" && !"y") |>
  write (!"x" || !"y")

(*
let _ =
  let [r] = run [3; 4] p in
  Printf.printf "%d\n" r

let run input p =
  srun input (compile_stmt p)

let _ =
  let [r] = run [3; 4] p in
  Printf.printf "%d\n" r
*)

let _ = build p "p"
