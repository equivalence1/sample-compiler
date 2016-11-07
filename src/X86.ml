type opnd = R of int | S of int | M of string | L of int

let x86regs = [|
  "%eax"; 
  "%edx"; 
  "%ecx"; 
  "%ebx"; 
  "%esi"; 
  "%edi"
|]

let num_of_regs = Array.length x86regs
let word_size = 4

let eax = R 0
let edx = R 1
let ecx = R 2
let ebx = R 3
let esi = R 4
let edi = R 5

type x86instr =
  | X86Add    of opnd * opnd
  | X86Mul    of opnd * opnd
  | X86Sub    of opnd * opnd
  | X86Div    of opnd
  | X86Mod    of opnd * opnd
  | X86Cmp    of opnd * opnd
  | X86Xor    of opnd * opnd
  | X86Or     of opnd * opnd
  | X86And    of opnd * opnd
  | X86Mov    of opnd * opnd
  | X86Xchg   of opnd * opnd
  | X86Push   of opnd
  | X86Pop    of opnd
  | X86Set    of string * string
  | X86Jmpc   of string * string
  | X86Movzbl of string * string
  | X86Call   of string
  | X86Jmp    of string
  | X86Lbl    of string
  | X86Cdq
  | X86Ret

module S = Set.Make (String)

class x86env =
  object(self)
    val    local_vars = ref S.empty
    method local x    = local_vars := S.add x !local_vars
    method local_vars = S.elements !local_vars

    val    allocated  = ref 0
    method allocate n = allocated := max n !allocated
    method allocated  = !allocated
  end

let allocate env stack =
  match stack with
  | []                              -> R 2
  | (S n)::_                        -> env#allocate (n+2); S (n+1)
  | (R n)::_ when n < num_of_regs-1 -> R (n+1)
  | _                               -> env#allocate (1); S 0

module Show =
  struct

    let slot = function
      | R i -> x86regs.(i)
      | S i -> Printf.sprintf "-%d(%%ebp)" (i * word_size)
      | M x -> x
      | L i -> Printf.sprintf "$%d" i

    let instr = function
      | X86Add  (s1, s2)   -> Printf.sprintf "\taddl\t%s,\t%s"  (slot s1) (slot s2)
      | X86Sub  (s1, s2)   -> Printf.sprintf "\tsubl\t%s,\t%s"  (slot s1) (slot s2)
      | X86Mul  (s1, s2)   -> Printf.sprintf "\timull\t%s,\t%s" (slot s1) (slot s2)
      | X86Mov  (s1, s2)   -> Printf.sprintf "\tmovl\t%s,\t%s"  (slot s1) (slot s2)
      | X86Cmp  (s1, s2)   -> Printf.sprintf "\tcmpl\t%s,\t%s"  (slot s1) (slot s2)
      | X86Xor  (s1, s2)   -> Printf.sprintf "\txorl\t%s,\t%s"  (slot s1) (slot s2)
      | X86Xchg (s1, s2)   -> Printf.sprintf "\txchg\t%s,\t%s"  (slot s1) (slot s2)
      | X86Or   (s1, s2)   -> Printf.sprintf "\torl\t%s,\t%s"   (slot s1) (slot s2)
      | X86And  (s1, s2)   -> Printf.sprintf "\tandl\t%s,\t%s"  (slot s1) (slot s2)
      | X86Set  (cmp, reg) -> Printf.sprintf "\tset%s\t%%%s"    (cmp    ) (reg    )
      | X86Jmpc (cmp, lbl) -> Printf.sprintf "\tj%s\t%s"        (cmp    ) (lbl    )
      | X86Div  (s1)       -> Printf.sprintf "\tidivl\t%s"      (slot s1)
      | X86Push  s         -> Printf.sprintf "\tpushl\t%s"      (slot s)
      | X86Pop   s         -> Printf.sprintf "\tpopl\t%s"       (slot s)
      | X86Call  p         -> Printf.sprintf "\tcall\t%s"        p
      | X86Jmp   lbl       -> Printf.sprintf "\tjmp\t%s"         lbl
      | X86Lbl   lbl       -> Printf.sprintf "%s:"               lbl
      | X86Movzbl (small_reg, reg) -> Printf.sprintf "\tmovzbl\t%%%s,\t%%%s" (small_reg) (reg)
      | X86Ret             -> "\tret"
      | X86Cdq             -> "\tcdq"

  end

(* 
 * result of f should be in y.
 * result of these functions will be in y as well
 *)

let ensure_first_reg x y f =
    match x with
    | R _ -> f x y
    | L _ -> f x y
    | _   -> [X86Xchg (x, eax)] @ (f eax y) @ [X86Xchg (eax, x)]

let ensure_second_reg x y f =
    match y with
    | R _ -> f x y
    | _   -> [X86Xchg (y, edx)] @ (f x edx) @ [X86Xchg (edx, y)]

let ensure_one_reg x y f =
    match (x, y) with
    | (_, R _) -> f x y
    | (_, L _) -> f x y
    | (_,   _) -> ensure_first_reg x y f

let save_regs a =
    [X86Push eax; X86Push edx] @ a @ [X86Pop edx; X86Pop eax]

let standard_compare x y cmp =
    [X86Xchg (eax, y); X86Cmp (x, eax); X86Set (Utils.Operation.cmp_to_name cmp, "al"); X86Movzbl ("al", "eax"); X86Xchg (eax, y)]

let standard_jmp_compare x y =
    ensure_one_reg x y @@ fun x y -> [X86Cmp (x, y)]

module Compile =
  struct

    open StackMachine

    let stack_program env code =
      let rec compile stack code =
	match code with
	| []       -> []
	| i::code' ->
      let (stack', x86code) =
        match i with
        | S_READ   -> ([R 2], [X86Call "read"; X86Mov (eax, R 2)])
        
        | S_WRITE  -> ([], [X86Push (R 2); X86Call "write"; X86Pop (R 2)])
        
        | S_PUSH n ->
           let s = allocate env stack in
           (s::stack, [X86Mov (L n, s)])
        
        | S_LD x   ->
           env#local x;
           let s = allocate env stack in
           (s::stack, ensure_one_reg (M x) s @@ fun x y -> [X86Mov (x, y)])
        
        | S_ASSIGN (x, n)   ->
           env#local x;
           (stack, [X86Mov (L n, M x)])
        
        | S_ST x   ->
           env#local x;
           let s::stack' = stack in
           (stack', [X86Mov (s, M x)])
        
        | S_BINOP "+"     ->
           let x::y::stack' = stack in
           (y::stack', ensure_one_reg x y @@ fun x y -> [X86Add (x, y)])
        
        | S_BINOP "-"     ->
           let x::y::stack' = stack in  (* we need y - x *)
           (y::stack', ensure_one_reg x y @@ fun x y -> [X86Sub (x, y)])
        
        | S_BINOP "*"     ->
           let x::y::stack' = stack in
           (y::stack', ensure_second_reg x y @@ fun x y -> [X86Mul (x, y)])
        
        | S_BINOP "/"     ->
           let x::y::stack' = stack in  (* x -- divider, y -- dividend *)
           (y::stack', [X86Xchg (y, eax); X86Cdq; X86Div (x); X86Xchg (eax, y)])
        
        | S_BINOP "%"     ->
           let x::y::stack' = stack in
           (y::stack', [X86Xchg (y, eax); X86Cdq; X86Div (x); X86Xchg (eax, y); X86Mov (edx, y)])
        
        | S_BINOP "&&"    ->
           let x::y::stack' = stack in
           (y::stack', save_regs [X86Xor (eax, eax);   X86Mov (x, edx);   X86Cmp (edx, eax); 
                                  X86Set ("ne", "al"); X86Mov (y, edx);   X86Mul (eax, edx); 
                                  X86Xor (eax, eax);   X86Cmp (edx, eax); X86Set ("ne", "al"); X86Mov (eax, y)])
        
        | S_BINOP "!!"    ->
           let x::y::stack' = stack in
           (y::stack', save_regs [X86Xor (eax, eax); X86Mov (x, edx);     X86Or (y, edx); 
                                  X86Cmp (edx, eax); X86Set ("ne", "al"); X86Mov (eax, y)])
        
        | S_BINOP cmp     ->
           let x::y::stack' = stack in
           (y::stack', standard_compare x y cmp)
        
        | S_LABLE l       ->
           (stack, [X86Lbl l])
        
        | S_JMP l         ->
           (stack, [X86Jmp l])
        
        | S_JMPC (cmp, l)  ->
           let x::stack' = stack in
           (stack', [X86Cmp (L 0, x); X86Jmpc (cmp, l)])
	    in
	    x86code @ compile stack' code'
      in
      compile [] code

  end

let compile stmt =
  let env = new x86env in
  let code = Compile.stack_program env @@ StackMachine.Compile.stmt stmt in
  let asm  = Buffer.create 1024 in
  let (!!) s = Buffer.add_string asm s in
  let (!)  s = !!s; !!"\n" in
  !"\t.text";
  List.iter (fun x ->
      !(Printf.sprintf "\t.comm\t%s,\t%d,\t%d" x word_size word_size))
    env#local_vars;
  !"\t.globl\tmain";
  let prologue, epilogue =
    if env#allocated = 0
    then (fun () -> ()), (fun () -> ())
    else
      (fun () ->
         !"\tpushl\t%ebp";
         !"\tmovl\t%esp,\t%ebp";
         !(Printf.sprintf "\tsubl\t$%d,\t%%esp" (env#allocated * word_size))
      ),
      (fun () ->
         !"\tmovl\t%ebp,\t%esp";
         !"\tpopl\t%ebp"
      )
  in
  !"main:";
  prologue();
  List.iter (fun i -> !(Show.instr i)) code;
  epilogue();
  !"\txorl\t%eax,\t%eax";
  !"\tret";
  Buffer.contents asm

let build stmt name =
  let outf = open_out (Printf.sprintf "%s.s" name) in
  Printf.fprintf outf "%s" (compile stmt);
  close_out outf;
  match Sys.command (Printf.sprintf "gcc -m32 -o %s $RC_RUNTIME/runtime.o %s.s" name name) with
  | 0 -> ()
  | _ -> failwith "gcc failed with non-zero exit code"
