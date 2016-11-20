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

let x86regs_opnd = [eax; edx; ecx; ebx; esi; edi]

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
  | X86Leave
  | X86AddEsp of int

module StringSet = Set.Make (String)

class x86env =
    object(self)
        (* local variables for functions are true local variable which lie on stack *)
        (* local variables for main are global variables *)

        val    local_vars = ref StringSet.empty
        method local x    = local_vars := StringSet.add x !local_vars
        method local_vars = StringSet.elements !local_vars

        val    allocated  = ref 0
        method allocate n = allocated := max n !allocated
        method allocated  = !allocated
    end

let allocate env stack n_locals =
  match stack with
  | []                              -> R 2
  | (S n)::_                        -> env#allocate (abs(n + n_locals) + 1); S (n - 1)
  | (R n)::_ when n < num_of_regs-1 -> R (n+1)
  | _                               -> env#allocate 1; S (-n_locals - 1)

module Show =
  struct

    let slot = function
      | R i -> x86regs.(i)
      | S i -> Printf.sprintf "%d(%%ebp)" (i * word_size)
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
      | X86Call  s         -> Printf.sprintf "\tcall\t%s"        s
      | X86Jmp   lbl       -> Printf.sprintf "\tjmp\t%s"         lbl
      | X86Lbl   lbl       -> Printf.sprintf "%s:"               lbl
      | X86Movzbl (small_reg, reg) -> Printf.sprintf "\tmovzbl\t%%%s,\t%%%s" (small_reg) (reg)
      | X86Ret             -> "\tret"
      | X86Cdq             -> "\tcdq"
      | X86Leave           -> "\tleave"
      | X86AddEsp n        -> Printf.sprintf "\taddl\t$%d,\t%%esp" n

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

let save_reserved_regs a =
    [X86Push eax; 
     X86Push edx] @ 
     a @ 
    [X86Pop edx; 
     X86Pop eax]

let standard_compare x y cmp =
    [X86Xchg (eax, y); 
     X86Cmp (x, eax); 
     X86Set (Utils.Operation.cmp_to_name cmp, "al"); 
     X86Movzbl ("al", "eax"); 
     X86Xchg (eax, y)]

let standard_jmp_compare x y =
    ensure_one_reg x y @@ fun x y -> [X86Cmp (x, y)]

module Compile =
  struct

    open StackMachine

    let get_index x l =
        let rec get_index' s x l =
            match l with
            | [] -> failwith "No such element"
            | y::l' -> 
                if x = y
                then s
                else get_index' (s + 1) x l'
        in
        get_index' 0 x l

    let contains x l =
        List.exists (fun y -> y = x) l

    let get_slot x (name, (params, locals)) =
        if contains x params
        then S ((get_index x params) + 2) (* +2 -- skip return address *)
        else
            if contains x locals
            then S (-((get_index x locals) + 1))
            else M x

    let compile_function env code ((name, (params, locals)) as meta) full_meta =
      let rec compile stack code =
	match code with
	| []       -> []
	| i::code' ->
      let (stack', x86code) =
        match i with
        | S_READ   -> ([R 2], [X86Call "read"; X86Mov (eax, R 2)])
        
        | S_WRITE  -> ([], [X86Push (R 2); X86Call "write"; X86Pop (R 2)])
        
        | S_PUSH n ->
           let s = allocate env stack (List.length locals) in
           (s::stack, [X86Mov (L n, s)])
        
        | S_LD x   ->
            let x_slot = get_slot x meta in
            if not (contains x params)
            then env#local x;
            let s = allocate env stack (List.length locals) in
            (s::stack, ensure_one_reg x_slot s @@ fun x y -> [X86Mov (x, y)])
        
        | S_ST x   ->
            let x_slot = get_slot x meta in
            if not (contains x params)
            then env#local x;
            let s::stack' = stack in
            (stack', ensure_one_reg s x_slot @@ fun x y -> [X86Mov (x, y)])
        
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
           (y::stack', save_reserved_regs [X86Xor (eax, eax);   X86Mov (x, edx);   X86Cmp (edx, eax); 
                                           X86Set ("ne", "al"); X86Mov (y, edx);   X86Mul (eax, edx); 
                                           X86Xor (eax, eax);   X86Cmp (edx, eax); X86Set ("ne", "al"); X86Mov (eax, y)])
        
        | S_BINOP "!!"    ->
           let x::y::stack' = stack in
           (y::stack', save_reserved_regs [X86Xor (eax, eax); X86Mov (x, edx);     X86Or (y, edx); 
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

        | S_CALL name ->
            let push_args stack =
                let rec push_args' push_code stack params =
                    match params with
                    | [] -> (stack, push_code)
                    | p::params' ->
                        let s::stack' = stack in
                        push_args' ((X86Push s)::push_code) stack' params'
                in push_args' [] stack (fst (List.assoc name full_meta))
            in
            let (stack', push_code) = push_args stack in
            let (preserve_used_args, restore_used_args) = 
                match stack' with
                | []  -> ([], [])
                | (R 2)::_ -> ([X86Push ecx],                                                                            [X86Pop ecx])
                | (R 3)::_ -> ([X86Push ecx; X86Push ebx],                                                   [X86Pop ebx; X86Pop ecx])
                | (R 4)::_ -> ([X86Push ecx; X86Push ebx; X86Push esi],                          [X86Pop esi; X86Pop ebx; X86Pop ecx])
                | (S _)::_ -> ([X86Push ecx; X86Push ebx; X86Push esi; X86Push edi], [X86Pop edi; X86Pop esi; X86Pop ebx; X86Pop ecx])
            in
            let s = allocate env stack' (List.length locals) in
            let add = List.length (fst (List.assoc name full_meta)) in
            (s::stack', preserve_used_args @ push_code @ [X86Call name; X86Mov (eax, s); X86AddEsp (add * word_size)] @ restore_used_args)

        | S_RET ->
            let y::stack' = stack in
            (stack', [X86Mov (y, eax);
                      X86Leave;
                      X86Ret])

        | S_DROP ->
            let y::stack' = stack in
            (stack', [])

        (* skip it, we already have all meta info *)
        | S_PARAM x ->
            (stack, [])
	    in
	    x86code @ compile stack' code'
      in
      compile [] code

  end

open Language.Stmt

let rec print_meta funcs_meta =
    match funcs_meta with
    | [] -> ()
    | (name, (params, locals))::funcs_meta' ->
        Printf.eprintf "<%s>:\n" name;
        Printf.eprintf "  params:\n";
        List.iter (fun x -> Printf.eprintf "\t%s\n" x) params;
        Printf.eprintf "  locals:\n";
        List.iter (fun x -> Printf.eprintf "\t%s\n" x) locals;
        Printf.eprintf "\n\n";
        print_meta funcs_meta'

let compile (funcs, main) =
    let funcs_meta_info = Utils.Meta.get_meta [] (funcs @ [("main", ([], main))]) in

(* debug
    Printf.eprintf "printing meta data:\n\n";
    print_meta funcs_meta_info;
*)

    let asm  = Buffer.create 1024 in
    let (!!) s = Buffer.add_string asm s in
    let (!)  s = !!s; !!"\n" in
  
    !"\t.text";

(* get x86 codes for function and main *)
    let labler = new StackMachine.labler in
    let one_func_compiler (name, (params, stmt)) = 
        let env = new x86env in
        let func_code = Compile.compile_function env (StackMachine.Compile.stmt' labler stmt) (name, (List.assoc name funcs_meta_info)) funcs_meta_info in
        (name, (func_code, env))
    in
    let funcs = List.map one_func_compiler funcs in
    let main = one_func_compiler ("main", ([], main)) in

(* create global variables *)
    List.iter (fun x -> !(Printf.sprintf "\t.comm\t%s,\t%d,\t%d" x word_size word_size)) ((snd (snd main))#local_vars);
(* main should be global function *)
    !"\t.globl\tmain";

    let print_one_func (name, (func_x86_code, env)) = 
        let prologue, epilogue =
            (fun () ->
                !"\tpushl\t%ebp";
                !"\tmovl\t%esp,\t%ebp";
                if name <> "main"
                then !(Printf.sprintf "\tsubl\t$%d,\t%%esp" ((env#allocated + (List.length env#local_vars)) * word_size))
                else !(Printf.sprintf "\tsubl\t$%d,\t%%esp" (env#allocated * word_size))
            ),
            (fun () ->
                if name = "main"
                then
                    (!"\txorl\t%eax,\t%eax";
                     !"\tleave";
                     !"\tret\n")
                else
                    (!"") (* leave empty line after fun code *)
            )
        in
        !(Printf.sprintf "%s:" name);
        prologue();
        List.iter (fun i -> !(Show.instr i)) func_x86_code;
        epilogue();
    in
    List.iter print_one_func (funcs @ [main]);

    Buffer.contents asm


let build prog name =
(* debug 
    Printf.eprintf "%s.s file will look like this:\n\n" name;
    Printf.eprintf "%s" (compile prog);
    ()
*)
    let outf = open_out (Printf.sprintf "%s.s" name) in
    Printf.fprintf outf "%s" (compile prog);
    close_out outf;
    match Sys.command (Printf.sprintf "gcc -m32 -o %s $RC_RUNTIME/runtime.o %s.s" name name) with
    | 0 -> ()
    | _ -> failwith "gcc failed with non-zero exit code"
