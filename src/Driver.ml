open Ostap

let parse infile =
  let s = Util.read infile in
  Util.parse
    (object
       inherit Matcher.t s
       inherit Util.Lexers.ident ["read"  ; "write"; "skip" ; "if"    ; "then";
                                  "elif"  ; "fi"   ; "while"; "do"    ; "od"  ;
                                  "repeat"; "until"; "for"  ; "return"; "fun" ;
                                  "begin" ; "end"                             ] s
       inherit Util.Lexers.decimal s
       inherit Util.Lexers.skip [
	 Matcher.Skip.whitespaces " \t\n";
	 Matcher.Skip.lineComment "--";
	 Matcher.Skip. nestedComment "(*" "*)"
       ] s
     end
    )
    (ostap (!(Language.Unit.parse) -EOF))

let main = ()
    try
        let mode, filename =
            match Sys.argv.(1) with
            | "-s" -> `SM , Sys.argv.(2)
            | "-o" -> `X86, Sys.argv.(2)
            | "-i" -> `Int, Sys.argv.(2)
            | _ -> raise (Invalid_argument "invalid flag")
        in
        match parse filename with
        | `Ok prog -> 
	        (
                match mode with
	            | `X86 ->
                    failwith "not supported" (*
                    let basename = Filename.chop_suffix filename ".expr" in 
	                X86.build prog basename *)
	            | `SM  -> 
                    let code = StackMachine.Prog.compile prog in
                    (* StackMachine.Interpreter.debug_print code; *)
                    StackMachine.Interpreter.run code
                | _    -> Interpreter.Prog.eval prog
	        )

        | `Fail er -> Printf.eprintf "%s\n" er
    with 
    | Invalid_argument _ ->
        Printf.printf "Usage: rc.byte <command> <name.expr>\n";
        Printf.printf "  <command> should be one of: -i, -s, -o\n"
