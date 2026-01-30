open C_compiler

let () =
  if Array.length Sys.argv < 3 then
    (Printf.printf "Format is %s [--lex | --parse | --codegen] filename.c"
       Sys.argv.(0); exit(1))
  else if String.equal Sys.argv.(1) "--lex" then
    let _ = Utils.lex_c Sys.argv.(2) in ()
  else if String.equal Sys.argv.(1) "--parse" then
    let _ = Utils.parse_c Sys.argv.(2) in ()
  else
    let prog = Utils.parse_c Sys.argv.(2) in 
    let source_filename = Sys.argv.(2) in
    let tacky_prog = Tacky.generate_tacky_program prog in
    if (String.equal Sys.argv.(1) "--compile") ||
         (String.equal Sys.argv.(1) "-S") then
      let _asm_filename =
        (String.sub source_filename 0
           ((String.length source_filename) - 2)) ^ ".s" in
        let asm_pass1 = Asm.generate_asm_program tacky_prog in
        let (stack_size, asm_pass2) = Asm.replace_pseudo_program asm_pass1 in
        let _asm_pass3 = Asm.fixup_program asm_pass2 stack_size in
      ()
    else
      ()

