open C_compiler

type mode_type = ModeLex | ModeParse | ModeTacky | ModeCodegen | ModeEmitAsm

let parse_mode = function
  | "--lex" -> ModeLex
  | "--parse" -> ModeParse
  | "--tacky" -> ModeTacky
  | "--codegen" -> ModeCodegen
  | _ -> ModeEmitAsm

let () =
  if Array.length Sys.argv < 2 then
    (Printf.printf "Format is %s [--lex | --parse | --codegen] filename.c"
       Sys.argv.(0); exit(1))
  else
    let (mode,source_filename) =
      if Array.length Sys.argv == 2 then (ModeEmitAsm, Sys.argv.(1)) else
        (parse_mode Sys.argv.(1), Sys.argv.(2)) in
    let lexer = Lexer.make_lexer source_filename in
    let tokens = Lexer.tokenize lexer in
    if mode != ModeLex then
      let prog = Parser.parse_program tokens in
      if mode != ModeParse then
        let tacky_prog = Tacky.generate_tacky_program prog in
        if mode != ModeTacky then
          let asm_pass1 = Asm.generate_asm_program tacky_prog in
          let (stack_size, asm_pass2) = Asm.replace_pseudo_program asm_pass1 in
          let asm_pass3 = Asm.fixup_program asm_pass2 stack_size in
          let lines = Asm.emit_program asm_pass3 in
          if mode != ModeCodegen then
            let asm_filename =
              (String.sub source_filename 0
                 ((String.length source_filename) - 2)) ^ ".s" in            
            (Out_channel.with_open_text asm_filename (fun out_file ->
                 List.iter (output_string out_file) lines); ())
          else ()
        else ()
      else()
    else
      ()

