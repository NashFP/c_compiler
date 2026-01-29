open C_compiler

let lex_c filename =
  In_channel.with_open_text filename
    (fun file ->
      let lexbuf = Lexing.from_channel file in
      Lexing.set_filename lexbuf filename;
      try
        let rec loop () =
          if C_lexer.token lexbuf == C_parser.EOF then
            None
          else
            loop () in
          loop ()
      with _ ->
        (Printf.printf "Error in %s on line %d:%d\n"
           lexbuf.lex_curr_p.pos_fname
           lexbuf.lex_curr_p.pos_lnum
          (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol));
        failwith "parse error")

let parse_c filename =
  In_channel.with_open_text filename
    (fun file ->
      let lexbuf = Lexing.from_channel file in
      Lexing.set_filename lexbuf filename;
      try
        (C_parser.program C_lexer.token lexbuf)
      with _ ->
        (Printf.printf "Error in %s on line %d:%d\n"
           lexbuf.lex_curr_p.pos_fname
           lexbuf.lex_curr_p.pos_lnum
          (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol));
        failwith "parse error")

let () =
  if Array.length Sys.argv < 3 then
    (Printf.printf "Format is %s [--lex | --parse | --codegen] filename.c"
       Sys.argv.(0); exit(1))
  else if String.equal Sys.argv.(1) "--lex" then
    let _ = lex_c Sys.argv.(2) in ()
  else if String.equal Sys.argv.(1) "--parse" then
    let _ = parse_c Sys.argv.(2) in ()
  else
    let prog = parse_c Sys.argv.(2) in 
    let source_filename = Sys.argv.(2) in
    let _asdl = Tacky.generate_tacky_program prog in
    if (String.equal Sys.argv.(1) "--compile") ||
         (String.equal Sys.argv.(1) "-S") then
      let _asm_filename =
        (String.sub source_filename 0
           ((String.length source_filename) - 2)) ^ ".s" in
      ()
    else
      ()

