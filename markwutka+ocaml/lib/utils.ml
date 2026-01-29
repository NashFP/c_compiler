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
