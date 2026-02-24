type token_type =
    CONSTANT_INT of  int64
  | IDENTIFIER of string
  | INT
  | VOID
  | RETURN
  | IF
  | ELSE
  | GOTO
  | DO
  | WHILE
  | FOR
  | BREAK
  | CONTINUE
  | SWITCH
  | CASE
  | DEFAULT
  | STATIC
  | EXTERN
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | SEMI
  | MINUSMINUS
  | MINUS
  | TILDE
  | ASTERISK
  | SLASH
  | PLUS
  | PERCENT
  | LESSLESS
  | GREATERGREATER
  | AMPERSAND
  | PIPE
  | CARET
  | BANG
  | AMPAMP
  | PIPEPIPE
  | EQUALEQUAL
  | BANGEQUAL
  | LESS
  | GREATER
  | LESSEQUAL
  | GREATEREQUAL
  | EQUAL
  | PLUSPLUS
  | PLUSEQUAL
  | MINUSEQUAL
  | ASTERISKEQUAL
  | SLASHEQUAL
  | PERCENTEQUAL
  | PIPEEQUAL
  | AMPEQUAL
  | GREATERGREATEREQUAL
  | LESSLESSEQUAL
  | CARETEQUAL
  | QUESTION
  | COLON
  | COMMA
  | EOF

type lexer_type = { lexer_lines: string list;
    lexer_filename: string; lexer_file_line: int; lexer_pos: int }

let line_command_regex = Str.regexp "^# *\\([0-9]*\\) *\"\\([^\"]*\\)\".*$"

let int_regex = Str.regexp "[0-9]+\\b"

let identifier_regex = Str.regexp "[A-Za-z_][A-Za-z0-9_]*\\b"

let fail_at lexer  message =
  (Printf.printf "%s, line %d, column %d: %s\n"
     (Filename.basename lexer.lexer_filename)
     lexer.lexer_file_line (lexer.lexer_pos+1) message; exit 1)
  
let rec parse_line_command lexer =
  if lexer.lexer_lines == [] then
    lexer
else
  let line = List.hd lexer.lexer_lines in
  if Str.string_match line_command_regex line 0 then
    advance_line { lexer with lexer_filename=Str.matched_group 2 line;
        lexer_file_line=(int_of_string (Str.matched_group 1 line) - 1)}
  else
    advance_line lexer
and advance_line lexer =
  if lexer.lexer_lines == [] then
    lexer
  else
    let lines = List.tl lexer.lexer_lines in
    if lines == [] then
      { lexer with lexer_lines=[] }
    else
      let line = List.hd lines in
      if (String.length line) > 0 && line.[0] == '#' then
        parse_line_command { lexer with lexer_lines = lines;
          lexer_file_line=lexer.lexer_file_line+1; lexer_pos=0}
      else
        { lexer with lexer_lines = lines;
          lexer_file_line=lexer.lexer_file_line+1; lexer_pos=0}

let at_eol lexer =
  if lexer.lexer_lines == [] then
    true
  else
    lexer.lexer_pos >= String.length (List.hd lexer.lexer_lines)

let at_eof lexer = lexer.lexer_lines == []

let peek lexer =
  if lexer.lexer_lines == [] then
    None
  else if at_eol lexer then
    None
  else
    Some (List.hd lexer.lexer_lines).[lexer.lexer_pos]

let next lexer =
  if at_eol lexer then
    (None, lexer)
  else
    (Some ((List.hd lexer.lexer_lines).[lexer.lexer_pos]),
     { lexer with lexer_pos = lexer.lexer_pos + 1 })

let skip lexer =
  if at_eol lexer then
    lexer
  else
    { lexer with lexer_pos = lexer.lexer_pos + 1 }

let match_regex lexer regex =
  if at_eol lexer then
    (None, lexer)
  else if Str.string_match regex (List.hd lexer.lexer_lines) lexer.lexer_pos then
    (Some (Str.matched_string (List.hd lexer.lexer_lines)),
     { lexer with lexer_pos = Str.match_end () })
  else
    (None, lexer)
    
let location lexer =
  C_ast.Location (lexer.lexer_filename, lexer.lexer_file_line,
    lexer.lexer_pos + 1)

let load_file filename =
  In_channel.with_open_text filename (fun f ->
    In_channel.input_lines f)

let make_lexer filename =
  let lines = load_file filename in
  let lexer = { lexer_lines = lines; lexer_pos=0; lexer_file_line=1;
    lexer_filename=filename} in
  if not (List.is_empty lines) && (String.length (List.hd lines) > 0) &&
    (List.hd lines).[0] == '#' then
    parse_line_command lexer
  else
    lexer

let parse_integer lexer =
  match match_regex lexer int_regex with
  | (None, _) -> fail_at lexer "Invalid integer constant"
  | (Some str, lexer) ->
    (CONSTANT_INT (Int64.of_string str), lexer)

let make_identifier_token ident = 
  match ident with
  | "int" -> INT
  | "void" -> VOID
  | "return" -> RETURN
  | "if" -> IF
  | "else" -> ELSE
  | "goto" -> GOTO
  | "do" -> DO
  | "while" -> WHILE
  | "for" -> FOR
  | "break" -> BREAK
  | "continue" -> CONTINUE
  | "switch" -> SWITCH
  | "case" -> CASE
  | "default" -> DEFAULT
  | "static" -> STATIC
  | "extern" -> EXTERN
  | _ -> IDENTIFIER ident

let parse_identifier lexer =
  match match_regex lexer identifier_regex with
  | (None, _) -> fail_at lexer "Invalid identifier"
  | (Some str, lexer) -> (make_identifier_token str, lexer)

let rec skip_whitespace lexer =
  if at_eof lexer then
    lexer
  else if at_eol lexer then
    skip_whitespace (advance_line lexer)
  else
    let ch = Option.get (peek lexer) in
    if Char.Ascii.is_white ch then
      skip_whitespace (skip lexer)
    else
      lexer

let is_next1 lexer default_token next_ch next_token =
  let lexer = skip lexer in
  match peek lexer with
  | None -> (default_token, lexer)
  | Some ch -> if ch = next_ch then
                 (next_token, skip lexer)
               else
                 (default_token, lexer)

let is_next2 lexer default_token next_ch1 next_token1 next_ch2 next_token2 =
  let lexer = skip lexer in
  match peek lexer with
  | None -> (default_token, lexer)
  | Some ch -> if ch = next_ch1 then
                 (next_token1, skip lexer)
               else if ch = next_ch2 then
                 (next_token2, skip lexer)
               else
                 (default_token, lexer)

let is_next2_ext lexer default_token next_ch1 next_token1 next_ch2 next_token2
      ext_next_ch2 ext_next_token2 =
  let lexer = skip lexer in
  match peek lexer with
  | None -> (default_token, lexer)
  | Some ch -> if ch = next_ch1 then
                    (next_token1, skip lexer)
               else if ch = next_ch2 then
                 let lexer = skip lexer in
                 (match peek lexer with
                  | None -> (next_token2, lexer)
                  | Some ch ->
                     if ch = ext_next_ch2 then
                       (ext_next_token2, skip lexer)
                     else
                       (next_token2, lexer))
               else
                 (default_token, lexer)

let read_token lexer =
  match Option.get (peek lexer) with
  | ch when Char.Ascii.is_digit ch ->
     parse_integer lexer
  | ch when Char.Ascii.is_letter ch || ch == '_' ->
     parse_identifier lexer
  | '-' -> is_next2 lexer MINUS '-' MINUSMINUS '=' MINUSEQUAL
  | '+' -> is_next2 lexer PLUS '+' PLUSPLUS '=' PLUSEQUAL
  | '<' -> is_next2_ext lexer LESS '=' LESSEQUAL '<' LESSLESS '=' LESSLESSEQUAL
  | '>' -> is_next2_ext lexer GREATER '=' GREATEREQUAL '>' GREATERGREATER
             '=' GREATERGREATEREQUAL
  | '&' -> is_next2 lexer AMPERSAND '=' AMPEQUAL '&' AMPAMP
  | '^' -> is_next1 lexer CARET '=' CARETEQUAL
  | '|' -> is_next2 lexer PIPE '=' PIPEEQUAL '|' PIPEPIPE
  | '=' -> is_next1 lexer EQUAL '=' EQUALEQUAL
  | '!' -> is_next1 lexer BANG '=' BANGEQUAL
  | '*' -> is_next1 lexer ASTERISK '=' ASTERISKEQUAL
  | '/' -> is_next1 lexer SLASH '=' SLASHEQUAL
  | '%' -> is_next1 lexer PERCENT '=' PERCENTEQUAL
  | '(' -> (LPAREN, skip lexer)
  | ')' -> (RPAREN, skip lexer)
  | '{' -> (LBRACE, skip lexer)
  | '}' -> (RBRACE, skip lexer)
  | '~' -> (TILDE, skip lexer)
  | ';' -> (SEMI, skip lexer)
  | '?' -> (QUESTION, skip lexer)
  | ':' -> (COLON, skip lexer)
  | ',' -> (COMMA, skip lexer)
  | ch -> fail_at lexer (Printf.sprintf "Unexpected token '%c'\n" ch)

let next_token lexer =
  let lexer = skip_whitespace lexer in
  if at_eof lexer then
    None
  else
    let loc = location lexer in
    let (token, lexer) = read_token lexer in
    Some (token, loc, lexer)

let tokenize lexer =
  let rec tokenize1 lexer tokens =
    match next_token lexer with
    | None -> List.rev ((EOF, location lexer) :: tokens)
    | Some (token, loc, lexer) ->
       tokenize1 lexer ((token,loc)::tokens)
  in
  tokenize1 lexer []
