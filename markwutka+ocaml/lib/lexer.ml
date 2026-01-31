type token_type =
    CONSTANT_INT of  int64
  | IDENTIFIER of string
  | INT
  | VOID
  | RETURN
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
  | CARAT
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
  | _ -> IDENTIFIER ident

let parse_identifier lexer =
  match match_regex lexer identifier_regex with
  | (None, _) -> fail_at lexer "Invalid identifier"
  | (Some str, lexer) -> (str, lexer)

let tokenize lexer =
  let rec tokenize_1 lexer tokens =
    if at_eof lexer then
      List.rev ((EOF,location lexer):: tokens)
    else if at_eol lexer then
      tokenize_1 (advance_line lexer) tokens
    else
      let ch = peek lexer in
      let ch = Option.get ch in
      let loc = location lexer in
      if Char.Ascii.is_white ch then
        tokenize_1 (skip lexer) tokens
      else if Char.Ascii.is_digit ch then
        let (token, lexer) = parse_integer lexer in
        tokenize_1 lexer ((token,loc) :: tokens)
      else if (Char.Ascii.is_letter ch) || ch == '_' then
        let (ident, lexer) = parse_identifier lexer in
        tokenize_1 lexer ((make_identifier_token ident,loc) :: tokens)
      else if ch == '-' then
        let lexer = skip lexer in
        match peek lexer with
        | Some '-' -> tokenize_1 (skip lexer) ((MINUSMINUS,loc) :: tokens)
        | _ -> tokenize_1 lexer ((MINUS,loc) :: tokens)
      else if ch == '<' then
        let lexer = skip lexer in
        match peek lexer with
        | Some '<' -> tokenize_1 (skip lexer) ((LESSLESS,loc) :: tokens)
        | _ -> fail_at lexer "Invalid token <"
      else if ch == '>' then
        let lexer = skip lexer in
        match peek lexer with
        | Some '>' -> tokenize_1 (skip lexer) ((LESSLESS,loc) :: tokens)
        | _ -> fail_at lexer "Invalid token >"
      else if ch == '&' then
        tokenize_1 (skip lexer) ((AMPERSAND, loc) :: tokens)        
      else if ch == '^' then
        tokenize_1 (skip lexer) ((CARAT, loc) :: tokens)        
      else if ch == '|' then
        tokenize_1 (skip lexer) ((PIPE, loc) :: tokens)        
      else if ch == '(' then
        tokenize_1 (skip lexer) ((LPAREN, loc) :: tokens)
      else if ch == ')' then
        tokenize_1 (skip lexer) ((RPAREN, loc) :: tokens)
      else if ch == '{' then
        tokenize_1 (skip lexer) ((LBRACE, loc) :: tokens)
      else if ch == '}' then
        tokenize_1 (skip lexer) ((RBRACE, loc) :: tokens)
      else if ch == '~' then
        tokenize_1 (skip lexer) ((TILDE, loc) :: tokens)
      else if ch == ';' then
        tokenize_1 (skip lexer) ((SEMI, loc) :: tokens)
      else if ch == '*' then
        tokenize_1 (skip lexer) ((ASTERISK, loc) :: tokens)
      else if ch == '/' then
        tokenize_1 (skip lexer) ((SLASH, loc) :: tokens)
      else if ch == '+' then
        tokenize_1 (skip lexer) ((PLUS, loc) :: tokens)
      else if ch == '%' then
        tokenize_1 (skip lexer) ((PERCENT, loc) :: tokens)
      else
        (Printf.printf "%s, line %d, column %d: Unexpected token %c\n"
           (Filename.basename lexer.lexer_filename)
           lexer.lexer_file_line (lexer.lexer_pos+1)
         ch;
         exit 1)
    in
      tokenize_1 lexer []
        
