{
  open C_parser

  module StringMap = Map.Make(String)
  
  let make_int loc = INT (loc)
  let make_void loc = VOID (loc)
  let make_return loc = RETURN (loc)
  let keyword_map = StringMap.of_list [
  ("int", make_int); ("void", make_void); ("return", make_return);
  ]
  let identifier_or_keyword str filename line col =
    match StringMap.find_opt str keyword_map with
    | Some create_func -> create_func (Location (filename, line, col))
    | None -> IDENTIFIER (Location (filename, line, col), str)

  let string_to_int str = Int64.of_string str

  let linenum (lbuf:Lexing.lexbuf) = lbuf.lex_curr_p.pos_lnum

  let colnum (lbuf:Lexing.lexbuf) = lbuf.lex_curr_p.pos_cnum - lbuf.lex_curr_p.pos_bol

  let fname (lbuf:Lexing.lexbuf) = lbuf.lex_curr_p.pos_fname

}

rule token = parse
  | ['\n'] { Lexing.new_line lexbuf; token lexbuf }
  | "# " (['0'-'9']+ as lxnum) " " ['"']([^'"']* as fname) ['"'] [^'\n']*  ['\n']
  { Lexing.set_position lexbuf { pos_fname=fname; pos_lnum=int_of_string lxnum; pos_bol=0; pos_cnum=0}; token lexbuf}
  | [' ' '\t'] { token lexbuf }
  | ['A'-'Z' 'a'-'z' '_']['A'-'Z' 'a'-'z' '0'-'9' '_']* as lxm { identifier_or_keyword lxm (fname lexbuf) (linenum lexbuf) (colnum lexbuf) }
  | ['0'-'9']+ as lxm { CONSTANT_INT (Location (fname lexbuf, linenum lexbuf, colnum lexbuf), string_to_int lxm)  }
  | (['0'-'9']+ ['A'-'Z' 'a'-'z' '_']) as ident { Printf.eprintf "Invalid identifier %s - may not start with digits in %s at %d:%d\n" ident (fname lexbuf) (linenum lexbuf) (colnum lexbuf); exit 2 }
  | "(" { LPAREN (Location (fname lexbuf, linenum lexbuf, colnum lexbuf)) }
  | ")" { RPAREN (Location (fname lexbuf, linenum lexbuf, colnum lexbuf)) }
  | "{" { LBRACE (Location (fname lexbuf, linenum lexbuf, colnum lexbuf)) }
  | "}" { RBRACE (Location (fname lexbuf, linenum lexbuf, colnum lexbuf)) }
  | ";" { SEMI (Location (fname lexbuf, linenum lexbuf, colnum lexbuf)) }
  | _ as ch { Printf.eprintf "Invalid char %c in %s at %d:%d\n" ch (fname lexbuf) (linenum lexbuf) (colnum lexbuf); exit 2 }
  | eof { EOF }
