open C_ast
open Lexer
open Context

let str_of_token = function
  | CONSTANT_INT _ -> Printf.sprintf "int constant"
  | IDENTIFIER _ -> Printf.sprintf "identifier"
  | INT -> "int"
  | VOID -> "void"
  | RETURN -> "return"
  | IF -> "if"
  | ELSE -> "else"
  | GOTO -> "goto"
  | DO -> "do"
  | WHILE -> "while"
  | FOR -> "for"
  | BREAK -> "break"
  | CONTINUE -> "continue"
  | SWITCH -> "switch"
  | CASE -> "case"
  | DEFAULT -> "default"
  | LPAREN -> "("
  | RPAREN -> ")"
  | LBRACE -> "{"
  | RBRACE -> "}"
  | SEMI -> ";"
  | MINUSMINUS -> "--"
  | MINUS -> "-"
  | TILDE -> "~"
  | ASTERISK -> "*"
  | SLASH -> "/"
  | PLUS -> "+"
  | PERCENT -> "%"
  | LESSLESS -> "<<"
  | GREATERGREATER -> ">>"
  | AMPERSAND -> "&"
  | CARAT -> "^"
  | PIPE -> "|"
  | EOF -> "end of file"
  | BANG -> "!"
  | BANGEQUAL -> "!="
  | AMPAMP -> "&&"
  | PIPEPIPE -> "||"
  | EQUALEQUAL -> "=="
  | LESS -> "<"
  | LESSEQUAL -> "<="
  | GREATER -> ">"
  | GREATEREQUAL -> ">="
  | EQUAL -> "="
  | PLUSPLUS -> "++"
  | PLUSEQUAL -> "+="
  | MINUSEQUAL -> "-="
  | ASTERISKEQUAL -> "*="
  | SLASHEQUAL -> "/="
  | PERCENTEQUAL -> "%="
  | PIPEEQUAL -> "|="
  | AMPEQUAL -> "&="
  | CARATEQUAL -> "^="
  | LESSLESSEQUAL -> "<<="
  | GREATERGREATEREQUAL -> ">>="
  | QUESTION -> "?"
  | COLON -> ":"

let ident_str = function
  | IDENTIFIER str -> str
  | tok -> (Printf.printf "Tried to fetch identifier name of token %s\n"
              (str_of_token tok);
          exit 2)

let constant_int_val = function
  | CONSTANT_INT i -> i
  | tok -> (Printf.printf "Tried to fetch constant int value of token %s\n"
              (str_of_token tok);
          exit 2)

let same_token t1 t2 =
  match (t1, t2) with
  | (CONSTANT_INT _, CONSTANT_INT _) -> true
  | (IDENTIFIER _, IDENTIFIER _) -> true
  | _ -> t1 == t2

let expect_and_get expected tokens =
  match tokens with
  | [] -> (Printf.printf "Expected %s, but found end of file\n"
             (str_of_token expected);
          exit 1)
  | (tok, loc) :: tokens_rest ->
     if same_token expected tok then
       (tok, loc, tokens_rest)
     else
       fail_at loc (Printf.sprintf "Expected %s but found %s\n"
          (str_of_token expected) (str_of_token tok))

let expect_identifier tokens =
  match tokens with
  | [] -> (Printf.printf "Expected identifier, but found end of file\n";
           exit 1)
  | ((IDENTIFIER str), loc) :: tokens -> (str, loc, tokens)
  | (other, loc) :: _ ->
     fail_at loc (Printf.sprintf "Expected identifier but found %s"
                  (str_of_token other))
  
let expect expected tokens =
  let (_, _, tokens) = expect_and_get expected tokens in
  tokens

let peek = function
  | [] -> (Printf.printf "Unexpected end of file\n"; exit 1)
  | (tok :: tokens) -> (tok, tokens)

let is_binop = function
  | MINUS -> true
  | PLUS -> true
  | ASTERISK -> true
  | SLASH -> true
  | PERCENT -> true
  | LESSLESS -> true
  | GREATERGREATER -> true
  | AMPERSAND -> true
  | CARAT -> true
  | PIPE -> true
  | AMPAMP -> true
  | PIPEPIPE -> true
  | EQUALEQUAL -> true
  | BANGEQUAL -> true
  | LESS -> true
  | LESSEQUAL -> true
  | GREATER -> true
  | GREATEREQUAL -> true
  | QUESTION -> true
  | _ -> false

let binop_precedence = function
  | ASTERISK -> 50
  | SLASH -> 50
  | PERCENT -> 50
  | PLUS -> 45
  | MINUS -> 45
  | LESSLESS -> 40
  | GREATERGREATER -> 40
  | LESS -> 35
  | LESSEQUAL -> 35
  | GREATER -> 35
  | GREATEREQUAL -> 35
  | EQUALEQUAL -> 30
  | BANGEQUAL -> 30
  | AMPERSAND -> 25
  | CARAT -> 20
  | PIPE -> 15
  | AMPAMP -> 10
  | PIPEPIPE -> 5
  | QUESTION -> 3
  | EQUAL -> 2
  | PLUSEQUAL -> 2
  | MINUSEQUAL -> 2
  | ASTERISKEQUAL -> 2
  | SLASHEQUAL -> 2
  | PERCENTEQUAL -> 2
  | LESSLESSEQUAL -> 2
  | GREATERGREATEREQUAL -> 2
  | AMPEQUAL -> 2
  | CARATEQUAL -> 2
  | PIPEEQUAL -> 2
  | _ -> 99

let is_assignment_op = function
  | EQUAL -> true
  | PLUSEQUAL -> true
  | MINUSEQUAL -> true
  | ASTERISKEQUAL -> true
  | SLASHEQUAL -> true
  | PERCENTEQUAL -> true
  | LESSLESSEQUAL -> true
  | GREATERGREATEREQUAL -> true
  | AMPEQUAL -> true
  | CARATEQUAL -> true
  | PIPEEQUAL -> true
  | _ -> false

let is_compound_op = function
  | PLUSEQUAL -> true
  | MINUSEQUAL -> true
  | ASTERISKEQUAL -> true
  | SLASHEQUAL -> true
  | PERCENTEQUAL -> true
  | LESSLESSEQUAL -> true
  | GREATERGREATEREQUAL -> true
  | AMPEQUAL -> true
  | CARATEQUAL -> true
  | PIPEEQUAL -> true
  | _ -> false

let compound_calc_op = function
  | PLUSEQUAL -> Add
  | MINUSEQUAL -> Subtract
  | ASTERISKEQUAL -> Multiply
  | SLASHEQUAL -> Divide
  | PERCENTEQUAL -> Remainder
  | LESSLESSEQUAL -> ShiftLeft
  | GREATERGREATEREQUAL -> ShiftRight
  | AMPEQUAL -> BitwiseAnd
  | CARATEQUAL -> BitwiseXor
  | PIPEEQUAL -> BitwiseOr
  | _ -> failwith "Tried to compute compound calc op for non compound op"
 

let is_unop = function
  | MINUS -> true
  | TILDE -> true
  | BANG -> true
  | PLUSPLUS -> true
  | MINUSMINUS -> true
  | _ -> false
    
let parse_unop tokens =
  let ((tok,loc), next_tokens) = peek tokens in
  match tok with
  | MINUS -> (Negate, next_tokens)
  | TILDE -> (Complement, next_tokens)
  | BANG -> (Not, next_tokens)
  | PLUSPLUS -> (PreInc, next_tokens)
  | MINUSMINUS -> (PreDec, next_tokens)
  | _ -> fail_at loc (Printf.sprintf "Unexpected unary operator %s"
                        (str_of_token tok))

let parse_binop tokens =
  let ((tok,loc), next_tokens) = peek tokens in
  match tok with
  | MINUS -> (Subtract, next_tokens)
  | PLUS -> (Add, next_tokens)
  | ASTERISK -> (Multiply, next_tokens)
  | SLASH -> (Divide, next_tokens)
  | PERCENT -> (Remainder, next_tokens)
  | LESSLESS -> (ShiftLeft, next_tokens)
  | GREATERGREATER -> (ShiftRight, next_tokens)
  | AMPERSAND -> (BitwiseAnd, next_tokens)
  | CARAT -> (BitwiseXor, next_tokens)
  | PIPE -> (BitwiseOr, next_tokens)
  | LESS -> (LessThan, next_tokens)
  | LESSEQUAL -> (LessOrEqual, next_tokens)
  | GREATER -> (GreaterThan, next_tokens)
  | GREATEREQUAL -> (GreaterOrEqual, next_tokens)
  | EQUALEQUAL -> (Equal, next_tokens)
  | BANGEQUAL -> (NotEqual, next_tokens)
  | AMPAMP -> (And, next_tokens)
  | PIPEPIPE -> (Or, next_tokens)
  | _ -> fail_at loc
           (Printf.sprintf "Unexpected binary operator %s"
              (str_of_token tok))

let rec parse_factor tokens =
  let (factor_exp, tokens) =
    let ((tok,loc), next_tokens) = peek tokens in
    if same_token tok (CONSTANT_INT 0L) then
      (ConstantInt (loc, constant_int_val tok), next_tokens)
    else if same_token tok (IDENTIFIER "") then
      (Var (loc, ident_str tok), next_tokens)
    else if is_unop tok then
      let (unop, tokens) = parse_unop tokens in
      let (inner_expr, tokens) = parse_factor tokens in
      (Unary (loc, unop, inner_expr), tokens)
    else if same_token tok LPAREN then
      let (inner_expr, tokens) = parse_expr next_tokens 0 in
      let tokens = expect RPAREN tokens in
      (inner_expr, tokens)
    else
      fail_at loc (Printf.sprintf "Unexpected token %s" (str_of_token tok))
  in
  let rec check_for_postfix factor_exp tokens =
    let ((next_tok, loc), post_next_tokens) = peek tokens in
    match next_tok with
    | PLUSPLUS ->
       check_for_postfix (Unary (loc, PostInc, factor_exp)) post_next_tokens
    | MINUSMINUS ->
       check_for_postfix (Unary (loc, PostDec, factor_exp)) post_next_tokens
    | _ -> (factor_exp, tokens)
  in
  check_for_postfix factor_exp tokens  

and parse_expr tokens min_prec =
  let ((_,loc),_) = peek tokens in
  let (left, tokens) = parse_factor tokens in
  let rec parse_expr1 loc curr_left tokens min_prec =
    let ((tok,_), next_tokens) = peek tokens in
    if (is_assignment_op tok) && (binop_precedence tok) >= min_prec then
      let (right, tokens) = parse_expr next_tokens 2 in
      if is_compound_op tok then
        parse_expr1 loc
          (AssignmentExpr (loc, curr_left,
                       Binary (loc, compound_calc_op tok, curr_left, right)))
          tokens min_prec
      else
        parse_expr1 loc (Assignment (loc, curr_left, right)) tokens min_prec
    else if (is_binop tok) && (binop_precedence tok) >= min_prec then
      match tok with
      | QUESTION ->
         let (right, tokens) =
           parse_expr next_tokens 0 in
         let tokens = expect COLON tokens in
         let (false_expr, tokens) =
           parse_expr tokens (binop_precedence tok) in
         parse_expr1 loc (Condition (loc, curr_left, right, false_expr))
           tokens min_prec
      | _ ->
         let (operator, tokens) = parse_binop tokens in
         let (right, tokens) =
           parse_expr tokens ((binop_precedence tok) + 1) in
         parse_expr1 loc (Binary (loc, operator, curr_left, right))
           tokens min_prec
    else
      (curr_left, tokens)
  in
  parse_expr1 loc left tokens min_prec

let parse_optional_expr tokens terminator =
  let ((tok,_),_) = peek tokens in
  if same_token tok terminator then
    let tokens = expect terminator tokens in
    (None, tokens)
  else
    let (expr, tokens) = parse_expr tokens 0 in
    let tokens = expect terminator tokens in
    (Some expr, tokens)

let rec parse_statement tokens =
  match tokens with
  | [] -> failwith "Expected statement, found end of file"
  | ((IDENTIFIER str, loc) :: (COLON, _) :: tokens) ->
     let (stmt, tokens) = parse_statement tokens in
     let new_label = Label (loc, str, stmt) in
     (new_label, tokens)
  | ((GOTO,loc) :: tokens) ->
     let (tok,_, tokens) = expect_and_get (IDENTIFIER "") tokens in
     let tokens = expect SEMI tokens in
     (Goto (loc, ident_str tok), tokens)
  | ((RETURN,loc) :: tokens) ->
     let (expr, tokens) = parse_expr tokens 0 in
     let tokens = expect SEMI tokens in
     (Return (loc, expr), tokens)
  | ((IF,loc) :: tokens) ->
     let tokens = expect LPAREN tokens in
     let (test_expr, tokens) = parse_expr tokens 0 in
     let tokens = expect RPAREN tokens in
     let (then_stmt, tokens) = parse_statement tokens in
     (match peek tokens with
      | ((ELSE, _),tokens) ->
         let (else_stmt, tokens) = parse_statement tokens in
         (If (loc, test_expr, then_stmt, Some else_stmt), tokens)
      | _ -> (If (loc, test_expr, then_stmt, None), tokens))
  | ((WHILE,loc) :: tokens) ->
     let tokens = expect LPAREN tokens in
     let (test_expr, tokens) = parse_expr tokens 0 in
     let tokens = expect RPAREN tokens in
     let (stmt, tokens) = parse_statement tokens in
     (While (loc,test_expr,stmt,None), tokens)
  | ((DO,loc) :: tokens) ->
     let (stmt, tokens) = parse_statement tokens in
     let tokens = expect WHILE tokens in
     let tokens = expect LPAREN tokens in
     let (test_expr, tokens) = parse_expr tokens 0 in
     let tokens = expect RPAREN tokens in
     let tokens = expect SEMI tokens in
     (DoWhile (loc,test_expr,stmt,None), tokens)
  | ((FOR,loc) :: tokens) ->
     let tokens = expect LPAREN tokens in
     let (init,tokens) = parse_for_init tokens in
     let (test_expr, tokens) = parse_optional_expr tokens SEMI in
     let (post_expr, tokens) = parse_optional_expr tokens RPAREN in
     let (stmt, tokens) = parse_statement tokens in
     (For (loc,init,test_expr,post_expr,stmt,None), tokens)
  | ((BREAK,loc) :: tokens) ->
     let tokens = expect SEMI tokens in
     (Break (loc,None), tokens)
  | ((CONTINUE,loc) :: tokens) ->
     let tokens = expect SEMI tokens in
     (Continue (loc,None), tokens)
  | ((LBRACE,loc) :: tokens) ->
     let (block_items, tokens) = parse_block_items tokens in
     let tokens = expect RBRACE tokens in
     (Compound (loc, Block block_items), tokens)
  | ((SWITCH,loc) :: tokens) ->
    let tokens = expect LPAREN tokens in
    let (switch_expr, tokens) = parse_expr tokens 0 in
    let tokens = expect RPAREN tokens in
    let (stmt, tokens) = parse_statement tokens in
    (Switch (loc, switch_expr, stmt, None), tokens)
  | ((CASE,loc) :: tokens) ->
    let (case_expr, tokens) = parse_expr tokens 0 in
    let tokens = expect COLON tokens in
    (Case (loc, case_expr, None), tokens)
  | ((DEFAULT,loc) :: tokens) ->
    let tokens = expect COLON tokens in
    (Default (loc, None), tokens)
  | ((SEMI,_loc) :: tokens) -> (Null, tokens)
  | ((_,loc) :: _tokens) -> let (expr, tokens) = parse_expr tokens 0 in
                            let tokens = expect SEMI tokens in
                            (Expression (loc, expr), tokens)

and parse_declaration tokens =
  let (_, loc, tokens) = expect_and_get INT tokens in
  let (var_name, _, tokens) = expect_identifier tokens in
  match peek tokens with
  | ((EQUAL,_), tokens) ->
     let (init_exp, tokens) = parse_expr tokens 0 in
     let tokens = expect SEMI tokens in
     (Declaration (loc, var_name, Some init_exp), tokens)
  | ((SEMI,_), tokens) ->
     (Declaration (loc, var_name, None), tokens)
  | ((other,loc), _) ->
     fail_at loc
       (Printf.sprintf "Invalid declaration, expected ';', but found %s"
          (str_of_token other))
and is_declaration tokens =
  match peek tokens with
  | ((INT,_),_) -> true
  | _ -> false
and parse_block_items tokens =
  let rec parse_block_items_1 tokens items =
    if is_declaration tokens then
       let (decl, tokens) = parse_declaration tokens in
       parse_block_items_1 tokens (D decl :: items)
    else
      match tokens with
      | ((SEMI,_) :: tokens) -> parse_block_items_1 tokens items
      | ((RBRACE,_) :: _) -> (List.rev items, tokens)
      | _ ->
         let (stmt, tokens) = parse_statement tokens in
         parse_block_items_1 tokens (S stmt :: items)
  in
  parse_block_items_1 tokens []

and parse_for_init tokens =
  if is_declaration tokens then
    let (decl, tokens) = parse_declaration tokens in
    (InitDecl decl, tokens)
  else
    let (expr, tokens) = parse_optional_expr tokens SEMI in
    (InitExpr expr, tokens)
let parse_function tokens =
  let (_, loc, tokens) = expect_and_get INT tokens in
  let (ident, _, tokens) = expect_and_get (IDENTIFIER "") tokens in
  let tokens = expect LPAREN tokens in
  let tokens = expect VOID tokens in
  let tokens = expect RPAREN tokens in
  let tokens = expect LBRACE tokens in
  let (stmts, tokens) = parse_block_items tokens in
  let tokens = expect RBRACE tokens in
  (FunctionDef (loc, ident_str ident, stmts), tokens)

let parse_program tokens =
  let (func, tokens) = parse_function tokens in
  let _ = expect EOF tokens in
  Program func
