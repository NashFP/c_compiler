open C_ast
open Lexer

let str_of_token = function
  | CONSTANT_INT _ -> Printf.sprintf "int constant"
  | IDENTIFIER _ -> Printf.sprintf "identifier"
  | INT -> "int"
  | VOID -> "void"
  | RETURN -> "return"
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
  | EOF -> "end of file"

let ident_str = function
  | IDENTIFIER str -> str
  | tok -> (Printf.printf "Tried to fetch identifier name of token %s\n" (str_of_token tok);
          exit 2)

let constant_int_val = function
  | CONSTANT_INT i -> i
  | tok -> (Printf.printf "Tried to fetch constant int value of token %s\n" (str_of_token tok);
          exit 2)

let same_token t1 t2 =
  match (t1, t2) with
  | (CONSTANT_INT _, CONSTANT_INT _) -> true
  | (IDENTIFIER _, IDENTIFIER _) -> true
  | _ -> t1 == t2

let fail_at (C_ast.Location (filename, line, column)) message =
  Printf.printf "%s, line %d, column %d: %s\n" filename line column message;
  exit 1

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
  | _ -> false

let binop_precedence = function
  | ASTERISK -> 50
  | SLASH -> 50
  | PERCENT -> 50
  | PLUS -> 40
  | MINUS -> 40
  | _ -> 99

let parse_unop tokens =
  let ((tok,loc), next_tokens) = peek tokens in
  match tok with
  | MINUS -> (Negate, next_tokens)
  | TILDE -> (Complement, next_tokens)
  | _ -> fail_at loc (Printf.sprintf "Unexpected unary operator %s" (str_of_token tok))

let parse_binop tokens =
  let ((tok,loc), next_tokens) = peek tokens in
  match tok with
  | MINUS -> (Subtract, next_tokens)
  | PLUS -> (Add, next_tokens)
  | ASTERISK -> (Multiply, next_tokens)
  | SLASH -> (Divide, next_tokens)
  | PERCENT -> (Remainder, next_tokens)
  | _ -> fail_at loc (Printf.sprintf "Unexpected binary operator %s" (str_of_token tok))

let rec parse_factor tokens =
  let ((tok,loc), next_tokens) = peek tokens in
  if same_token tok (CONSTANT_INT 0L) then
    (ConstantInt (loc, constant_int_val tok), next_tokens)
  else if (same_token tok MINUS) || (same_token tok TILDE) then
    let (unop, tokens) = parse_unop tokens in
    let (inner_expr, tokens) = parse_factor tokens in
    (Unary (loc, unop, inner_expr), tokens)
  else if same_token tok LPAREN then
    let (inner_expr, tokens) = parse_expr next_tokens 0 in
    let tokens = expect RPAREN tokens in
    (inner_expr, tokens)
  else
    fail_at loc (Printf.sprintf "Unexpected token %s" (str_of_token tok))

and parse_expr tokens min_prec =
  let ((_,loc),_) = peek tokens in
  let (left, tokens) = parse_factor tokens in
  let rec parse_expr1 loc curr_left tokens min_prec =
    let ((tok,_), _) = peek tokens in
    if (is_binop tok) && (binop_precedence tok) >= min_prec then
      let (operator, tokens) = parse_binop tokens in
      let (right, tokens) = parse_expr tokens ((binop_precedence tok) + 1) in
      parse_expr1 loc (Binary (loc, operator, curr_left, right)) tokens min_prec
    else
      (curr_left, tokens)
  in
  parse_expr1 loc left tokens min_prec
  
let parse_statement tokens =
  let (_, loc, tokens) = expect_and_get RETURN tokens in
  let (expr, tokens) = parse_expr tokens 0 in
  let tokens = expect SEMI tokens in
  (StmtReturn (loc, expr), tokens)
let parse_function tokens =
  let (_, loc, tokens) = expect_and_get INT tokens in
  let (ident, _, tokens) = expect_and_get (IDENTIFIER "") tokens in
  let tokens = expect LPAREN tokens in
  let tokens = expect VOID tokens in
  let tokens = expect RPAREN tokens in
  let tokens = expect LBRACE tokens in
  let (stmt, tokens) = parse_statement tokens in
  (FunctionDef (loc, ident_str ident, stmt), tokens)

let parse_program tokens =
  let (func, tokens) = parse_function tokens in
  let _ = expect EOF tokens in
  Program func
