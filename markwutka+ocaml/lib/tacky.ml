type unary_operator = Complement | Negate
type val_type = ConstantInt of int64 | Var of string
type instruction = Return of val_type | Unary of unary_operator * val_type * val_type
type function_definition = Function of string * instruction list
type program_type = Program of function_definition
type function_context = { func_name: string; func_next_temp_num: int }

let make_temporary func_ctx =
  ( { func_ctx with func_next_temp_num = func_ctx.func_next_temp_num + 1 },
    Printf.sprintf "%s.%d" func_ctx.func_name func_ctx.func_next_temp_num )

let convert_unop unary_op =
  match unary_op with
  | C_ast.Complement -> Complement
  | C_ast.Negate -> Negate

let rec generate_tacky_expr func_ctx instrs expr =
  match expr with
  | C_ast.ConstantInt (_, i) -> (func_ctx, instrs, ConstantInt i)
  | C_ast.Unary (_, unary_op, unary_expr) ->
    let (func_ctx, instrs, src) =
      generate_tacky_expr func_ctx instrs unary_expr in
    let (func_ctx, dst_name) = make_temporary func_ctx in
    let dst = Var dst_name in
    let tacky_op = convert_unop unary_op in
    let instrs = instrs @ [Unary (tacky_op, src, dst)] in
    (func_ctx, instrs, dst)

let generate_tacky_stmt func_ctx (C_ast.StmtReturn (_, expr)) =
let (func_ctx, instrs, dst) = generate_tacky_expr func_ctx [] expr in
  (func_ctx, instrs @ [Return dst])

let generate_tacky_function (C_ast.FunctionDef (_, (name, stmt))) =
  let func_ctx = { func_name=name; func_next_temp_num=0 } in
  let (_, instrs) = generate_tacky_stmt func_ctx stmt in
  Function (name, instrs)

let generate_tacky_program (C_ast.Program func_type) =
  Program (generate_tacky_function func_type)
