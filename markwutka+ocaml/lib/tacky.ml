open Context
    
type unary_operator = Complement | Negate | Not
type binary_operator = Add | Subtract | Multiply | Divide | Remainder | ShiftLeft |
                       ShiftRight | BitwiseAnd | BitwiseOr | BitwiseXor |
                       Equal | NotEqual | LessThan | LessOrEqual |
                       GreaterThan | GreaterOrEqual | And | Or
type val_type = ConstantInt of int64 | Var of string
type instruction = Return of val_type | Unary of unary_operator * val_type * val_type |
                   Binary of binary_operator * val_type * val_type * val_type |
                   Copy of val_type * val_type |
                   Jump of string |
                   JumpIfZero of val_type * string |
                   JumpIfNotZero of val_type * string |
                   Label of string
                 
type function_definition = Function of string * instruction list
type program_type = Program of function_definition
type function_context = { func_name: string; func_next_temp_num: int }

let convert_unop unary_op =
  match unary_op with
  | C_ast.Complement -> Complement
  | C_ast.Negate -> Negate
  | C_ast.Not -> Not

let convert_binop binary_op =
  match binary_op with
  | C_ast.Add -> Add
  | C_ast.Subtract -> Subtract
  | C_ast.Multiply -> Multiply
  | C_ast.Divide -> Divide
  | C_ast.Remainder -> Remainder
  | C_ast.ShiftLeft -> ShiftLeft
  | C_ast.ShiftRight -> ShiftRight
  | C_ast.BitwiseAnd -> BitwiseAnd
  | C_ast.BitwiseOr -> BitwiseOr
  | C_ast.BitwiseXor -> BitwiseXor
  | C_ast.Equal -> Equal
  | C_ast.NotEqual -> NotEqual
  | C_ast.LessThan -> LessThan
  | C_ast.LessOrEqual -> LessOrEqual
  | C_ast.GreaterThan -> GreaterThan
  | C_ast.GreaterOrEqual -> GreaterOrEqual
  | C_ast.And -> And
  | C_ast.Or -> Or

let rec generate_tacky_expr ctx instrs expr =
  match expr with
  | C_ast.ConstantInt (_, i) -> (ctx, instrs, ConstantInt i)
  | C_ast.Unary (_, unary_op, unary_expr) ->
    let (ctx, instrs, src) =
      generate_tacky_expr ctx instrs unary_expr in
    let (ctx, dst_name) = make_func_temporary ctx in
    let dst = Var dst_name in
    let tacky_op = convert_unop unary_op in
    let instrs = instrs @ [Unary (tacky_op, src, dst)] in
    (ctx, instrs, dst)
  | C_ast.Binary (_, And, src1, src2) ->
    let (ctx, instrs, v1) = generate_tacky_expr ctx instrs src1 in
    let (ctx, false_label_name) = make_func_label ctx "false" in
    let instrs = instrs @ [JumpIfZero (v1, false_label_name)] in
    let (ctx, instrs, v2) = generate_tacky_expr ctx instrs src2 in
    let instrs = instrs @ [JumpIfZero (v2, false_label_name)] in
    let (ctx, dst_name) = make_func_temporary ctx in
    let dst = Var dst_name in
    let instrs = instrs @ [Copy (ConstantInt 1L, dst)] in
    let (ctx, end_label_name) = make_func_label ctx "end" in
    let instrs = instrs @ [Jump end_label_name;
                           Label false_label_name;
                           Copy (ConstantInt 0L, dst);
                           Label end_label_name] in
    (ctx, instrs, dst)
  | C_ast.Binary (_, Or, src1, src2) ->
    let (ctx, instrs, v1) = generate_tacky_expr ctx instrs src1 in
    let (ctx, true_label_name) = make_func_label ctx "true" in
    let instrs = instrs @ [JumpIfNotZero (v1, true_label_name)] in
    let (ctx, instrs, v2) = generate_tacky_expr ctx instrs src2 in
    let instrs = instrs @ [JumpIfNotZero (v2, true_label_name)] in
    let (ctx, dst_name) = make_func_temporary ctx in
    let dst = Var dst_name in
    let instrs = instrs @ [Copy (ConstantInt 0L, dst)] in
    let (ctx, end_label_name) = make_func_label ctx "end" in
    let instrs = instrs @ [Jump end_label_name;
                           Label true_label_name;
                           Copy (ConstantInt 1L, dst);
                           Label end_label_name] in
    (ctx, instrs, dst)        
  | C_ast.Binary (_, binary_op, src1, src2) ->
    let (ctx, instrs, v1) = generate_tacky_expr ctx instrs src1 in
    let (ctx, instrs, v2) = generate_tacky_expr ctx instrs src2 in
    let (ctx, dst_name) = make_func_temporary ctx in    
    let dst = Var dst_name in
    let tacky_op = convert_binop binary_op in
    let instrs = instrs @ [Binary (tacky_op, v1, v2, dst)] in
    (ctx, instrs, dst)
  | C_ast.Var (_, var_name) -> (ctx, instrs, Var var_name)
  | C_ast.Assignment (_, C_ast.Var (_, var_name), expr) ->
    let (ctx, instrs, dst) = generate_tacky_expr ctx instrs expr in
    (ctx, instrs @ [Copy (dst, Var var_name)], Var var_name)
  | _ -> failwith "tacky can't match expr"

let generate_tacky_stmt ctx instrs stmt =
  match stmt with
  | (C_ast.Return (_, expr)) ->
    let (ctx, instrs, dst) = generate_tacky_expr ctx instrs expr in
    (ctx, instrs @ [Return dst])
  | (C_ast.Expression (_, expr)) ->
    let (ctx, instrs, _dst) = generate_tacky_expr ctx instrs expr in
    (ctx, instrs)
  | _ -> failwith "tacky can't match stmt"

let generate_tacky_declaration ctx instrs
    (C_ast.Declaration (_, _, expr)) =
  match expr with
  | None -> (ctx, instrs)
  | Some expr -> let (ctx, instrs, _) = generate_tacky_expr ctx instrs expr in
    (ctx, instrs)
                     
let generate_block_item (ctx,instrs) item =
  match item with
  | C_ast.D decl -> generate_tacky_declaration ctx instrs decl
  | C_ast.S stmt -> generate_tacky_stmt ctx instrs stmt

let generate_tacky_function ctx (C_ast.FunctionDef (_, name, block_items)) =
  let ctx = context_in_func ctx name in
  let (ctx, instrs) = List.fold_left generate_block_item (ctx, []) block_items in
  (ctx, Function (name, instrs))

let generate_tacky_program ctx (C_ast.Program func_type) =
  let (ctx, func_def) = generate_tacky_function ctx func_type in
  (ctx, Program func_def)
