module StringMap = Map.Make(String)
    
type unary_operator = Complement | Negate | Not
type binary_operator = Add | Subtract | Multiply | Divide | Remainder | ShiftLeft |
                       ShiftRight | BitwiseAnd | BitwiseOr | BitwiseXor |
                       And | Or | Equal | NotEqual | LessThan | LessOrEqual |
                       GreaterThan | GreaterOrEqual
type loc_type = Location of string * int * int
type val_type = ConstantInt of int64 | Var of string
type instruction = Return of val_type |
                   Unary of unary_operator * val_type * val_type |
                   Binary of binary_operator * val_type * val_type * val_type |
                   Copy of val_type * val_type |
                   Jump of string |
                   JumpIfZero of val_type * string |
                   JumpIfNotZero of val_type * string |
                   Label of string
type function_definition = Function of string * instruction list
type program_type = Program of function_definition
type function_context = { func_name: string; func_next_temp_num: int;
                          func_label_map: int StringMap.t }

let make_temporary func_ctx =
  ( { func_ctx with func_next_temp_num = func_ctx.func_next_temp_num + 1 },
    Printf.sprintf "%s.%d" func_ctx.func_name func_ctx.func_next_temp_num )

let make_label func_ctx prefix =
  match StringMap.find_opt prefix func_ctx.func_label_map with
  | None -> ({func_ctx with
              func_label_map=StringMap.add prefix 1 func_ctx.func_label_map},
             Printf.sprintf "%s_%s0" func_ctx.func_name prefix)
  | Some count -> ({func_ctx with
                    func_label_map=StringMap.add prefix (count+1) func_ctx.func_label_map},
                   Printf.sprintf "%s_%s%d" func_ctx.func_name prefix count)
                  
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
  | C_ast.And -> And
  | C_ast.Or -> Or
  | C_ast.Equal -> Equal
  | C_ast.NotEqual -> NotEqual
  | C_ast.LessThan -> LessThan
  | C_ast.LessOrEqual -> LessOrEqual
  | C_ast.GreaterThan -> GreaterThan
  | C_ast.GreaterOrEqual -> GreaterOrEqual

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
  | C_ast.Binary (_, And, src1, src2) ->
    let (func_ctx, instrs, v1) = generate_tacky_expr func_ctx instrs src1 in
    let (func_ctx, false_label) = make_label func_ctx "false" in
    let instrs = instrs @ [JumpIfZero (v1, false_label)] in
    let (func_ctx, instrs, v2) = generate_tacky_expr func_ctx instrs src2 in
    let instrs = instrs @ [JumpIfZero(v2, false_label)] in
    let (func_ctx, end_label) = make_label func_ctx "end" in
    let (func_ctx, dst_name) = make_temporary func_ctx in
    let dst = Var dst_name in
    let instrs = instrs @ [Copy (ConstantInt 1L, dst); Jump end_label;
                           Label false_label; Copy (ConstantInt 0L, dst);
                           Label end_label] in
    (func_ctx, instrs, dst)
  | C_ast.Binary (_, Or, src1, src2) ->
    let (func_ctx, instrs, v1) = generate_tacky_expr func_ctx instrs src1 in
    let (func_ctx, true_label) = make_label func_ctx "true" in
    let instrs = instrs @ [JumpIfNotZero(v1, true_label)] in
    let (func_ctx, instrs, v2) = generate_tacky_expr func_ctx instrs src2 in
    let instrs = instrs @ [JumpIfNotZero(v2, true_label)] in
    let (func_ctx, end_label) = make_label func_ctx "end" in
    let (func_ctx, dst_name) = make_temporary func_ctx in
    let dst = Var dst_name in
    let instrs = instrs @ [Copy (ConstantInt 0L, dst); Jump end_label;
                           Label true_label; Copy (ConstantInt 1L, dst);
                           Label end_label] in
    (func_ctx, instrs, dst)                          
  | C_ast.Binary (_, binary_op, src1, src2) ->
    let (func_ctx, instrs, v1) = generate_tacky_expr func_ctx instrs src1 in
    let (func_ctx, instrs, v2) = generate_tacky_expr func_ctx instrs src2 in
    let (func_ctx, dst_name) = make_temporary func_ctx in    
    let dst = Var dst_name in
    let tacky_op = convert_binop binary_op in
    let instrs = instrs @ [Binary (tacky_op, v1, v2, dst)] in
    (func_ctx, instrs, dst)

let generate_tacky_stmt func_ctx (C_ast.StmtReturn (_, expr)) =
let (func_ctx, instrs, dst) = generate_tacky_expr func_ctx [] expr in
  (func_ctx, instrs @ [Return dst])

let generate_tacky_function (C_ast.FunctionDef (_, name, stmt)) =
  let func_ctx = { func_name=name; func_next_temp_num=0;
                 func_label_map=StringMap.empty} in
  let (_, instrs) = generate_tacky_stmt func_ctx stmt in
  Function (name, instrs)

let generate_tacky_program (C_ast.Program func_type) =
  Program (generate_tacky_function func_type)
