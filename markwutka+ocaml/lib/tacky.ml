open Context

type unary_operator = Complement | Negate | Not
type binary_operator = Add | Subtract | Multiply | Divide | Remainder |
                       ShiftLeft | ShiftRight | BitwiseAnd | BitwiseOr |
                       BitwiseXor |
                       Equal | NotEqual | LessThan | LessOrEqual |
                       GreaterThan | GreaterOrEqual | And | Or
type val_type = ConstantInt of int64 | Var of string
type instruction = Return of val_type
                 | Unary of unary_operator * val_type * val_type
                 | Binary of binary_operator * val_type * val_type * val_type
                 | Copy of val_type * val_type
                 | Jump of string
                 | JumpIfZero of val_type * string
                 | JumpIfNotZero of val_type * string
                 | Label of string
                 | FunctionCall of string * val_type list * val_type
                 
type function_definition = Function of string * val_type list *
                                         instruction list
type program_type = Program of function_definition list
type function_context = { func_name: string; func_next_temp_num: int }

let (<::) lst item = item :: lst

let tag_label maybe_label tag =
  match maybe_label with
  | None -> failwith "Label was not resolved"
  | Some label -> Printf.sprintf "%s.%s" label tag

let case_tag maybe_case_label =
  match maybe_case_label with
  | None -> failwith "Case value was not evaluated"
  | Some (v, label) ->
     if v < 0L then
       Printf.sprintf "%s.case_%Ld" label v
     else
       Printf.sprintf "%s.case.%Ld" label v

let convert_unop unary_op =
  match unary_op with
  | C_ast.Complement -> Complement
  | C_ast.Negate -> Negate
  | C_ast.Not -> Not
  | _ -> failwith "not implemented yet"

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
  | _ -> failwith "error"

let rec generate_tacky_expr ctx instrs expr =
  match expr with
  | C_ast.ConstantInt (_, i) -> (ctx, instrs, ConstantInt i)
  | C_ast.Unary (_, PreInc, unary_expr) ->
     let (ctx, instrs, src) = generate_tacky_expr ctx instrs unary_expr in
     (ctx, instrs <:: Binary (Add, src, ConstantInt 1L, src), src)
  | C_ast.Unary (_, PreDec, unary_expr) ->
     let (ctx, instrs, src) = generate_tacky_expr ctx instrs unary_expr in
     (ctx, instrs <:: Binary (Subtract, src, ConstantInt 1L, src), src)
  | C_ast.Unary (_, PostInc, unary_expr) ->
     let (ctx, instrs, src) = generate_tacky_expr ctx instrs unary_expr in
     let (ctx, temp_var) = make_func_temporary ctx in
     let instrs =
       instrs
       <:: Copy (src, Var temp_var)
       <:: Binary (Add, src, ConstantInt 1L, src)
     in (ctx, instrs, Var temp_var)
  | C_ast.Unary (_, PostDec, unary_expr) ->
     let (ctx, instrs, src) = generate_tacky_expr ctx instrs unary_expr in
     let (ctx, temp_var) = make_func_temporary ctx in
     let instrs =
       instrs
       <:: Copy (src, Var temp_var)
       <:: Binary (Subtract, src, ConstantInt 1L, src)
     in (ctx, instrs, Var temp_var)
  | C_ast.Unary (_, unary_op, unary_expr) ->
    let (ctx, instrs, src) =
      generate_tacky_expr ctx instrs unary_expr in
    let (ctx, dst_name) = make_func_temporary ctx in
    let dst = Var dst_name in
    let tacky_op = convert_unop unary_op in
    let instrs = instrs <:: Unary (tacky_op, src, dst) in
    (ctx, instrs, dst)
  | C_ast.Binary (_, And, src1, src2) ->
    let (ctx, instrs, v1) = generate_tacky_expr ctx instrs src1 in
    let (ctx, false_label_name) = make_func_label ctx "false" in
    let instrs = instrs <:: JumpIfZero (v1, false_label_name) in
    let (ctx, instrs, v2) = generate_tacky_expr ctx instrs src2 in
    let instrs = instrs <:: JumpIfZero (v2, false_label_name) in
    let (ctx, dst_name) = make_func_temporary ctx in
    let dst = Var dst_name in
    let instrs = instrs <:: Copy (ConstantInt 1L, dst) in
    let (ctx, end_label_name) = make_func_label ctx "end" in
    let instrs = instrs <:: Jump end_label_name
                           <:: Label false_label_name
                           <:: Copy (ConstantInt 0L, dst)
                           <:: Label end_label_name in
    (ctx, instrs, dst)
  | C_ast.Binary (_, Or, src1, src2) ->
    let (ctx, instrs, v1) = generate_tacky_expr ctx instrs src1 in
    let (ctx, true_label_name) = make_func_label ctx "true" in
    let instrs = instrs <:: JumpIfNotZero (v1, true_label_name) in
    let (ctx, instrs, v2) = generate_tacky_expr ctx instrs src2 in
    let instrs = instrs <:: JumpIfNotZero (v2, true_label_name) in
    let (ctx, dst_name) = make_func_temporary ctx in
    let dst = Var dst_name in
    let instrs = instrs <:: Copy (ConstantInt 0L, dst) in
    let (ctx, end_label_name) = make_func_label ctx "end" in
    let instrs = instrs <:: Jump end_label_name
                           <:: Label true_label_name
                           <:: Copy (ConstantInt 1L, dst)
                           <:: Label end_label_name in
    (ctx, instrs, dst)        
  | C_ast.Binary (_, binary_op, src1, src2) ->
    let (ctx, instrs, v1) = generate_tacky_expr ctx instrs src1 in
    let (ctx, instrs, v2) = generate_tacky_expr ctx instrs src2 in
    let (ctx, dst_name) = make_func_temporary ctx in    
    let dst = Var dst_name in
    let tacky_op = convert_binop binary_op in
    let instrs = instrs <:: Binary (tacky_op, v1, v2, dst) in
    (ctx, instrs, dst)
  | C_ast.Var (_, var_name) -> (ctx, instrs, Var var_name)
  | C_ast.Assignment (_, C_ast.Var (_, var_name), expr) ->
    let (ctx, instrs, dst) = generate_tacky_expr ctx instrs expr in
    (ctx, instrs <:: Copy (dst, Var var_name), Var var_name)
  | C_ast.Assignment (loc, _, _) ->
     fail_at loc "assignment to non-lvalue"
  | C_ast.AssignmentExpr (_, C_ast.Var (_, var_name), expr) ->
    let (ctx, instrs, dst) = generate_tacky_expr ctx instrs expr in
    (ctx, instrs <:: Copy (dst, Var var_name), Var var_name)
  | C_ast.AssignmentExpr (loc, _, _) ->
     fail_at loc "assignment to non-lvalue"
  | C_ast.Condition (_, test_expr, true_expr, false_expr) ->
     let (ctx, false_label_name) = make_func_label ctx "false" in
     let (ctx, end_label_name) = make_func_label ctx "end" in
     let (ctx, dst_name) = make_func_temporary ctx in
     let dst = Var dst_name in
     let (ctx, instrs, test_expr) =
       generate_tacky_expr ctx instrs test_expr in
     let instrs = instrs <:: JumpIfZero (test_expr, false_label_name) in
     let (ctx, instrs, true_expr) =
       generate_tacky_expr ctx instrs true_expr in
     let instrs =
       instrs
       <:: Copy (true_expr, dst)
       <:: Jump end_label_name
       <:: Label false_label_name in
     let (ctx, instrs, false_expr) =
       generate_tacky_expr ctx instrs false_expr in
     (ctx, instrs <:: Copy (false_expr, dst) <:: Label end_label_name,
      dst)
  | C_ast.FunctionCall (_, name, args) ->
     generate_func_call ctx instrs name args
and generate_func_call ctx instrs name args =
  let generate_arg (ctx,instrs,dsts) arg =
    let (ctx, instrs, dst) = generate_tacky_expr ctx instrs arg in
    (ctx, instrs, dst :: dsts)
  in
  let (ctx, instrs, dsts) = List.fold_left generate_arg
                              (ctx, instrs, []) args in
  let dsts = List.rev dsts in
  let (ctx, dst_name) = make_func_temporary ctx in
  let dst = Var dst_name in
  (ctx, instrs <:: FunctionCall (name, dsts, dst), dst)    

let rec generate_tacky_stmt ctx instrs stmt =
  match stmt with
  | C_ast.Return (_, expr) ->
    let (ctx, instrs, dst) = generate_tacky_expr ctx instrs expr in
    (ctx, instrs <:: Return dst)
  | C_ast.Expression (_, expr) ->
    let (ctx, instrs, _dst) = generate_tacky_expr ctx instrs expr in
    (ctx, instrs)
  | C_ast.If (_, test_expr, true_stmt, Some false_stmt) ->
     let (ctx, instrs, dst) = generate_tacky_expr ctx instrs test_expr in
     let (ctx, else_label_name) = make_func_label ctx "false" in
     let (ctx, end_label_name) = make_func_label ctx "end" in
     let instrs = instrs <:: JumpIfZero (dst, else_label_name) in
     let (ctx, instrs) = generate_tacky_stmt ctx instrs true_stmt in
     let instrs =
       instrs
       <:: Jump end_label_name
       <:: Label else_label_name in
     let (ctx, instrs) = generate_tacky_stmt ctx instrs false_stmt in
     (ctx, instrs <:: Label end_label_name)
  | C_ast.If (_, test_expr, true_stmt, None) ->
     let (ctx, instrs, dst) = generate_tacky_expr ctx instrs test_expr in
     let (ctx, end_label_name) = make_func_label ctx "end" in
     let instrs = instrs <:: JumpIfZero (dst, end_label_name) in
     let (ctx, instrs) = generate_tacky_stmt ctx instrs true_stmt in
     (ctx, instrs <:: Label end_label_name)
  | C_ast.Compound (_, Block block_items) ->
     generate_block_items ctx instrs block_items
  | C_ast.Label (_, label_str, stmt) ->
     let instrs = instrs <:: Label label_str in
     let (ctx, instrs) = generate_tacky_stmt ctx instrs stmt in
     (ctx, instrs)
  | C_ast.Goto (_, label_str) ->
     (ctx, instrs <:: Jump label_str)
  | C_ast.Break (_, label) ->
     (ctx, instrs <:: Jump (tag_label label "break"))
  | C_ast.Continue (_, label) ->
     (ctx, instrs <:: Jump (tag_label label "cont"))
  | C_ast.Case (_, _, label) ->
     (ctx, instrs <:: Label (case_tag label))
  | C_ast.Default (_, label) ->
     (ctx, instrs <:: Label (tag_label label "case.default"))
  | C_ast.While (_, test_expr, stmt, label) ->
     let instrs = instrs <:: Label (tag_label label "cont") in
     let (ctx, instrs, dst) = generate_tacky_expr ctx instrs test_expr in
     let instrs = instrs <:: JumpIfZero (dst, tag_label label "break") in
     let (ctx, instrs) = generate_tacky_stmt ctx instrs stmt in
     let instrs =
       instrs
       <:: Jump (tag_label label "cont")
       <:: Label (tag_label label "break") in
     (ctx, instrs)
  | C_ast.DoWhile (_, test_expr, stmt, label) ->
     let instrs = instrs <:: Label (tag_label label "top") in
     let (ctx, instrs) = generate_tacky_stmt ctx instrs stmt in
     let instrs = instrs <:: Label (tag_label label "cont") in
     let (ctx, instrs, dst) = generate_tacky_expr ctx instrs test_expr in
     let instrs = instrs <:: JumpIfNotZero (dst, tag_label label "top") in
     let instrs = instrs <:: Label (tag_label label "break") in
     (ctx, instrs)
  | C_ast.For (_, init, test_expr, post_expr, stmt, label) ->
     let (ctx, instrs) =
       (match init with
        | InitDecl decl -> generate_tacky_var_decl ctx instrs decl
        | InitExpr (Some expr) ->
           let (ctx, instrs, _) = generate_tacky_expr ctx instrs expr in
           (ctx, instrs)
        | InitExpr None -> (ctx, instrs)) in
     let instrs = instrs <:: Label (tag_label label "top") in
     let (ctx, instrs) =
       (match test_expr with
       | Some test_expr ->
          let (ctx, instrs, dst) =
            generate_tacky_expr ctx instrs test_expr in          
          let instrs = instrs <:: JumpIfZero (dst, tag_label label "break") in
          (ctx, instrs)
       | None -> (ctx, instrs)) in
     let (ctx, instrs) = generate_tacky_stmt ctx instrs stmt in
     let instrs = instrs <:: Label (tag_label label "cont") in
     let (ctx, instrs) =
       (match post_expr with
        | Some post_expr ->
           let (ctx, instrs, _) = generate_tacky_expr ctx instrs post_expr in
           (ctx, instrs)
        | None -> (ctx, instrs)) in
     let instrs =
       instrs
       <:: Jump (tag_label label "top")
       <:: Label (tag_label label "break") in
     (ctx, instrs)
  | C_ast.Switch (_, expr, stmt, cases, opt_default, label) ->
    let (ctx, instrs, switch_exp_dst) =
      generate_tacky_expr ctx instrs expr in
    let (ctx, switch_cmp_dst_name) = make_func_temporary ctx in
    let switch_cmp_dst = Var switch_cmp_dst_name in
    let rec generate_case_tests instrs prev_case_val cases =
      (match cases with
       | [] -> instrs
       | ((next_v, _) as case_data) :: cases_rest ->
         let instrs =
           (match prev_case_val with
            | None -> instrs
            | Some prev_v ->
              if next_v > Int64.add prev_v 1L then
                (match opt_default with
                 | None ->
                   instrs
                   <:: Binary (LessThan, switch_exp_dst, ConstantInt next_v,
                               switch_cmp_dst)
                   <:: JumpIfNotZero (switch_cmp_dst, tag_label label "break")
                 | Some _ ->
                   instrs
                   <:: Binary (LessThan, switch_exp_dst, ConstantInt next_v,
                               switch_cmp_dst)
                   <:: JumpIfNotZero
                     (switch_cmp_dst, tag_label opt_default "case.default"))
              else
                instrs) in
         let instrs =
           instrs
           <:: Binary (Equal, switch_exp_dst, ConstantInt next_v,
                       switch_cmp_dst)
           <:: JumpIfNotZero (switch_cmp_dst, case_tag (Some case_data)) in
         generate_case_tests instrs (Some next_v) cases_rest)
    in
    let instrs = generate_case_tests instrs None cases in
    let instrs =
      (match opt_default with
       | None -> instrs <:: Jump (tag_label label "break")
       | Some _ ->
         instrs <:: Jump (tag_label opt_default "case.default")) in
    let (ctx, instrs) = generate_tacky_stmt ctx instrs stmt in
    (ctx, instrs <:: Label (tag_label label "break"))
  | C_ast.Null -> (ctx, instrs)

and generate_tacky_var_decl ctx instrs (VarDecl (_, var_name,
                                                 _storage_class, expr)) =
  match expr with
  | None -> (ctx, instrs)
  | Some expr ->
     let (ctx, instrs, dst) = generate_tacky_expr ctx instrs expr in
     (ctx, instrs <:: Copy (dst, Var var_name))

and generate_tacky_declaration ctx instrs decl =
  match decl with
  | C_ast.F (C_ast.FunDecl _) -> (ctx, instrs)
  | C_ast.V decl -> generate_tacky_var_decl ctx instrs decl

and generate_block_item (ctx,instrs) item =
  match item with
  | C_ast.D decl -> generate_tacky_declaration ctx instrs decl
  | C_ast.S stmt -> generate_tacky_stmt ctx instrs stmt

and generate_block_items ctx instrs block_items =
  List.fold_left generate_block_item (ctx,instrs) block_items
let generate_tacky_function ctx (C_ast.FunDecl (_, name, _storage_class,
                                 args, maybe_block_items)) =
  match maybe_block_items with
  | None -> failwith "Tried to generate tacky for external function"
  | Some block_items ->
     let args = List.map (fun name -> Var name) args in     
     let ctx = enter_func ctx name in
     let (ctx, instrs) = generate_block_items ctx [] block_items in
     let ctx = leave_func ctx in
     (ctx, Function (name, args,
                     List.rev (instrs <:: Return (ConstantInt 0L))))

let generate_tacky_program ctx (C_ast.Program func_types) =
  let is_full_func (C_ast.FunDecl (_, _, _, _, maybe_block_items)) =
    Option.is_some maybe_block_items in
  let func_types = List.filter is_full_func func_types in
  let (ctx, func_defs) = map_with_ctx generate_tacky_function ctx func_types in
  (ctx, Program func_defs)
