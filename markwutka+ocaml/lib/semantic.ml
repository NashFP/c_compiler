open C_ast
open Context

module StringSet = Set.Make(String)
module Int64Set = Set.Make(Int64)

let (<::) lst item = item :: lst

let is_lvalue = function
  | Var (_, _) -> true
  | _ -> false

let is_compound_op = function
  | PreInc -> true
  | PreDec -> true
  | PostInc -> true
  | PostDec -> true
  | _ -> false

let compound_name = function
  | PreInc -> "pre-increment"
  | PreDec -> "pre-decrement"
  | PostInc -> "post-increment"
  | PostDec -> "post-decrement"
  | _ -> failwith "Tried to get compound name for non compound op"

let rec resolve_expr ctx expr =
  match expr with
  | Var (loc, var_name) ->
    (match lookup_var ctx var_name with
     | None -> Context.fail_at loc
                 (Printf.sprintf "Undeclared variable: %s" var_name)
     | Some v -> (ctx, Var (loc, v.unique_var_name)))
  | Assignment (loc, (Var (vloc, var_name)), expr) ->
     (match lookup_var ctx var_name with
      | None -> Context.fail_at loc
                  (Printf.sprintf "Undeclared variable: %s" var_name)
      | Some v ->
         let (ctx, expr) = resolve_expr ctx expr in
         (ctx, Assignment (loc, Var (vloc, v.unique_var_name), expr)))
  | Assignment (loc, _, _) ->
     Context.fail_at loc
       (Printf.sprintf "Invalid lvalue in assignment")
  | AssignmentExpr (loc, (Var (vloc, var_name)), expr) ->
     (match lookup_var ctx var_name with
      | None -> Context.fail_at loc
                  (Printf.sprintf "Undeclared variable: %s" var_name)
      | Some v ->
         let (ctx, expr) = resolve_expr ctx expr in
         (ctx, AssignmentExpr (loc, Var (vloc, v.unique_var_name), expr)))
  | AssignmentExpr (loc, _, _) ->
     Context.fail_at loc
       (Printf.sprintf "Invalid lvalue in assignment")
  | Binary (loc, op, exp1, exp2) ->
     let (ctx, exp1) = resolve_expr ctx exp1 in
     let (ctx, exp2) = resolve_expr ctx exp2 in
     (ctx, Binary (loc, op, exp1, exp2))
  | ConstantInt (loc, v) -> (ctx, ConstantInt (loc, v))
  | Unary (loc, op, expr) ->
     let (ctx, expr) = resolve_expr ctx expr in
     if (is_compound_op op) && not (is_lvalue expr) then
       fail_at loc (Printf.sprintf "Can't apply %s to non-lvalue"
                      (compound_name op))
     else
       (ctx, Unary (loc, op, expr))
  | Condition (loc, test_expr, true_expr, false_expr) ->
     let (ctx, test_expr) = resolve_expr ctx test_expr in
     let (ctx, true_expr) = resolve_expr ctx true_expr in
     let (ctx, false_expr) = resolve_expr ctx false_expr in
     (ctx, Condition (loc, test_expr, true_expr, false_expr))
and resolve_optional_expr ctx maybe_expr =
  match maybe_expr with
  | None -> (ctx, None)
  | Some expr ->
     let (ctx, expr) = resolve_expr ctx expr in
     (ctx, Some expr)
                                              
and resolve_statement ctx stmt create_block =
  match stmt with
  | Expression (loc, expr) ->
     let (ctx, expr) = resolve_expr ctx expr in
     (ctx, Expression (loc, expr))
  | Return (loc, expr) ->
     let (ctx, expr) = resolve_expr ctx expr in
     (ctx, Return (loc, expr))
  | If (loc, expr, true_stmt, maybe_false_stmt) ->
     let (ctx, expr) = resolve_expr ctx expr in
     let (ctx, true_stmt) = resolve_statement ctx true_stmt true in
     (match maybe_false_stmt with
      | Some false_stmt ->
         let (ctx, false_stmt) = resolve_statement ctx false_stmt true in
         (ctx, If (loc, expr, true_stmt, Some false_stmt))
      | None ->
         (ctx, If (loc, expr, true_stmt, None)))
  | Compound (loc, Block block_items) ->
    let ctx = if create_block then enter_statements_block ctx else ctx in
    let (ctx, block_items) = resolve_block_items ctx block_items in
    let ctx = if create_block then leave_block ctx else ctx in

     (ctx, Compound (loc, Block block_items))
  | Label (loc,str,stmt) -> 
     let (ctx, stmt) = resolve_statement ctx stmt true in
     (ctx, Label (loc,str,stmt))
  | While (loc,test_expr,stmt,opt_label) ->
    let (ctx,_) = enter_block ctx BlockWhile in
    let (ctx, test_expr) = resolve_expr ctx test_expr in
    let (ctx, stmt) = resolve_statement ctx stmt false in
    let ctx = leave_block ctx in
     (ctx, While (loc,test_expr,stmt,opt_label))
  | DoWhile (loc,test_expr,stmt,opt_label) ->
    let (ctx,_) = enter_block ctx BlockWhile in
    let (ctx, test_expr) = resolve_expr ctx test_expr in
    let (ctx, stmt) = resolve_statement ctx stmt false in
    let ctx = leave_block ctx in
     (ctx, DoWhile (loc,test_expr,stmt,opt_label))
  | For (loc,init,test_expr,post_expr,stmt,opt_label) ->
     let (ctx,_) = enter_block ctx BlockFor in
     let (ctx, init) =
       match init with
       | InitDecl decl ->
          let (ctx, decl) = resolve_declaration ctx decl in
          (ctx, InitDecl decl)
       | InitExpr expr ->
          let (ctx, expr) = resolve_optional_expr ctx expr in
          (ctx, InitExpr expr) in
     let (ctx, test_expr) = resolve_optional_expr ctx test_expr in
     let (ctx, post_expr) = resolve_optional_expr ctx post_expr in
     let (ctx, stmt) = resolve_statement ctx stmt true in
     let ctx = leave_block ctx in
     (ctx, For (loc,init,test_expr,post_expr,stmt,opt_label))
  | Switch (loc,expr,stmt, cases, opt_default, opt_label) ->
    let (ctx,_) = enter_block ctx BlockSwitch in
    let (ctx, expr) = resolve_expr ctx expr in
    let (ctx, stmt) = resolve_statement ctx stmt false in
    let ctx = leave_block ctx in
    (ctx, Switch (loc,expr,stmt,cases, opt_default, opt_label))
  | Break _ -> (ctx, stmt)
  | Continue _ -> (ctx, stmt)
  | Case _ -> (ctx, stmt)
  | Default _ -> (ctx, stmt)
  | Goto _ -> (ctx, stmt)
  | Null -> (ctx, stmt)
and resolve_declaration ctx (Declaration (loc, var_name, var_exp)) =
  let (ctx, unique_var) = make_unique_var ctx loc var_name in
  match var_exp with
  | Some var_exp ->
     let (ctx, var_exp) = resolve_expr ctx var_exp in
     (ctx, Declaration (loc, unique_var.unique_var_name, Some var_exp))
  | None -> (ctx, Declaration (loc, unique_var.unique_var_name, None))
and resolve_block_item (ctx, block_items) block_item =
  match block_item with
  | D decl -> let (ctx, decl) = resolve_declaration ctx decl in
              (ctx, (D decl) :: block_items)
  | S stmt -> let (ctx, stmt) = resolve_statement ctx stmt true in
              (ctx, (S stmt) :: block_items)
and resolve_block_items ctx block_items =
  let (ctx, block_items) =
    List.fold_left resolve_block_item (ctx,[]) block_items in
  (ctx, List.rev block_items)
and resolve_function ctx (FunctionDef (loc, func_name, block_items)) =
  let ctx = enter_func ctx func_name in
  let (ctx,block_items) = resolve_block_items ctx block_items in
  (leave_func ctx, FunctionDef (loc, func_name, block_items))

let resolve_variables ctx (Program func_def) =
  let (ctx, func_def) = resolve_function ctx func_def in
  (ctx, Program func_def)

let verify_label_and_goto (Program func_def) =
  let rec find_labels labels block_item =
    match block_item with
    | D _ -> labels
    | S (Label (loc, str,stmt)) ->
      if StringMap.mem str labels then
        fail_at loc (Printf.sprintf "Duplicate label %s in function" str)
      else
        let labels = find_labels labels (S stmt) in
        StringMap.add str loc labels
    | S (If (_, _, true_stmt, Some false_stmt)) ->
      let labels = find_labels labels (S true_stmt) in
      find_labels labels (S false_stmt)
    | S (If (_, _, true_stmt, None)) ->
      find_labels labels (S true_stmt)
    | S (While (_, _, stmt, _)) ->
      find_labels labels (S stmt)
    | S (DoWhile (_, _, stmt, _)) ->
      find_labels labels (S stmt)
    | S (For (_, _, _, _, stmt, _)) ->
      find_labels labels (S stmt)
    | S (Switch (_, _, stmt, _, _, _)) ->
      find_labels labels (S stmt)
    | S (Compound (_, Block block_items)) ->
      List.fold_left find_labels labels block_items
    | _ -> labels in
  let verify_goto labels used_labels block_item =
    match block_item with
    | S (Goto (loc, str)) ->
       if not (StringMap.mem str labels) then
         fail_at loc (Printf.sprintf "No matching label found for goto %s"
                        str)
       else
         StringMap.remove str used_labels
    | _ -> used_labels in
  let verify_function (FunctionDef (loc, func_name, block_items)) =
    let label_set = List.fold_left find_labels StringMap.empty block_items in
    let used_labels = List.fold_left (verify_goto label_set)
                        label_set block_items in
    if StringMap.cardinal used_labels > 0 then
      let warn_unused (label,loc) =
        warn_at loc (Printf.sprintf "Unused label %s" label) in
      (List.iter warn_unused (StringMap.to_list used_labels);
       FunctionDef (loc, func_name, block_items))
    else
      FunctionDef (loc, func_name, block_items)
  in
  Program (verify_function func_def)

let rec eval_const_expr expr =
  let to_bool i = if i == 0L then false else true  in
  let from_bool b = if b then 1L else 0L in
  match expr with
  | ConstantInt (_,v) -> Some v
  | Unary (_, op, unary_expr) ->
    (match eval_const_expr unary_expr with
     | None -> None
     | Some v ->
       (match op with
        | Complement -> Some (Int64.logxor v 0L)
        | Negate -> Some (Int64.neg v)
        | Not -> Some (if v == 0L then 1L else 0L)
        | _ -> None))
  | Binary (_, op, left, right) ->
    (match eval_const_expr left with
     | None -> None
     | Some l ->
       (match eval_const_expr right with
        | None -> None
        | Some r ->
          (match op with
           | Add -> Some (Int64.add l r)
           | Subtract -> Some (Int64.sub l r)
           | Multiply -> Some (Int64.mul l r)
           | Divide -> Some (Int64.div l r)
           | Remainder -> Some (Int64.rem l r)
           | ShiftLeft -> Some (Int64.shift_left l (Int64.to_int r))
           | ShiftRight -> Some (Int64.shift_right l (Int64.to_int r))
           | BitwiseAnd -> Some (Int64.logand l r)
           | BitwiseOr -> Some (Int64.logor l r)
           | BitwiseXor -> Some (Int64.logxor l r)
           | And -> Some (from_bool ((to_bool l) && (to_bool r)))
           | Or -> Some (from_bool ((to_bool l) || (to_bool r)))
           | Equal -> Some (from_bool (l == r))
           | NotEqual -> Some (from_bool (l != r))
           | LessThan -> Some (from_bool (l < r))
           | LessOrEqual -> Some (from_bool (l <= r))
           | GreaterThan -> Some (from_bool (l > r))
           | GreaterOrEqual -> Some (from_bool (l >= r))
           | _ -> None)))
  | _ -> None
         
let label_loops ctx (Program func_def) =
  let rec label_stmt ctx stmt create_block =
    match stmt with
    | Return _ -> (ctx, stmt)
    | Expression _ -> (ctx, stmt)
    | If (loc, test_expr, true_stmt, maybe_false_stmt) ->
       let (ctx, true_stmt) = label_stmt ctx true_stmt true in
       let (ctx, maybe_false_stmt) =
         label_optional_stmt ctx maybe_false_stmt in
       (ctx, If (loc, test_expr, true_stmt, maybe_false_stmt))
    | Label (loc, label, stmt) ->
        let (ctx, stmt) = label_stmt ctx stmt true in
        (ctx, Label (loc, label, stmt))
    | Goto _ -> (ctx, stmt)
    | Compound (loc, Block block_items) ->
      let ctx = if create_block then enter_statements_block ctx else ctx in
       let (ctx, block_items) = label_block_items ctx block_items in
       let ctx = if create_block then leave_block ctx else ctx in
       (ctx, Compound (loc, Block block_items))
    | Break (loc, _) ->
       (match curr_breakable_id ctx with
        | None -> fail_at loc "break statement outside of a loop or switch"
        | Some (block_id, _) ->
           (ctx, Break (loc, Some block_id)))
    | Continue (loc, _) ->
       (match curr_loop_id ctx with
        | None -> fail_at loc "continue statement outside of a loop"
        | Some (block_id, _) ->
          (ctx, Continue (loc, Some block_id)))
    | While (loc, test_expr, stmt, _) ->
       let (ctx, block_name) = enter_block ctx BlockWhile in
       let (ctx, stmt) = label_stmt ctx stmt false in
       let ctx = leave_block ctx in
       (ctx, While (loc, test_expr, stmt, Some block_name))
    | DoWhile (loc, test_expr, stmt, _) ->
       let (ctx, block_name) = enter_block ctx BlockDoWhile in
       let (ctx, stmt) = label_stmt ctx stmt false in
       let ctx = leave_block ctx in
       (ctx, DoWhile (loc, test_expr, stmt, Some block_name))
    | For (loc, init, test_expr, post_expr, stmt, _) ->
      let (ctx, block_name) = enter_block ctx BlockFor in
      let (ctx, stmt) = label_stmt ctx stmt true in
      let ctx = leave_block ctx in
       (ctx, For (loc, init, test_expr, post_expr, stmt, Some block_name))
    | Switch (loc, switch_expr, stmt, _, _, _) ->
      let (ctx, block_name) = enter_block ctx BlockSwitch in
      let (ctx, stmt) = label_stmt ctx stmt false in
      (match curr_switch_ctx ctx with
      | None -> failwith "Switch context is missing"
      | Some switch_ctx ->
        (leave_block ctx, Switch (loc, switch_expr, stmt,
                      get_switch_cases switch_ctx,
                      get_switch_default switch_ctx, Some block_name)))
    | Case (loc, expr, _) ->
      (match curr_switch_id ctx with
       | None -> fail_at loc "case statement outside of a switch"
       | Some (block_id,_) ->
         (match eval_const_expr expr with
          | None ->
            fail_at loc "case expression must be a constant expression"
          | Some v ->
            (match curr_switch_ctx ctx with
             | None -> fail_at loc "case statement outside of a switch"
             | Some switch_ctx ->
               if has_case switch_ctx v then
                 fail_at loc "duplicate case value"
               else
                 (add_switch_case ctx v block_id,
                  Case (loc, expr, Some (v, block_id))))))
    | Default (loc, _) ->
      (match curr_switch_id ctx with
       | None -> fail_at loc "default statement outside of a switch"
       | Some (block_id, _) ->
         (match curr_switch_ctx ctx with
          | None -> fail_at loc "default statement outside of a switch"
          | Some switch_ctx ->
            if has_default switch_ctx then
              fail_at loc "duplicate default switch case"
            else
              (add_switch_default ctx (Some block_id),
               Default (loc, Some block_id))))
    | Null -> (ctx, stmt)       
  and label_optional_stmt ctx optional_stmt =
    match optional_stmt with
    | None -> (ctx, None)
    | Some stmt ->
       let (ctx, stmt) = label_stmt ctx stmt true in
       (ctx, Some stmt)
  and label_block_item (ctx, items) block_item =
    match block_item with
    | D (Declaration (loc, var_name, _)) ->
      if in_switch_block ctx then
        (match curr_switch_ctx ctx with
         | None -> (ctx, block_item :: items)
         | Some switch_ctx ->
           if not switch_ctx.got_case then
             (warn_at loc "declaration in switch cannot be initialized";
              (ctx, D (Declaration (loc, var_name, None)) :: items))
           else if not (has_default switch_ctx) then
             fail_at loc
               "declaration not allowed within non-default switch case"
           else
             (ctx, block_item :: items))
      else
       (ctx, block_item::items)
    | S stmt ->
       let (ctx, stmt) = label_stmt ctx stmt true in
       (ctx, (S stmt) :: items)
  and label_block_items ctx block_items =
    let (ctx, block_items) =
      List.fold_left label_block_item (ctx,[]) block_items in
    (ctx, List.rev block_items) in
  let label_function_loops ctx (FunctionDef (loc, func_name, block_items)) =
    let ctx = enter_func ctx func_name in
    let (ctx, block_items) = label_block_items ctx block_items in
    let ctx = leave_func ctx in
    (ctx, FunctionDef (loc, func_name, block_items))
  in
  let (_, func_def) = (label_function_loops ctx func_def) in
  (ctx, Program func_def)
