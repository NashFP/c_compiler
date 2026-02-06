open C_ast
open Context

module StringSet = Set.Make(String)

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
and resolve_statement ctx stmt =
  match stmt with
  | Expression (loc, expr) ->
    let (ctx, expr) = resolve_expr ctx expr in
    (ctx, Expression (loc, expr))
  | Return (loc, expr) ->
    let (ctx, expr) = resolve_expr ctx expr in
    (ctx, Return (loc, expr))
  | If (loc, expr, true_stmt, maybe_false_stmt) ->
    let (ctx, expr) = resolve_expr ctx expr in
    let (ctx, true_stmt) = resolve_statement ctx true_stmt in
    (match maybe_false_stmt with
     | Some false_stmt ->
       let (ctx, false_stmt) = resolve_statement ctx false_stmt in
       (ctx, If (loc, expr, true_stmt, Some false_stmt))
     | None ->
       (ctx, If (loc, expr, true_stmt, None)))
  | Compound (loc, Block block_items) ->
    let ctx = enter_scope ctx in
    let (ctx, block_items) = resolve_block_items ctx block_items in
    let ctx = leave_scope ctx in
    (ctx, Compound (loc, Block block_items))
  | Label (loc,str) -> (ctx, Label (loc,str))
  | Goto (loc,str) -> (ctx, Goto (loc,str))
  | Null -> (ctx, Null)
and resolve_declaration ctx (Declaration (loc, var_name, var_exp)) =
  let (ctx, unique_var) = make_unique_var ctx loc var_name in
  match var_exp with
  | Some var_exp -> let (ctx, var_exp) = resolve_expr ctx var_exp in
    (ctx, Declaration (loc, unique_var.unique_var_name, Some var_exp))
  | None -> (ctx, Declaration (loc, unique_var.unique_var_name, None))
and resolve_block_item (ctx, block_items) block_item =
  match block_item with
  | D decl -> let (ctx, decl) = resolve_declaration ctx decl in
    (ctx, (D decl) :: block_items)
  | S stmt -> let (ctx, stmt) = resolve_statement ctx stmt in
    (ctx, (S stmt) :: block_items)
and resolve_block_items ctx block_items =
  let (ctx, block_items) = List.fold_left resolve_block_item (ctx,[]) block_items in
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
    | S (Label (loc, str)) ->
       if StringMap.mem str labels then
         fail_at loc (Printf.sprintf "Duplicate label %s in function" str)
       else
         StringMap.add str loc labels
    | S (If (_, _, true_stmt, Some false_stmt)) ->
       let labels = find_labels labels (S true_stmt) in
       find_labels labels (S false_stmt)
    | S (If (_, _, true_stmt, None)) ->
       find_labels labels (S true_stmt)
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
       Program (FunctionDef (loc, func_name, block_items)))
    else
      Program (FunctionDef (loc, func_name, block_items))
  in
  verify_function func_def    
