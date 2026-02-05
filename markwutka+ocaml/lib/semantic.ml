open C_ast
open Context

let resolve_variables ctx (Program func_def) =
  let rec resolve_expr ctx expr =
    match expr with
    | Var (loc, var_name) ->
      (match StringMap.find_opt var_name ctx.func_vars with
       | None -> Context.fail_at loc
                   (Printf.sprintf "Undeclared variable: %s" var_name)
       | Some v -> (ctx, Var (loc, v.unique_var_name)))
    | Assignment (loc, (Var (vloc, var_name)), expr) ->
      (match StringMap.find_opt var_name ctx.func_vars with
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
      (ctx, Unary (loc, op, expr))
  in              
  let resolve_statement ctx stmt =
    match stmt with
    | Expression (loc, expr) ->
      let (ctx, expr) = resolve_expr ctx expr in
      (ctx, Expression (loc, expr))
    | Return (loc, expr) ->
      let (ctx, expr) = resolve_expr ctx expr in
      (ctx, Return (loc, expr))
    | Null -> (ctx, Null) in
  let resolve_declaration ctx (Declaration (loc, var_name, var_exp)) =
    let (ctx, unique_var) = make_unique_var ctx loc var_name in
    match var_exp with
    | Some var_exp -> let (ctx, var_exp) = resolve_expr ctx var_exp in
      (ctx, Declaration (loc, unique_var.unique_var_name, Some var_exp))
    | None -> (ctx, Declaration (loc, unique_var.unique_var_name, None)) in
  let resolve_block_item (ctx, block_items) block_item =
    match block_item with
    | D decl -> let (ctx, decl) = resolve_declaration ctx decl in
      (ctx, (D decl) :: block_items)
    | S stmt -> let (ctx, stmt) = resolve_statement ctx stmt in
      (ctx, (S stmt) :: block_items) in
  let resolve_function ctx (FunctionDef (loc, func_name, block_items)) =
    let ctx = context_in_func ctx func_name in
    let (ctx,block_items) = List.fold_left resolve_block_item (ctx,[]) block_items in
    (ctx, FunctionDef (loc, func_name, List.rev block_items))
  in
  let (ctx, func_def) = resolve_function ctx func_def in
  (ctx, Program func_def)
                        
  
  
