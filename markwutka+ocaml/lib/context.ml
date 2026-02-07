module StringMap = Map.Make(String)
module Int64Set = Set.Make(Int64)

type var_type = { orig_var_name: string; unique_var_name: string }

type block_type = BlockWhile | BlockDoWhile | BlockFor | BlockSwitch |
                  BlockStatements
type switch_ctx_type = { got_case: bool; got_default: bool;
                         cases: Int64Set.t }
                         
type context_type = { global_counter: int; func_name: string;
                      func_next_temp_num: int;
                      func_vars_stack: (var_type StringMap.t) list;
                      func_labels: int StringMap.t;
                      block_stack: (string * block_type) list;
                      switch_stack: switch_ctx_type list;
                    }

let make_context = { global_counter=0; func_name="";
                     func_next_temp_num=0;
                     func_vars_stack=[StringMap.empty];
                     func_labels=StringMap.empty;
                     block_stack=[];
                     switch_stack=[];
                   }

let fail_at (C_ast.Location (filename, line, column)) message =
  Printf.printf "%s, line %d, column %d: %s\n" filename line column message;
  exit 1

let warn_at (C_ast.Location (filename, line, column)) message =
  Printf.printf "Warning: %s, line %d, column %d: %s\n" filename line column message

let make_func_temporary ctx =
  ( { ctx with func_next_temp_num = ctx.func_next_temp_num + 1 },
    Printf.sprintf "%s.%d" ctx.func_name ctx.func_next_temp_num )

let make_func_label ctx prefix =
  match StringMap.find_opt prefix ctx.func_labels with
  | Some count ->
    let new_label = Printf.sprintf "%s_%s.%d" ctx.func_name prefix count in
    ({ctx with func_labels=StringMap.add prefix (count+1) ctx.func_labels},
     new_label)
  | None ->
    let new_label = Printf.sprintf "%s_%s.0" ctx.func_name prefix in
    ({ctx with func_labels=StringMap.add prefix 1 ctx.func_labels},
     new_label)

let lookup_var ctx var_name =
  let rec lookup_var_1 func_vars_stack =
    match func_vars_stack with
    | [] -> None
    | func_vars :: func_vars_stack ->
      (match StringMap.find_opt var_name func_vars with
       | Some v -> Some v
       | None -> lookup_var_1 func_vars_stack)
  in
  lookup_var_1 ctx.func_vars_stack
    
let make_unique_var ctx loc var_name =
  match StringMap.find_opt var_name (List.hd ctx.func_vars_stack) with
  | Some _ -> fail_at loc
                (Printf.sprintf "Duplicate variable declaration: %s"
                   var_name)
  | None ->
    let unique_var_name = Printf.sprintf "%s.%d" var_name
        ctx.global_counter in
    let unique_var = {orig_var_name=var_name;
                      unique_var_name=unique_var_name} in
    let func_vars = List.hd ctx.func_vars_stack in
    let func_vars = StringMap.add var_name unique_var func_vars in
    ({ctx with global_counter=ctx.global_counter+1;
               func_vars_stack=func_vars :: List.tl ctx.func_vars_stack },
     unique_var)

let curr_block_id ctx =
  match ctx.block_stack with
  | [] -> None
  | l :: _ -> Some l

let curr_loop_id ctx =
  let is_loop (_,block_type) =
    match block_type with
    | BlockSwitch -> false
    | BlockStatements -> false
    | _ -> true in
  List.find_opt is_loop ctx.block_stack

let curr_breakable_id ctx =
  let is_loop (_,block_type) =
    match block_type with
    | BlockStatements -> false
    | _ -> true in
  List.find_opt is_loop ctx.block_stack

let curr_switch_id ctx =
  let is_switch (_,block_type) =
    match block_type with
    | BlockSwitch -> true
    | _ -> false in
  List.find_opt is_switch ctx.block_stack

let curr_switch_ctx ctx =
  match ctx.switch_stack with
  | [] -> None
  | switch_ctx :: _ -> Some switch_ctx

let add_switch_case ctx v =
  match ctx.switch_stack with
  | switch_ctx :: rest ->
    { ctx with switch_stack =
                 {switch_ctx with got_case=true;
                                  cases=Int64Set.add v switch_ctx.cases}
                 :: rest}
  | _ -> ctx
    
let add_switch_default ctx =
  match ctx.switch_stack with
  | switch_ctx :: rest ->
    { ctx with switch_stack =
                 {switch_ctx with got_default=true} :: rest}
  | _ -> ctx
    
let in_switch_block ctx =
  match curr_block_id ctx with
  | Some (_, BlockSwitch) -> true
  | _ -> false
    
let enter_block ctx block_type =
  let (ctx, block_prefix) =
    match block_type with
    | BlockWhile -> (ctx, "while")
    | BlockDoWhile -> (ctx, "do_while")
    | BlockFor -> (ctx, "for")
    | BlockStatements -> (ctx, "")
    | BlockSwitch ->
        let new_switch_ctx = {got_case=false; got_default=false;
                              cases=Int64Set.empty} in
        ({ctx with switch_stack=new_switch_ctx::ctx.switch_stack},
         "switch") in
  let block_name = Printf.sprintf "%s_%s.%d" ctx.func_name
                                 block_prefix ctx.global_counter in
  ({ ctx with block_stack=(block_name,block_type) :: ctx.block_stack;
              global_counter=ctx.global_counter+1;
              func_vars_stack=StringMap.empty :: ctx.func_vars_stack },
   block_name)

let enter_statements_block ctx =
  let (ctx, _) = enter_block ctx BlockStatements in
  ctx
    
let leave_block ctx =
  let (_,block_type) = List.hd ctx.block_stack in
  match block_type with
  | BlockSwitch ->
    { ctx with block_stack=List.tl ctx.block_stack;
               switch_stack=List.tl ctx.switch_stack;
               func_vars_stack=List.tl ctx.func_vars_stack}
  | _ ->
    { ctx with block_stack=List.tl ctx.block_stack;
               func_vars_stack=List.tl ctx.func_vars_stack}

let enter_func ctx func_name =
  { ctx with func_name=func_name }

let leave_func ctx = ctx
