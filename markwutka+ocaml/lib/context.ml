module StringMap = Map.Make(String)
module Int64Map = Map.Make(Int64)

type identifier_id_type = IdVar | IdFuncForward | IdFunc
type identifier_type = { id_type : identifier_id_type;
                         orig_name: string; unique_name: string;
                       has_linkage: bool; args: (string list) option}

type block_type = BlockWhile | BlockDoWhile | BlockFor | BlockSwitch |
                  BlockStatements
type switch_ctx_type = { got_case: bool; opt_default : string option;
                         cases: string Int64Map.t }
                         
type context_type = { global_counter: int; func_name: string;
                      func_next_temp_num: int;
                      identifier_stack: (identifier_type StringMap.t) list;
                      global_identifiers: identifier_type StringMap.t;
                      func_labels: int StringMap.t;
                      block_stack: (string * block_type) list;
                      switch_stack: switch_ctx_type list;
                    }

let make_context = { global_counter=0; func_name="";
                     func_next_temp_num=0;
                     identifier_stack=[StringMap.empty];
                     global_identifiers=StringMap.empty;
                     func_labels=StringMap.empty;
                     block_stack=[];
                     switch_stack=[];
                   }

let map_with_ctx fn ctx items =
  let folder (ctx,items) item =
    let (ctx, item) = fn ctx item in
    (ctx, item :: items) in
  let (ctx, items) = List.fold_left folder (ctx,[]) items in
  (ctx, List.rev items)

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

let lookup_identifier ctx ident =  
  let rec lookup_ident_1 identifier_stack in_current_scope =
    match identifier_stack with
    | [] ->
       (match StringMap.find_opt ident ctx.global_identifiers with
        | None -> (None, false)
        | Some v -> (Some v, false))
    | identifiers :: identifier_stack ->
      (match StringMap.find_opt ident identifiers with
       | Some v -> (Some v, in_current_scope)
       | None -> lookup_ident_1 identifier_stack false)
  in
  lookup_ident_1 ctx.identifier_stack true

let compare_args args1 args2 =
  (List.length args1) == (List.length args2)

let register_identifier ctx loc ident_name id_type has_linkage opt_args =
  let symbol_map = if has_linkage then ctx.global_identifiers else
                  List.hd ctx.identifier_stack in
  match StringMap.find_opt ident_name symbol_map with
  | Some ident ->
     if not (Option.equal compare_args opt_args ident.args) ||
          (ident.id_type == IdVar && id_type == IdVar) then
       fail_at loc (Printf.sprintf "Duplicate identifier declared: %s"
                      ident_name)
     else
       (ctx, ident)
  | None ->
     let ctx =
       if has_linkage then
         let new_ident = {id_type=id_type; orig_name=ident_name;
                          unique_name=ident_name;
                          has_linkage=has_linkage; args=opt_args} in
         let identifiers = StringMap.add ident_name new_ident symbol_map in         
         { ctx with global_identifiers=identifiers}
       else
         ctx in
     let unique_name = Printf.sprintf "%s.%d" ident_name ctx.global_counter in
     let new_ident = {id_type=id_type;
                      orig_name=ident_name;
                      unique_name=unique_name;
                      has_linkage=has_linkage; args=opt_args} in
     let identifiers = StringMap.add ident_name new_ident
                         (List.hd ctx.identifier_stack) in
     ({ctx with global_counter=ctx.global_counter+1;
                identifier_stack=
                  identifiers :: List.tl ctx.identifier_stack },
      new_ident)
       
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

let add_switch_case ctx v label =
  match ctx.switch_stack with
  | switch_ctx :: rest ->
    { ctx with switch_stack =
                 {switch_ctx with got_case=true;
                                  cases=Int64Map.add v label switch_ctx.cases}
                 :: rest}
  | _ -> ctx
    
let add_switch_default ctx label =
  match ctx.switch_stack with
  | switch_ctx :: rest ->
    { ctx with switch_stack =
                 {switch_ctx with opt_default=label} :: rest}
  | _ -> ctx
    
let in_switch_block ctx =
  match curr_block_id ctx with
  | Some (_, BlockSwitch) -> true
  | _ -> false

let get_switch_cases switch_ctx =
  Int64Map.to_list switch_ctx.cases

let get_switch_default switch_ctx =
  switch_ctx.opt_default

let has_case switch_ctx v =
  Int64Map.mem v switch_ctx.cases

let has_default switch_ctx =
  Option.is_some switch_ctx.opt_default
    
let enter_block ctx block_type =
  let (ctx, block_prefix) =
    match block_type with
    | BlockWhile -> (ctx, "while")
    | BlockDoWhile -> (ctx, "do_while")
    | BlockFor -> (ctx, "for")
    | BlockStatements -> (ctx, "")
    | BlockSwitch ->
        let new_switch_ctx = {got_case=false; opt_default=None;
                              cases=Int64Map.empty} in
        ({ctx with switch_stack=new_switch_ctx::ctx.switch_stack},
         "switch") in
  let block_name = Printf.sprintf "%s_%s.%d" ctx.func_name
                                 block_prefix ctx.global_counter in
  ({ ctx with block_stack=(block_name,block_type) :: ctx.block_stack;
              global_counter=ctx.global_counter+1;
              identifier_stack=StringMap.empty :: ctx.identifier_stack },
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
               identifier_stack=List.tl ctx.identifier_stack}
  | _ ->
    { ctx with block_stack=List.tl ctx.block_stack;
               identifier_stack=List.tl ctx.identifier_stack}

let enter_func ctx func_name =
  { ctx with func_name=func_name }

let leave_func ctx = ctx
