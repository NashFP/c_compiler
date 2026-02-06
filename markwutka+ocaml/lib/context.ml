module StringMap = Map.Make(String)

type var_type = { orig_var_name: string; unique_var_name: string }
                
type context_type = { global_counter: int; func_name: string;
                      func_next_temp_num: int;
                      func_vars_stack: (var_type StringMap.t) list;
                    func_labels: int StringMap.t}

let make_context = { global_counter=0; func_name="";
                     func_next_temp_num=0;
                     func_vars_stack=[StringMap.empty];
                     func_labels=StringMap.empty;
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

let enter_scope ctx =
  { ctx with func_vars_stack=StringMap.empty :: ctx.func_vars_stack }

let leave_scope ctx =
  { ctx with func_vars_stack=List.tl ctx.func_vars_stack 
  }

let enter_func ctx func_name =
  { (enter_scope ctx) with func_name=func_name }

let leave_func ctx = leave_scope ctx
