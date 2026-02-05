module StringMap = Map.Make(String)

type var_type = { orig_var_name: string; unique_var_name: string }
                
type context_type = { global_counter: int; func_name: string;
                      func_next_temp_num: int;
                      func_vars: var_type StringMap.t;
                    func_labels: int StringMap.t}

let make_context = { global_counter=0; func_name="";
                     func_next_temp_num=0;
                     func_vars=StringMap.empty;
                     func_labels=StringMap.empty;
                   }

let fail_at (C_ast.Location (filename, line, column)) message =
  Printf.printf "%s, line %d, column %d: %s\n" filename line column message;
  exit 1

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
    
let make_unique_var ctx loc var_name =
  match StringMap.find_opt var_name ctx.func_vars with
  | Some _ -> fail_at loc
                (Printf.sprintf "Duplicate variable declaration: %s"
                   var_name)
  | None ->
    let unique_var_name = Printf.sprintf "%s.%d" var_name
        ctx.global_counter in
    let unique_var = {orig_var_name=var_name;
                      unique_var_name=unique_var_name} in
    ({ctx with global_counter=ctx.global_counter+1;
               func_vars=StringMap.add var_name unique_var ctx.func_vars},
     unique_var)

let context_in_func ctx func_name =
  { ctx with func_name=func_name }
     
