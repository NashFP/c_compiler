module StringMap = Map.Make(String)

type reg_type = AX | R10
type operand_type = Imm of int64 | Reg of reg_type | Pseudo of string |
    Stack of int
type unary_operator = Neg | Not
type instruction = Mov of operand_type * operand_type
    | Unary of unary_operator * operand_type
    | AllocateStack of int
    | Ret
type function_definition = Function of string * instruction list
type program_type = Program of function_definition

let convert_unop unop =
  match unop with
  | Tacky.Complement -> Not
  | Tacky.Negate -> Neg

let convert_operand operand =
  match operand with
  | Tacky.ConstantInt i -> Imm i
  | Tacky.Var s -> Pseudo s

let generate_asm_instr instrs instr =
  match instr with
  | Tacky.Return v -> Ret :: Mov (convert_operand v, Reg AX) :: instrs
  | Tacky.Unary (unop, src, dst) ->
      Unary (convert_unop unop, convert_operand dst) ::
      Mov (convert_operand src, convert_operand dst) :: instrs
  
let generate_asm_func (Tacky.Function (name, instrs)) =
  let instrs_rev = List.fold_left generate_asm_instr [] instrs in
  Function (name, List.rev instrs_rev)

let generate_asm_program (Tacky.Program func_def) =
  Program (generate_asm_func func_def)

let pseudo_to_stack var_map stack_size s =
  match StringMap.find_opt s var_map with
  | Some v -> (var_map, stack_size, Stack v)
  | None -> (StringMap.add s (stack_size-4) var_map, stack_size - 4,
    Stack (stack_size - 4))

let replace_op_pseudo var_map stack_size op =
  match op with
  | Pseudo s ->
    let (var_map, stack_size, new_op) =
      pseudo_to_stack var_map stack_size s in
      (var_map, stack_size, new_op)
  | rest -> (var_map, stack_size, rest)

let replace_pseudo (var_map, stack_size, instrs) instr =
  match instr with
  | Mov (src, dst) ->
    let (var_map, stack_size, new_src) =
      replace_op_pseudo var_map stack_size src in
    let (var_map, stack_size, new_dst) =
      replace_op_pseudo var_map stack_size dst in
    (var_map, stack_size, (Mov (new_src, new_dst)) :: instrs)
  | Unary (op, operand) ->
    let (var_map, stack_size, new_operand) =
      replace_op_pseudo var_map stack_size operand in
      (var_map, stack_size, (Unary (op, new_operand)) :: instrs)
  | rest -> (var_map, stack_size, rest :: instrs)

let replace_pseudo_func (Function (name, instrs)) =
  let (_, stack_size, new_instrs) =
    List.fold_left replace_pseudo (StringMap.empty, 0, []) instrs in
  (stack_size, Function (name, List.rev new_instrs))

let replace_pseudo_program (Program func_def) =
  let (stack_size, new_func) = replace_pseudo_func func_def in
  (stack_size, Program new_func)

let fixup_instr instrs instr =
  match instr with
  | Mov (Stack src, Stack dst) ->
    (Mov (Reg R10, Stack dst)) ::
    (Mov (Stack src, Reg R10)) :: instrs
  | rest -> rest :: instrs

let fixup_func (Function (name, instrs)) stack_size =
  let new_instrs = List.fold_left fixup_instr [] instrs in
  Function (name, (AllocateStack (-stack_size) :: List.rev new_instrs))

let fixup_program (Program func_def) stack_size =
  Program (fixup_func func_def stack_size)

let emit_operand operand =
  match operand with
  | Reg AX -> "%eax"
  | Reg R10 -> "%r10d"
  | Stack v -> Printf.sprintf "%d(%%rbp)" v
  | Imm v -> Printf.sprintf "$%Ld" v
  | Pseudo _ -> failwith "Pseudo register not removed"

let emit_unop unop =
  match unop with
  | Neg -> "negl"
  | Not -> "notl"

let emit_instr instrs instr =
  match instr with
  | Mov(src, dst) -> (Printf.sprintf "    movl   %s, %s\n"
    (emit_operand src) (emit_operand dst)) :: instrs
  | Unary (op, operand) ->
    (Printf.sprintf "    %s    %s\n"
      (emit_unop op) (emit_operand operand)) :: instrs
  | Ret -> "    ret\n" :: "    popq    %rbp\n" ::
    "    movq    %rbp, %rsp\n" :: instrs
  | AllocateStack v ->
    (Printf.sprintf "    subq    $%d, %%rsp\n" v) :: instrs
    
let emit_func (Function (name, instrs)) =
  (Printf.sprintf "    .globl %s\n" name)::
  (Printf.sprintf "%s:\n" name)::
   "    pushq    %rbp\n"::
   "    movq     %rsp, %rbp\n"::
  (List.rev (List.fold_left emit_instr [] instrs))

let emit_program (Program func_def) =
  let func_lines = emit_func func_def in
  func_lines @ ["    .section .note.GNU-stack,\"\",@progbits\n"]
