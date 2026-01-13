type asdl_operand_type = Op_Immediate of int64 | Op_Register

type asdl_instruction_type =
  Inst_Mov of asdl_operand_type * asdl_operand_type | Inst_Ret

type asdl_function_type =
  Func_Def of string * asdl_instruction_type list

type asdl_program_type = ASDL_Program of asdl_function_type

let generate_asdl_expr (C_ast.ConstantInt (_, i)) =
  Inst_Mov (Op_Immediate i, Op_Register)

let generate_asdl_stmt (C_ast.StmtReturn (_, expr)) =
  [generate_asdl_expr expr; Inst_Ret]

let generate_asdl_func (C_ast.FunctionDef (_, (name, stmt))) =
  Func_Def (name, generate_asdl_stmt stmt)

let generate_asdl (C_ast.Program func_type) =
  ASDL_Program (generate_asdl_func func_type)

let op_str operand =
  match operand with
  | Op_Immediate i -> Printf.sprintf "$%Ld" i
  | Op_Register -> "%eax"

let generate_asm_instr out_file instr =
  match instr with
  | Inst_Mov (src, dst) -> Printf.fprintf out_file "    movl %s,%s\n"
      (op_str src) (op_str dst)
  | Inst_Ret -> Printf.fprintf out_file "    ret\n"

let generate_asm_func out_file (Func_Def (name, instrs)) =
  Printf.fprintf out_file "    .globl %s\n" name;
  Printf.fprintf out_file "%s:\n" name;
  List.iter (generate_asm_instr out_file) instrs

let generate_asm asm_filename (ASDL_Program func_def) = 
  let out_file = open_out asm_filename in
  (generate_asm_func out_file func_def ;
    Printf.fprintf out_file "    .section .note.GNU-stack,\"\",@progbits\n")
