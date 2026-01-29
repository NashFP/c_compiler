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
