module StringMap = Map.Make(String)

type operand_type = Imm of int64 | Var of string
type unary_operator = Neg | Not
type binary_operator = Add | Sub | Mult | ShiftLeft | ShiftRight |
                       BitwiseAnd | BitwiseOr | BitwiseXor | Div |
                       Remainder
type instruction =
    Binary of binary_operator * operand_type * operand_type * operand_type
  | Ret of operand_type
    
type function_definition = Function of string * instruction list
type program_type = Program of function_definition

let convert_unop unop =
  match unop with
  | Tacky.Complement -> Not
  | Tacky.Negate -> Neg

let convert_binop binary_operator =
  match binary_operator with
  | Tacky.Add -> Add
  | Tacky.Multiply -> Mult
  | Tacky.Subtract -> Sub
  | Tacky.Divide -> Div
  | Tacky.Remainder -> Remainder
  | Tacky.ShiftLeft -> ShiftLeft
  | Tacky.ShiftRight -> ShiftRight
  | Tacky.BitwiseAnd -> BitwiseAnd
  | Tacky.BitwiseOr -> BitwiseOr
  | Tacky.BitwiseXor -> BitwiseXor
         
let convert_operand operand =
  match operand with
  | Tacky.ConstantInt i -> Imm i
  | Tacky.Var s -> Var s

let generate_asm_instr instrs instr =
  match instr with
  | Tacky.Return v -> Ret (convert_operand v) :: instrs
  | Tacky.Unary (Complement, src, dst) ->
    Binary (BitwiseXor, Imm (-1L), convert_operand src, convert_operand dst) :: instrs
  | Tacky.Unary (Negate, src, dst) ->
    Binary (Sub, Imm 0L, convert_operand src, convert_operand dst) :: instrs
  | Tacky.Binary (binary_operator, src1, src2, dst) ->
    Binary (convert_binop binary_operator,
            convert_operand src1,
            convert_operand src2,
            convert_operand dst) :: instrs
  
let generate_asm_func (Tacky.Function (name, instrs)) =
  let instrs_rev = List.fold_left generate_asm_instr [] instrs in
  Function (name, List.rev instrs_rev)

let generate_asm_program (Tacky.Program func_def) =
  Program (generate_asm_func func_def)

let emit_operand operand =
  match operand with
  | Var v -> Printf.sprintf "%%%s" v
  | Imm v -> Printf.sprintf "%Ld" v

let emit_binop = function
  | Add -> "add"
  | Sub -> "sub"
  | Mult -> "mul"
  | Div -> "sdiv"
  | Remainder -> "srem"
  | BitwiseAnd -> "and"
  | BitwiseOr -> "or"
  | BitwiseXor -> "xor"
  | ShiftLeft -> "shl"
  | ShiftRight -> "ashr"
           
let emit_instr instrs instr =
  match instr with
  | Binary (op, src1, src2, dst) ->
    (Printf.sprintf "    %s =  %s i32 %s, %s\n"
       (emit_operand dst) (emit_binop op)
       (emit_operand src1) (emit_operand src2)) :: instrs
  | Ret v -> (Printf.sprintf "    ret i32 %s\n" (emit_operand v)) :: instrs
    
let emit_func (Function (name, instrs)) =
  (Printf.sprintf "define i32 @%s() { \n" name)::
  ((List.rev (List.fold_left emit_instr [] instrs)) @ ["}\n"])

let emit_program (Program func_def) =
  let func_lines = emit_func func_def in
  func_lines
