module StringMap = Map.Make(String)

type size_type = Size_1 | Size_4 | Size_8
type reg_type = AX | DX | R10 | R11 | CX | DI | SI | R8 | R9
type cond_code = E | NE | G | GE | L | LE
type operand_type = Imm of int64 | Reg of reg_type | Pseudo of string |
    Stack of int
type unary_operator = Neg | Not
type binary_operator = Add | Sub | Mult | ShiftLeft | ShiftRight |
                       BitwiseAnd | BitwiseOr | BitwiseXor |
                       Equal | NotEqual | LessThan | LessOrEqual |
                       GreaterThan | GreaterOrEqual |
                       Divide | Remainder | And | Or
type instruction =
    Mov of operand_type * operand_type
  | Unary of unary_operator * operand_type
  | Binary of binary_operator * operand_type * operand_type
  | Idiv of operand_type
  | Cdq
  | AllocateStack of int
  | Cmp of operand_type * operand_type
  | Jmp of string
  | JmpCC of cond_code * string
  | SetCC of cond_code * operand_type
  | Label of string
  | DeallocateStack of int
  | Push of operand_type
  | Call of string
  | Ret
    
type function_definition = Function of string * instruction list * int
type program_type = Program of function_definition list

let calling_regs = [ DI; SI; DX; CX; R8; R9 ]

let (<::) lst item = item :: lst

let rec pair_regs args regs pairs =
  match args with
  | [] -> pairs
  | arg :: args ->
     if not (List.is_empty regs) then
       pair_regs args (List.tl regs) ((arg, List.hd regs) :: pairs)
     else
       pairs

let convert_unop unop =
  match unop with
  | Tacky.Complement -> Not
  | Tacky.Negate -> Neg
  | Tacky.Not -> Not

let convert_binop binary_operator =
  match binary_operator with
  | Tacky.Add -> Add
  | Tacky.Multiply -> Mult
  | Tacky.Subtract -> Sub
  | Tacky.ShiftLeft -> ShiftLeft
  | Tacky.ShiftRight -> ShiftRight
  | Tacky.BitwiseAnd -> BitwiseAnd
  | Tacky.BitwiseOr -> BitwiseOr
  | Tacky.BitwiseXor -> BitwiseXor
  | Tacky.Divide -> Divide
  | Tacky.Remainder -> Remainder
  | Tacky.Equal -> Equal
  | Tacky.NotEqual -> NotEqual
  | Tacky.LessThan -> LessThan
  | Tacky.LessOrEqual -> LessOrEqual
  | Tacky.GreaterThan -> GreaterThan
  | Tacky.GreaterOrEqual -> GreaterOrEqual
  | Tacky.And -> And
  | Tacky.Or -> Or
         
let convert_operand operand =
  match operand with
  | Tacky.ConstantInt i -> Imm i
  | Tacky.Var s -> Pseudo s

let generate_asm_instr instrs instr =
  match instr with
  | Tacky.Return v -> instrs <:: Mov (convert_operand v, Reg AX) <:: Ret
  | Tacky.Unary (Tacky.Not, src, dst) ->
    instrs
    <:: Cmp (Imm 0L, convert_operand src)
    <:: Mov (Imm 0L, convert_operand dst)
    <:: SetCC (E, convert_operand dst)
  | Tacky.Unary (unop, src, dst) ->
    instrs
    <:: Mov (convert_operand src, convert_operand dst)
    <:: Unary (convert_unop unop, convert_operand dst)
  | Tacky.Binary (Divide, src1, src2, dst) ->
    instrs
    <:: Mov (convert_operand src1, Reg AX)
    <:: Cdq
    <:: Idiv (convert_operand src2)
    <:: Mov (Reg AX, convert_operand dst)
  | Tacky.Binary (Remainder, src1, src2, dst) ->
    instrs
    <:: Mov (convert_operand src1, Reg AX)
    <:: Cdq
    <:: Idiv (convert_operand src2)
    <:: Mov (Reg DX, convert_operand dst)
  | Tacky.Binary (Tacky.Equal, src1, src2, dst) ->
    let dst = convert_operand dst in
    instrs
    <:: Cmp (convert_operand src2, convert_operand src1)
    <:: Mov (Imm 0L, dst)
    <:: SetCC (E, dst)
  | Tacky.Binary (Tacky.NotEqual, src1, src2, dst) ->
    let dst = convert_operand dst in
    instrs
    <:: Cmp (convert_operand src2, convert_operand src1)
    <:: Mov (Imm 0L, dst)
    <:: SetCC (NE, dst)
  | Tacky.Binary (Tacky.LessThan, src1, src2, dst) ->
    let dst = convert_operand dst in
    instrs
    <:: Cmp (convert_operand src2, convert_operand src1)
    <:: Mov (Imm 0L, dst)
    <:: SetCC (L, dst)
  | Tacky.Binary (Tacky.LessOrEqual, src1, src2, dst) ->
    let dst = convert_operand dst in
    instrs
    <:: Cmp (convert_operand src2, convert_operand src1)
    <:: Mov (Imm 0L, dst)
    <:: SetCC (LE, dst)
  | Tacky.Binary (Tacky.GreaterThan, src1, src2, dst) ->
    let dst = convert_operand dst in
    instrs
    <:: Cmp (convert_operand src2, convert_operand src1)
    <:: Mov (Imm 0L, dst)
    <:: SetCC (G, dst)
  | Tacky.Binary (Tacky.GreaterOrEqual, src1, src2, dst) ->
    let dst = convert_operand dst in
    instrs
    <:: Cmp (convert_operand src2, convert_operand src1)
    <:: Mov (Imm 0L, dst)
    <:: SetCC (GE, dst)
 | Tacky.Binary (binary_operator, src1, src2, dst) ->
   instrs
   <:: Mov (convert_operand src1, convert_operand dst)
   <::  Binary (convert_binop binary_operator, convert_operand src2,
                convert_operand dst)
  | Tacky.Copy (src, dst) ->
    instrs
    <:: Mov (convert_operand src, convert_operand dst)
  | Tacky.Label str -> instrs <:: Label str
  | Tacky.Jump str -> instrs <:: Jmp str
  | Tacky.JumpIfZero (src, str) ->
    instrs
    <:: Cmp (Imm 0L, convert_operand src)
    <:: JmpCC (E, str)
  | Tacky.JumpIfNotZero (src, str) ->
    instrs
    <:: Cmp (Imm 0L, convert_operand src)
    <:: JmpCC (NE, str)
  | Tacky.FunctionCall (label, args, dst) ->
     let gen_move_to_reg instrs (arg,reg) =
       instrs <:: Mov (convert_operand arg, Reg reg) in
     let gen_move_to_stack instrs arg =       
       match convert_operand arg with
       | Reg reg -> instrs <:: Push (Reg reg)
       | Imm v -> instrs <:: Push (Imm v)
       | arg -> instrs
              <:: Mov (arg, Reg(AX))
              <:: Push (Reg AX) in

     let stack_padding = if (List.length args) mod 2 == 1 then 8 else 0 in
     let instrs = if stack_padding > 0 then
                    instrs <:: AllocateStack stack_padding
                  else
                    instrs in
     let call_reg_pairs = pair_regs args calling_regs [] in
     let instrs = List.fold_left gen_move_to_reg instrs call_reg_pairs in
     let args_rest = List.rev (List.drop 6 args) in
     let instrs = List.fold_left gen_move_to_stack instrs args_rest in
     let instrs = instrs <:: Call label in
     let bytes_to_remove = 8 * (List.length args_rest) + stack_padding in
     let instrs = if bytes_to_remove > 0 then
                    instrs <:: DeallocateStack bytes_to_remove
                  else
                    instrs in
     instrs <:: Mov (Reg AX, convert_operand dst)
  
let generate_asm_func (Tacky.Function (name, args, instrs)) =
  let asm_instrs = [] in
  let gen_move_reg_to_stack instrs (reg,arg) =
    instrs <:: Mov (Reg reg, convert_operand arg) in
  let gen_move_to_stack (instrs, stack_offset) arg =
    (instrs <:: Mov (Stack stack_offset, convert_operand arg),
     stack_offset + 8) in
  let call_reg_pairs = pair_regs calling_regs args [] in
  let asm_instrs = List.fold_left gen_move_reg_to_stack
                     asm_instrs call_reg_pairs in
  let args_rest = List.drop 6 args in
  let (asm_instrs, _) = List.fold_left gen_move_to_stack
                      (asm_instrs, 16) args_rest in
  let instrs_rev = List.fold_left generate_asm_instr asm_instrs instrs in
  Function (name, List.rev instrs_rev, 0)

let generate_asm_program (Tacky.Program func_defs) =
  Program (List.map generate_asm_func func_defs)

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
  | Binary (op, op1, op2) ->
    let (var_map, stack_size, new_operand1) =
      replace_op_pseudo var_map stack_size op1 in
    let (var_map, stack_size, new_operand2) =
      replace_op_pseudo var_map stack_size op2 in
    (var_map, stack_size, (Binary (op, new_operand1, new_operand2)) :: instrs)
  | Idiv operand ->
    let (var_map, stack_size, new_operand) =
      replace_op_pseudo var_map stack_size operand in
    (var_map, stack_size, Idiv new_operand :: instrs)
  | Cmp (src1, src2) ->
    let (var_map, stack_size, new_src1) =
      replace_op_pseudo var_map stack_size src1 in
    let (var_map, stack_size, new_src2) =
      replace_op_pseudo var_map stack_size src2 in
    (var_map, stack_size, Cmp (new_src1, new_src2) :: instrs)
  | SetCC (cc, src) ->
    let (var_map, stack_size, new_src) =
      replace_op_pseudo var_map stack_size src in
    (var_map, stack_size, SetCC (cc, new_src) :: instrs)
  | Push _ ->
     (var_map, stack_size - 8, instr :: instrs)      
  | rest -> (var_map, stack_size, rest :: instrs)

let replace_pseudo_func (Function (name, instrs,_)) =
  let (_, stack_size, new_instrs) =
    List.fold_left replace_pseudo (StringMap.empty, 0, []) instrs in
  Function (name, List.rev new_instrs, -stack_size)

let replace_pseudo_program (Program func_defs) =  
   Program (List.map replace_pseudo_func func_defs)

let fixup_instr instrs instr =
  match instr with
  | Mov (Stack src, Stack dst) ->
    instrs
    <:: Mov (Stack src, Reg R10)
    <:: Mov (Reg R10, Stack dst)
  | Idiv src ->
    instrs
    <:: Mov (src, Reg R10)
    <:: Idiv (Reg R10)
  | Binary (Add, Stack src, Stack dst) ->
    instrs
    <:: Mov (Stack src, Reg R10)
    <:: Binary (Add, Reg R10, Stack dst)
  | Binary (Sub, Stack src, Stack dst) ->
    instrs
    <:: Mov (Stack src, Reg R10)
    <:: Binary (Sub, Reg R10, Stack dst)
  | Binary (Mult, src, Stack dst) ->
    instrs
    <:: Mov (Stack dst, Reg R11)
    <:: Binary (Mult, src, Reg R11)
    <:: Mov (Reg R11, Stack dst)
  | Binary (BitwiseAnd, src, Stack dst) ->
    instrs
    <:: Mov (Stack dst, Reg R11)
    <:: Binary (BitwiseAnd, src, Reg R11)
    <:: Mov (Reg R11, Stack dst)
  | Binary (BitwiseOr, src, Stack dst) ->
    instrs
    <:: Mov (Stack dst, Reg R11)
    <:: Binary (BitwiseOr, src, Reg R11)
    <:: Mov (Reg R11, Stack dst)
  | Binary (BitwiseXor, src, Stack dst) ->
    instrs
    <:: Mov (Stack dst, Reg R11)
    <:: Binary (BitwiseXor, src, Reg R11)
    <:: Mov (Reg R11, Stack dst)
  | Binary (ShiftLeft, src, dst) ->
    instrs
    <:: Mov (src, Reg CX)
    <:: Binary (ShiftLeft, Reg CX, dst)
  | Binary (ShiftRight, src, dst) ->
    instrs
    <:: Mov (src, Reg CX)
    <:: Binary (ShiftRight, Reg CX, dst)
  | Cmp (Stack src1, Stack src2) ->
    instrs
    <:: Mov (Stack src1, Reg R10)
    <:: Cmp (Reg R10, Stack src2)
  | Cmp (src, Imm n) ->
    instrs
    <:: Mov (Imm n, Reg R11)
    <:: Cmp (src, Reg R11)
  | rest -> instrs <:: rest

let fixup_func (Function (name, instrs, stack_size)) =
  let new_instrs = List.fold_left fixup_instr [] instrs in
  let stack_size =
    if stack_size mod 16 > 0 then
      stack_size + (16 - (stack_size mod 16))
    else
      stack_size in
  Function (name, (AllocateStack stack_size :: List.rev new_instrs),
            stack_size)

let fixup_program (Program func_def)  =
  Program (List.map fixup_func func_def)

let reg_name reg size =
  match size with
  | Size_1 ->
     (match reg with
     | AX -> "%al"
     | CX -> "%cl"
     | DX -> "%dl"
     | DI -> "%dil"
     | SI -> "%sil"
     | R8 -> "%r8b"
     | R9 -> "%r9b"
     | R10 -> "%r10b"
     | R11 -> "%r11b")
  | Size_4 ->
     (match reg with
      | AX -> "%eax"
      | CX -> "%ecx"
      | DX -> "%edx"
      | DI -> "%edi"
      | SI -> "%esi"
      | R8 -> "%r8d"
      | R9 -> "%r9d"
      | R10 -> "%r10d"
      | R11 -> "%r11d")
  | Size_8 ->
     (match reg with
      | AX -> "%rax"
      | CX -> "%rcx"
      | DX -> "%rdx"
      | DI -> "%rdi"
      | SI -> "%rsi"
      | R8 -> "%r8"
      | R9 -> "%r9"
      | R10 -> "%r10"
      | R11 -> "%r11")

let emit_operand operand size =
  match operand with
  | Reg reg -> reg_name reg size
  | Stack v -> Printf.sprintf "%d(%%rbp)" v
  | Imm v -> Printf.sprintf "$%Ld" v
  | Pseudo _ -> failwith "Pseudo register not removed"

let emit_unop = function
  | Neg -> "negl"
  | Not -> "notl"

let emit_binop = function
  | Add -> "addl"
  | Sub -> "subl"
  | Mult -> "imull"
  | BitwiseAnd -> "andl"
  | BitwiseOr -> "orl"
  | BitwiseXor -> "xorl"
  | ShiftLeft -> "sall"
  | ShiftRight -> "sarl"
  | _ -> failwith "Tried to emit invalid binop"

let emit_cond_code = function
  | E -> "e"
  | NE -> "ne"
  | L -> "l"
  | LE -> "le"
  | G -> "g"
  | GE -> "ge"

let emit_instr instr =
  match instr with
  | Mov(src, dst) -> (Printf.sprintf "    movl   %s, %s\n"
    (emit_operand src Size_4) (emit_operand dst Size_4))
  | Unary (op, operand) ->
    (Printf.sprintf "    %s    %s\n"
       (emit_unop op) (emit_operand operand Size_4))
  | Binary (ShiftLeft, src, dst) ->
    (Printf.sprintf "    %s    %s, %s\n"
       (emit_binop ShiftLeft) (emit_operand src Size_1)
       (emit_operand dst Size_4))
  | Binary (ShiftRight, src, dst) ->
    (Printf.sprintf "    %s    %s, %s\n"
       (emit_binop ShiftRight) (emit_operand src Size_1)
       (emit_operand dst Size_4))
  | Binary (op, src, dst) ->
    (Printf.sprintf "    %s    %s, %s\n"
       (emit_binop op) (emit_operand src Size_4) (emit_operand dst Size_4))
  | Idiv operand ->
    (Printf.sprintf "    idivl    %s\n" (emit_operand operand Size_4))
  | Cdq -> "    cdq\n"
  | Ret -> "    movq    %rbp, %rsp\n" ^
           "    popq    %rbp\n"^
           "    ret\n"
  | AllocateStack v ->
    (Printf.sprintf "    subq    $%d, %%rsp\n" v)
  | Cmp (src1, src2) ->
    (Printf.sprintf "    cmpl    %s, %s\n" (emit_operand src1 Size_4)
       (emit_operand src2 Size_4))
  | Jmp str -> (Printf.sprintf "    jmp    .L%s\n" str)
  | JmpCC (cc, str) -> (Printf.sprintf "    j%s    .L%s\n"
                          (emit_cond_code cc) str)
  | SetCC (cc, src) -> (Printf.sprintf "    set%s    %s\n"
                          (emit_cond_code cc)
                       (emit_operand src Size_1))
  | Label str -> (Printf.sprintf ".L%s:\n" str)
  | DeallocateStack v ->
     (Printf.sprintf "    addq    $%d, %%rsp\n" v)
  | Push dst ->
     (Printf.sprintf "    pushq   %s\n" (emit_operand dst Size_8))
  | Call label ->
     (Printf.sprintf "    call    %s@PLT\n" label)
     
let emit_func (Function (name, instrs,_)) =
  (Printf.sprintf "    .globl %s\n" name)::
  (Printf.sprintf "%s:\n" name)::
   "    pushq    %rbp\n"::
   "    movq     %rsp, %rbp\n"::
  List.map emit_instr instrs

let emit_program (Program func_defs) =
  let func_lines = List.concat_map emit_func func_defs in
  func_lines @ ["    .section .note.GNU-stack,\"\",@progbits\n"]
