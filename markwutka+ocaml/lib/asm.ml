module StringMap = Map.Make(String)

type reg_type = AX | DX | R10 | R11 | CX | CL
type cond_code = E | NE | G | GE | L | LE
type operand_type = Imm of int64 | Reg of reg_type | Pseudo of string |
    Stack of int
type unary_operator = Neg | Not
type binary_operator = Add | Sub | Mult | ShiftLeft | ShiftRight |
                       BitwiseAnd | BitwiseOr | BitwiseXor |
                       And | Or | Equal | NotEqual | LessThan | LessOrEqual |
                       GreaterThan | GreaterOrEqual
type instruction =
    Mov of operand_type * operand_type
  | Unary of unary_operator * operand_type
  | Binary of binary_operator * operand_type * operand_type
  | Cmp of operand_type * operand_type
  | Idiv of operand_type
  | Cdq
  | Jmp of string
  | JmpCC of cond_code * string
  | SetCC of cond_code * operand_type
  | Label of string
  | AllocateStack of int
  | Ret
    
type function_definition = Function of string * instruction list
type program_type = Program of function_definition

let convert_unop unop =
  match unop with
  | Tacky.Complement -> Not
  | Tacky.Negate -> Neg
  | _ -> failwith "Unsupported unary op"

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
  | Tacky.And -> And
  | Tacky.Or -> Or
  | Tacky.Equal -> Equal
  | Tacky.NotEqual -> NotEqual
  | Tacky.LessThan -> LessThan
  | Tacky.LessOrEqual -> LessOrEqual
  | Tacky.GreaterThan -> GreaterThan
  | Tacky.GreaterOrEqual -> GreaterOrEqual
  | Tacky.Divide -> failwith "Can't convert Divide to asm binary operator"
  | Tacky.Remainder -> failwith "Can't convert Remainder to asm binary operator"
         
let convert_operand operand =
  match operand with
  | Tacky.ConstantInt i -> Imm i
  | Tacky.Var s -> Pseudo s

let generate_asm_instr instrs instr =
  match instr with
  | Tacky.Return v -> Ret :: Mov (convert_operand v, Reg AX) :: instrs
  | Tacky.Unary (Not, src, dst) ->
    let dst = convert_operand dst in
    let src = convert_operand src in
    SetCC (E, dst) :: Mov (Imm 0L, dst) :: Cmp (Imm 0L, src) :: instrs     
  | Tacky.Unary (unop, src, dst) ->
    Unary (convert_unop unop, convert_operand dst) ::
    Mov (convert_operand src, convert_operand dst) :: instrs
  | Tacky.Binary (Divide, src1, src2, dst) ->
    Mov (Reg AX, convert_operand dst) ::
    Idiv (convert_operand src2) ::
    Cdq ::
    Mov (convert_operand src1, Reg AX) :: instrs
  | Tacky.Binary (Remainder, src1, src2, dst) ->
    Mov (Reg DX, convert_operand dst) ::
    Idiv (convert_operand src2) ::
    Cdq ::
    Mov (convert_operand src1, Reg AX) :: instrs
  | Tacky.Binary (Equal, src1, src2, dst) ->
    let dst = convert_operand dst in
    SetCC (E, dst) :: Mov (Imm 0L, dst) ::
    Cmp (convert_operand src2, convert_operand src1) :: instrs
  | Tacky.Binary (NotEqual, src1, src2, dst) ->
    let dst = convert_operand dst in
    SetCC (NE, dst) :: Mov (Imm 0L, dst) ::
    Cmp (convert_operand src2, convert_operand src1) :: instrs
  | Tacky.Binary (LessThan, src1, src2, dst) ->
    let dst = convert_operand dst in
    SetCC (L, dst) :: Mov (Imm 0L, dst) ::
    Cmp (convert_operand src2, convert_operand src1) :: instrs
  | Tacky.Binary (LessOrEqual, src1, src2, dst) ->
    let dst = convert_operand dst in
    SetCC (LE, dst) :: Mov (Imm 0L, dst) ::
    Cmp (convert_operand src2, convert_operand src1) :: instrs
  | Tacky.Binary (GreaterThan, src1, src2, dst) ->
    let dst = convert_operand dst in
    SetCC (G, dst) :: Mov (Imm 0L, dst) ::
    Cmp (convert_operand src2, convert_operand src1) :: instrs
  | Tacky.Binary (GreaterOrEqual, src1, src2, dst) ->
    let dst = convert_operand dst in
    SetCC (GE, dst) :: Mov (Imm 0L, dst) ::
    Cmp (convert_operand src2, convert_operand src1) :: instrs
  | Tacky.Binary (binary_operator, src1, src2, dst) ->
    Binary (convert_binop binary_operator, convert_operand src2,
            convert_operand dst) ::
    Mov (convert_operand src1, convert_operand dst) :: instrs
  | Tacky.Jump target -> Jmp target :: instrs
  | Tacky.JumpIfZero (cond, target) ->
    JmpCC (E, target) :: Cmp (Imm 0L, convert_operand cond) :: instrs
  | Tacky.JumpIfNotZero  (cond, target) ->
    JmpCC (NE, target) :: Cmp (Imm 0L, convert_operand cond) :: instrs
  | Tacky.Copy (src, dst) -> Mov (convert_operand src, convert_operand dst) :: instrs
  | Tacky.Label label -> Label label :: instrs
  
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
  | Cmp (src, dst) ->
    let (var_map, stack_size, new_src) =
      replace_op_pseudo var_map stack_size src in
    let (var_map, stack_size, new_dst) =
      replace_op_pseudo var_map stack_size dst in
    (var_map, stack_size, Cmp (new_src, new_dst) :: instrs)
  | SetCC (cond_code, op) ->
    let (var_map, stack_size, new_op) =
      replace_op_pseudo var_map stack_size op in
    (var_map, stack_size, SetCC (cond_code, new_op) :: instrs)
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
  | Idiv src ->
    Idiv (Reg R10) :: Mov (src, Reg R10) :: instrs
  | Binary (Add, Stack src, Stack dst) ->
    Binary (Add, Reg R10, Stack dst) ::
    Mov (Stack src, Reg R10) :: instrs
  | Binary (Sub, Stack src, Stack dst) ->
    Binary (Sub, Reg R10, Stack dst) ::
    Mov (Stack src, Reg R10) :: instrs
  | Binary (Mult, src, Stack dst) ->
    Mov (Reg R11, Stack dst) ::
    Binary (Mult, src, Reg R11) ::
    Mov (Stack dst, Reg R11) :: instrs
  | Binary (BitwiseAnd, src, Stack dst) ->
    Mov (Reg R11, Stack dst) ::
    Binary (BitwiseAnd, src, Reg R11) ::
    Mov (Stack dst, Reg R11) :: instrs
  | Binary (BitwiseOr, src, Stack dst) ->
    Mov (Reg R11, Stack dst) ::
    Binary (BitwiseOr, src, Reg R11) ::
    Mov (Stack dst, Reg R11) :: instrs
  | Binary (BitwiseXor, src, Stack dst) ->
    Mov (Reg R11, Stack dst) ::
    Binary (BitwiseXor, src, Reg R11) ::
    Mov (Stack dst, Reg R11) :: instrs
  | Binary (ShiftLeft, src, dst) ->
    Binary (ShiftLeft, Reg CL, dst) ::
    Mov (src, Reg CX) :: instrs
  | Binary (ShiftRight, src, dst) ->
    Binary (ShiftRight, Reg CL, dst) ::
    Mov (src, Reg CX) :: instrs
  | Cmp (Stack op1, Stack op2) ->
    Cmp (Reg R10, Stack op2) ::
    Mov (Stack op1, Reg R10)  :: instrs
  | Cmp (op1, Imm n) ->
    Cmp (op1, Reg R11) ::
    Mov (Imm n, Reg R11) :: instrs
  | rest -> rest :: instrs

let fixup_func (Function (name, instrs)) stack_size =
  let new_instrs = List.fold_left fixup_instr [] instrs in
  Function (name, (AllocateStack (-stack_size) :: List.rev new_instrs))

let fixup_program (Program func_def) stack_size =
  Program (fixup_func func_def stack_size)

let emit_cond_code = function
  | E -> "E"
  | NE -> "NE"
  | L -> "L"
  | LE -> "LE"
  | G -> "G"
  | GE -> "GE"
    
let emit_operand operand =
  match operand with
  | Reg AX -> "%eax"
  | Reg DX -> "%edx"
  | Reg R10 -> "%r10d"
  | Reg R11 -> "%r11d"
  | Reg CX -> "%ecx"
  | Reg CL -> "%cl"
  | Stack v -> Printf.sprintf "%d(%%rbp)" v
  | Imm v -> Printf.sprintf "$%Ld" v
  | Pseudo _ -> failwith "Pseudo register not removed"

let emit_one_byte_operand operand =
  match operand with
  | Reg AX -> "%eal"
  | Reg DX -> "%dl"
  | Reg R10 -> "%r10b"
  | Reg R11 -> "%r11b"
  | Reg CX -> "%cl"
  | Reg CL -> "%cl"
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
  | _ -> failwith "Tried to emit incorrect binop"
           
let emit_instr instrs instr =
  match instr with
  | Mov(src, dst) -> (Printf.sprintf "    movl   %s, %s\n"
    (emit_operand src) (emit_operand dst)) :: instrs
  | Unary (op, operand) ->
    (Printf.sprintf "    %s    %s\n"
       (emit_unop op) (emit_operand operand)) :: instrs
  | Binary (op, src, dst) ->
    (Printf.sprintf "    %s    %s, %s\n"
       (emit_binop op) (emit_operand src) (emit_operand dst)) :: instrs
  | Idiv operand ->
    (Printf.sprintf "    idivl    %s\n" (emit_operand operand)) :: instrs
  | Cdq -> "    cdq\n" :: instrs
  | Cmp (op1, op2) ->
    (Printf.sprintf "    cmpl    %s, %s\n"
       (emit_operand op1) (emit_operand op2)) :: instrs
  | Jmp label -> (Printf.sprintf "    jmp    .L%s\n" label) :: instrs
  | JmpCC (cond_code, label) -> (Printf.sprintf "    j%s    .L%s\n"
                                   (emit_cond_code cond_code) label) :: instrs
  | SetCC (cond_code, operand) -> (Printf.sprintf "    set%s    %s\n"
                                     (emit_cond_code cond_code)
                                     (emit_one_byte_operand operand)) :: instrs
  | Label label -> (Printf.sprintf ".L%s:\n" label) :: instrs
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
