(* Although the Writing a C Compiler doesn't mention this in the
   early chapters, I am keeping track of the original source file,
   line number, and column in order to report errors. *)

module StringMap = Map.Make(String)
    
type unary_op = Complement | Negate | Not | PreInc | PreDec | PostInc | PostDec 
type binary_op = Add | Subtract | Multiply | Divide | Remainder | ShiftLeft |
                 ShiftRight | BitwiseAnd | BitwiseOr | BitwiseXor |
                 And | Or | Equal | NotEqual | LessThan | LessOrEqual |
                 GreaterThan | GreaterOrEqual |
                 AddEqual | SubtractEqual | MultiplyEqual | DivideEqual |
                 RemainderEqual | ShiftLeftEqual | ShiftRightEqual |
                 BitwiseAndEqual | BitwiseOrEqual | BitwiseXorEqual
type loc_type = Location of string * int * int
and exp_type = ConstantInt of loc_type * int64 |
               Unary of loc_type * unary_op * exp_type |
               Binary of loc_type * binary_op * exp_type * exp_type |
               Var of loc_type * string |
               Assignment of loc_type * exp_type * exp_type |
               AssignmentExpr of loc_type * exp_type * exp_type |
               Condition of loc_type * exp_type * exp_type * exp_type
and statement_type = Return of loc_type * exp_type |
                     Expression of loc_type * exp_type |
                     If of loc_type * exp_type * statement_type *
                           statement_type option |
                     Label of loc_type * string * statement_type |
                     Goto of loc_type * string |
                     Compound of loc_type * block_type |
                     Break of loc_type * string option |
                     Continue of loc_type * string option |
                     While of loc_type * exp_type * statement_type *
                                string option |
                     DoWhile of loc_type * exp_type * statement_type *
                                  string option |
                     For of loc_type * for_init_type * exp_type option *
                              exp_type option * statement_type *
                                string option|
                     Null
and function_def_type = FunctionDef of loc_type * string * block_item list
and program_type = Program of function_def_type
and declaration_type = Declaration of loc_type * string * exp_type option
and for_init_type = InitDecl of declaration_type | InitExpr of exp_type option
and block_item = S of statement_type | D of declaration_type
and block_type = Block of block_item list


