(* Although the Writing a C Compiler doesn't mention this in the
   early chapters, I am keeping track of the original source file,
   line number, and column in order to report errors. *)

module StringMap = Map.Make(String)
    
type unary_op = Complement | Negate | Not
type binary_op = Add | Subtract | Multiply | Divide | Remainder | ShiftLeft |
                 ShiftRight | BitwiseAnd | BitwiseOr | BitwiseXor |
                 And | Or | Equal | NotEqual | LessThan | LessOrEqual |
                 GreaterThan | GreaterOrEqual
type loc_type = Location of string * int * int
and exp_type = ConstantInt of loc_type * int64 |
               Unary of loc_type * unary_op * exp_type |
               Binary of loc_type * binary_op * exp_type * exp_type |
               Var of loc_type * string |
               Assignment of loc_type * exp_type * exp_type
  and statement_type = Return of loc_type * exp_type |
                       Expression of loc_type * exp_type |
                       Null
  and function_def_type = FunctionDef of loc_type * string * block_item list
  and program_type = Program of function_def_type
  and declaration_type = Declaration of loc_type * string * exp_type option
and block_item = S of statement_type | D of declaration_type                   

let exp_loc exp =
  match exp with
  | ConstantInt (loc,_) -> loc
  | Unary (loc,_,_) -> loc
  | Binary (loc,_,_,_) -> loc
  | Var (loc,_) -> loc
  | Assignment (loc,_,_) -> loc

