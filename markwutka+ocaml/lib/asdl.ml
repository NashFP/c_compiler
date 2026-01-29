type unary_operator = Complement | Negate
type val_type = ConstantInt of int64 | Var of string
type instruction = Return of val_type | Unary of unary_operator * val_type * val_type
type function_definition = Function of string * instruction list
type program_type = Program of function_definition
