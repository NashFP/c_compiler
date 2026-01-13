(* Although the Writing a C Compiler doesn't mention this in the
   early chapters, I am keeping track of the original source file,
   line number, and column in order to report errors. *)

type loc_type = Location of string * int * int
  and exp_type = ConstantInt of loc_type * int64
  and statement_type = StmtReturn of loc_type * exp_type
  and function_def_type = FunctionDef of loc_type * (string * statement_type)
  and program_type = Program of function_def_type
