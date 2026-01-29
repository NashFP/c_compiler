%token <C_ast.loc_type * int64> CONSTANT_INT
%token <C_ast.loc_type * string> IDENTIFIER

%token<C_ast.loc_type> INT
%token<C_ast.loc_type> VOID
%token<C_ast.loc_type> RETURN
%token<C_ast.loc_type> LPAREN
%token<C_ast.loc_type> RPAREN
%token<C_ast.loc_type> LBRACE
%token<C_ast.loc_type> RBRACE
%token<C_ast.loc_type> SEMI
%token<C_ast.loc_type> MINUSMINUS
%token<C_ast.loc_type> MINUS
%token<C_ast.loc_type> TILDE
%token EOF

%start<C_ast.program_type> program
%%


program: funcdef EOF { C_ast.Program ($1) }
  ;

funcdef: INT IDENTIFIER LPAREN VOID RPAREN LBRACE statement RBRACE
  { C_ast.FunctionDef ($1, (snd $2, $7)) }
  ;

statement: RETURN exp SEMI { C_ast.StmtReturn ($1, $2) }
  ;

exp: CONSTANT_INT { C_ast.ConstantInt (fst $1, snd $1) }
    | MINUS exp { C_ast.Unary ($1, Negate, $2) }
    | TILDE exp { C_ast.Unary ($1, Complement, $2) }
    | LPAREN exp RPAREN { $2 }
  ;
