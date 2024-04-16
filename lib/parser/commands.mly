%token PLUS
%token MINUS
%token TIMES
%token DIV
%token MOD
%token EQ
%token NEQ
%token LT
%token LE
%token GT
%token GE
%token AND
%token OR
%token NOT
%token TRUE
%token FALSE
%token SKIP
%token ALLOC
%token FREE
%token LBRACKET
%token RBRACKET
%token SEMICOLON
%token QUESTION
%token <int> INT
%token <string> IDENTIFIER
%token <string> HEAPIDENTIFIER
%token NONDET
%token EOF

%start program

program:
  | toplevel_command EOF      { $1 }

toplevel_command:
  | atomic_command            { Ast.HeapRegularCommand.Command($1) }
  | sequence                  { Ast.HeapRegularCommand.Sequence($1) }
  | nondetchoice                    { Ast.HeapRegularCommand.NondeterministicChoice($1) }
  | star                      { Ast.HeapRegularCommand.Star($1) }
;

atomic_command:
  | SKIP                      { Ast.HeapAtomicCommand.Skip }
  | id = IDENTIFIER NONDET    { Ast.HeapAtomicCommand.NonDet($1) }


sequence:
  | atomic_command SEMICOLON atomic_command { Ast.HeapRegularCommand.Sequence($1, $3) }

nondetchoice:
  | toplevel_command PLUS toplevel_command { Ast.HeapRegularCommand.NondeterministicChoice($1, $3) }

star:
  | atomic_command STAR { Ast.HeapRegularCommand.Star($1) }
