%{
  open Ast.ASTHRC
%}

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
%token STAR
%token EOF

%start <Ast.ASTHRC.HeapRegularCommands.t>program

%%

program:
  | toplevel_command EOF      { $1 }

toplevel_command:
  | atomic_command            { Ast.ASTHRC.HeapRegularCommand.Command($1) }
  | sequence                  { Ast.ASTHRC.HeapRegularCommand.Sequence($1) }
  | nondetchoice                    { Ast.ASTHRC.HeapRegularCommand.NondeterministicChoice($1) }
  | star                      { Ast.ASTHRC.HeapRegularCommand.Star($1) }
;

atomic_command:
  | SKIP                      { Ast.ASTHRC.HeapAtomicCommand.Skip }
  | id = IDENTIFIER NONDET    { Ast.ASTHRC.HeapAtomicCommand.NonDet(id) }


sequence:
  | atomic_command SEMICOLON atomic_command { Ast.ASTHRC.HeapRegularCommand.Sequence($1, $3) }

nondetchoice:
  | toplevel_command PLUS toplevel_command { Ast.ASTHRC.HeapRegularCommand.NondeterministicChoice($1, $3) }

star:
  | atomic_command STAR { Ast.ASTHRC.HeapRegularCommand.Star($1) }
