%{
  open Ast.HeapRegularCommands
%}

%token PLUS
%token MINUS
%token TIMES
%token DIV
%token MOD
%token EQ
%token EQEQ
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
%token LPAREN
%token RPAREN
%token SEMICOLON
%token QUESTION
%token <int> INT
%token <string> IDENTIFIER
%token <string> HEAPIDENTIFIER
%token NONDET
%token STAR
%token EOF

%start <HeapRegularCommands.t>program

%%

program:
  | toplevel_command EOF                                                                    { $1 }

toplevel_command:
  | atomic_command                                                                          { HeapRegularCommands.HeapRegularCommand.Command($1) }
  | sequence                                                                                { HeapRegularCommands.HeapRegularCommand.Sequence($1) }
  | nondetchoice                                                                            { HeapRegularCommands.HeapRegularCommand.NondeterministicChoice($1) }
  | star                                                                                    { HeapRegularCommands.HeapRegularCommand.Star($1) }
;

atomic_command:
  | SKIP                                                                                    { HeapRegularCommands.HeapAtomicCommand.Skip }
  | id = IDENTIFIER EQ a = arithmentic_expression                                           { HeapRegularCommands.HeapAtomicCommand.Assignment(id, a) }
  | id = IDENTIFIER NONDET                                                                  { HeapRegularCommands.HeapAtomicCommand.NonDet(id) }
  | b = boolean_expression QUESTION                                                         { HeapRegularCommands.HeapAtomicCommand.Guard(b) }
  | id = IDENTIFIER EQ ALLOC                                                                { HeapRegularCommands.HeapAtomicCommand.Allocation(id) }
  | FREE LPAREN id = IDENTIFIER RPAREN                                                      { HeapRegularCommands.HeapAtomicCommand.Deallocation(id) }
  | id1 = IDENTIFIER EQ LBRACKET id2 = IDENTIFIER RBRACKET                                  { HeapRegularCommands.HeapAtomicCommand.Dereference(id1, id2) }
  | LBRACKET id1 = IDENTIFIER RBRACKET EQ a = arithmentic_expression                        { HeapRegularCommands.HeapAtomicCommand.Reference(id1, id2) }
;

arithmentic_expression:
  | INT                                                                                     { HeapRegularCommands.ArithmenticExpression.Literal($1) }
  | id = IDENTIFIER                                                                         { HeapRegularCommands.ArithmenticExpression.Variable(id) }
  | a1 = arithmentic_expression o = arithmetic_operator a2 = arithmentic_expression         { HeapRegularCommands.ArithmenticExpression.BinaryOperation(o, a1, a2) }
;

arithmetic_operator:
  | PLUS                                                                                    { HeapRegularCommands.ArithmeticOperation.Plus }
  | MINUS                                                                                   { HeapRegularCommands.ArithmeticOperation.Minus }
  | TIMES                                                                                   { HeapRegularCommands.ArithmeticOperation.Times }
  | DIV                                                                                     { HeapRegularCommands.ArithmeticOperation.Division }
  | MOD                                                                                     { HeapRegularCommands.ArithmeticOperation.Modulo }
;

boolean_expression:
  | TRUE                                                                                    { HeapRegularCommands.BooleanExpression.True }
  | FALSE                                                                                   { HeapRegularCommands.BooleanExpression.False }
  | NOT b = boolean_expression                                                              { HeapRegularCommands.BooleanExpression.Not(b) }
  | b1 = boolean_expression AND b2 = boolean_expression                                     { HeapRegularCommands.BooleanExpression.And(b1, b2) }
  | b1 = boolean_expression OR b2 = boolean_expression                                      { HeapRegularCommands.BooleanExpression.Or(b1, b2) }
  | a1 = arithmentic_expression c = boolean_comparison_op a2 = arithmentic_expression       { HeapRegularCommands.BooleanExpression.Comparison(c, a1, a2) }
;

boolean_comparison_op:
  | EQEQ                                                                                    { HeapRegularCommands.BooleanComparison.Equal }
  | NEQ                                                                                     { HeapRegularCommands.BooleanComparison.NotEqual }
  | LT                                                                                      { HeapRegularCommands.BooleanComparison.LessThan }
  | LE                                                                                      { HeapRegularCommands.BooleanComparison.LessOrEqual }
  | GT                                                                                      { HeapRegularCommands.BooleanComparison.GreaterThan }
  | GE                                                                                      { HeapRegularCommands.BooleanComparison.GreaterOrEqual }
;

sequence:
  | atomic_command SEMICOLON atomic_command                                                 { HeapRegularCommands.HeapRegularCommand.Sequence($1, $3) }
;

nondetchoice:
  | toplevel_command PLUS toplevel_command                                                  { HeapRegularCommands.HeapRegularCommand.NondeterministicChoice($1, $3) }
;

star:
  | atomic_command STAR                                                                     { HeapRegularCommands.HeapRegularCommand.Star($1) }
;
