%{
  open Prelude.Ast.Commands
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

%start <t>program

%%

program:
  | toplevel_command EOF                                                                    { $1 }

toplevel_command:
  | atomic_command                                                                          { HeapRegularCommand.Command($1) }
  | sequence                                                                                { $1 }
  | nondetchoice                                                                            { $1 }
  | star                                                                                    { $1 }
;

atomic_command:
  | SKIP                                                                                    { HeapAtomicCommand.Skip }
  | id = IDENTIFIER EQ a = arithmentic_expression                                           { HeapAtomicCommand.Assignment(id, a) }
  | id = IDENTIFIER NONDET                                                                  { HeapAtomicCommand.NonDet(id) }
  | b = boolean_expression QUESTION                                                         { HeapAtomicCommand.Guard(b) }
  | id = IDENTIFIER EQ ALLOC                                                                { HeapAtomicCommand.Allocation(id) }
  | FREE LPAREN id = IDENTIFIER RPAREN                                                      { HeapAtomicCommand.Deallocation(id) }
  | id1 = IDENTIFIER EQ LBRACKET id2 = IDENTIFIER RBRACKET                                  { HeapAtomicCommand.Dereference(id1, id2) }
  | LBRACKET id1 = IDENTIFIER RBRACKET EQ a = arithmentic_expression                        { HeapAtomicCommand.Reference(id1, id2) }
;

arithmentic_expression:
  | INT                                                                                     { ArithmenticExpression.Literal($1) }
  | id = IDENTIFIER                                                                         { ArithmenticExpression.Variable(id) }
  | a1 = arithmentic_expression o = arithmetic_operator a2 = arithmentic_expression         { ArithmenticExpression.BinaryOperation(o, a1, a2) }
;

arithmetic_operator:
  | PLUS                                                                                    { ArithmeticOperation.Plus }
  | MINUS                                                                                   { ArithmeticOperation.Minus }
  | TIMES                                                                                   { ArithmeticOperation.Times }
  | DIV                                                                                     { ArithmeticOperation.Division }
  | MOD                                                                                     { ArithmeticOperation.Modulo }
;

boolean_expression:
  | TRUE                                                                                    { BooleanExpression.True }
  | FALSE                                                                                   { BooleanExpression.False }
  | NOT b = boolean_expression                                                              { BooleanExpression.Not(b) }
  | b1 = boolean_expression AND b2 = boolean_expression                                     { BooleanExpression.And(b1, b2) }
  | b1 = boolean_expression OR b2 = boolean_expression                                      { BooleanExpression.Or(b1, b2) }
  | a1 = arithmentic_expression c = boolean_comparison_op a2 = arithmentic_expression       { BooleanExpression.Comparison(c, a1, a2) }
;

boolean_comparison_op:
  | EQEQ                                                                                    { BooleanComparison.Equal }
  | NEQ                                                                                     { BooleanComparison.NotEqual }
  | LT                                                                                      { BooleanComparison.LessThan }
  | LE                                                                                      { BooleanComparison.LessOrEqual }
  | GT                                                                                      { BooleanComparison.GreaterThan }
  | GE                                                                                      { BooleanComparison.GreaterOrEqual }
;

sequence:
  | atomic_command SEMICOLON atomic_command                                                 { HeapRegularCommand.Sequence($1, $3) }
;

nondetchoice:
  | toplevel_command PLUS toplevel_command                                                  { HeapRegularCommand.NondeterministicChoice($1, $3) }
;

star:
  | atomic_command STAR                                                                     { HeapRegularCommand.Star($1) }
;
