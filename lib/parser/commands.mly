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

%start <Ast.ASTHRC.HeapRegularCommands.t>program

%%

program:
  | toplevel_command EOF                                                                    { $1 }

toplevel_command:
  | atomic_command                                                                          { Ast.ASTHRC.HeapRegularCommand.Command($1) }
  | sequence                                                                                { Ast.ASTHRC.HeapRegularCommand.Sequence($1) }
  | nondetchoice                                                                            { Ast.ASTHRC.HeapRegularCommand.NondeterministicChoice($1) }
  | star                                                                                    { Ast.ASTHRC.HeapRegularCommand.Star($1) }
;

atomic_command:
  | SKIP                                                                                    { Ast.ASTHRC.HeapAtomicCommand.Skip }
  | id = IDENTIFIER EQ a = arithmentic_expression                                           { Ast.ASTHRC.HeapAtomicCommand.Assignment(id, a) }
  | id = IDENTIFIER NONDET                                                                  { Ast.ASTHRC.HeapAtomicCommand.NonDet(id) }
  | b = boolean_expression QUESTION                                                         { Ast.ASTHRC.HeapAtomicCommand.Guard(b) }
  | id = IDENTIFIER EQ ALLOC                                                                { Ast.ASTHRC.HeapAtomicCommand.Allocation(id) }
  | FREE LPAREN id = IDENTIFIER RPAREN                                                      { Ast.ASTHRC.HeapAtomicCommand.Deallocation(id) }
  | id1 = IDENTIFIER EQ LBRACKET id2 = IDENTIFIER RBRACKET                                  { Ast.ASTHRC.HeapAtomicCommand.Dereference(id1, id2) }
  | LBRACKET id1 = IDENTIFIER RBRACKET EQ a = arithmentic_expression                        { Ast.ASTHRC.HeapAtomicCommand.Reference(id1, id2) }
;

arithmentic_expression:
  | INT                                                                                     { Ast.ASTHRC.ArithmenticExpression.Literal($1) }
  | id = IDENTIFIER                                                                         { Ast.ASTHRC.ArithmenticExpression.Variable(id) }
  | a1 = arithmentic_expression o = arithmetic_operator a2 = arithmentic_expression         { Ast.ASTHRC.ArithmenticExpression.BinaryOperation(o, a1, a2) }
;

arithmetic_operator:
  | PLUS                                                                                    { Ast.ASTHRC.ArithmeticOperation.Plus }
  | MINUS                                                                                   { Ast.ASTHRC.ArithmeticOperation.Minus }
  | TIMES                                                                                   { Ast.ASTHRC.ArithmeticOperation.Times }
  | DIV                                                                                     { Ast.ASTHRC.ArithmeticOperation.Division }
  | MOD                                                                                     { Ast.ASTHRC.ArithmeticOperation.Modulo }
;

boolean_expression:
  | TRUE                                                                                    { Ast.ASTHRC.BooleanExpression.True }
  | FALSE                                                                                   { Ast.ASTHRC.BooleanExpression.False }
  | NOT b = boolean_expression                                                              { Ast.ASTHRC.BooleanExpression.Not(b) }
  | b1 = boolean_expression AND b2 = boolean_expression                                     { Ast.ASTHRC.BooleanExpression.And(b1, b2) }
  | b1 = boolean_expression OR b2 = boolean_expression                                      { Ast.ASTHRC.BooleanExpression.Or(b1, b2) }
  | a1 = arithmentic_expression c = boolean_comparison_op a2 = arithmentic_expression       { Ast.ASTHRC.BooleanExpression.Comparison(c, a1, a2) }
;

boolean_comparison_op:
  | EQ                                                                                      { Ast.ASTHRC.BooleanComparison.Equal }
  | NEQ                                                                                     { Ast.ASTHRC.BooleanComparison.NotEqual }
  | LT                                                                                      { Ast.ASTHRC.BooleanComparison.LessThan }
  | LE                                                                                      { Ast.ASTHRC.BooleanComparison.LessOrEqual }
  | GT                                                                                      { Ast.ASTHRC.BooleanComparison.GreaterThan }
  | GE                                                                                      { Ast.ASTHRC.BooleanComparison.GreaterOrEqual }
;

sequence:
  | atomic_command SEMICOLON atomic_command                                                 { Ast.ASTHRC.HeapRegularCommand.Sequence($1, $3) }
;

nondetchoice:
  | toplevel_command PLUS toplevel_command                                                  { Ast.ASTHRC.HeapRegularCommand.NondeterministicChoice($1, $3) }
;

star:
  | atomic_command STAR                                                                     { Ast.ASTHRC.HeapRegularCommand.Star($1) }
;
