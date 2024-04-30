%{
  open Prelude.Ast.Commands

  open Prelude.Ast

  let from_menhir_pos (position) =
    let line = position.Lexing.pos_lnum in
    let column = position.Lexing.pos_cnum in
    make_position line column

  let annotate command position formula = Prelude.Ast.Commands.annotate command (from_menhir_pos position) formula
  let annotateEmpty command position = annotate command position None
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
  | atomic_command
    { annotateEmpty(HeapRegularCommand.Command($1), $startpos) }
  | sequence                                                                                { $1 }
  | nondetchoice                                                                            { $1 }
  | star                                                                                    { $1 }
;

atomic_command:
  | SKIP
    { annotateEmpty(HeapAtomicCommand.Skip, $startpos) }
  | id = IDENTIFIER EQ a = arithmentic_expression
    { annotateEmpty(HeapAtomicCommand.Assignment(id, a), $startpos) }
  | id = IDENTIFIER NONDET
    { annotateEmpty(HeapAtomicCommand.NonDet(id), $startpos) }
  | b = boolean_expression QUESTION
    { annotateEmpty(HeapAtomicCommand.Guard(b), $startpos) }
  | id = IDENTIFIER EQ ALLOC
    { annotateEmpty(HeapAtomicCommand.Allocation(id), $startpos) }
  | FREE LPAREN id = IDENTIFIER RPAREN
    { annotateEmpty(HeapAtomicCommand.Deallocation(id), $startpos) }
  | id1 = IDENTIFIER EQ LBRACKET id2 = IDENTIFIER RBRACKET
    { annotateEmpty(HeapAtomicCommand.Dereference(id1, id2), $startpos) }
  | LBRACKET id1 = IDENTIFIER RBRACKET EQ a = arithmentic_expression
    { annotateEmpty(HeapAtomicCommand.Reference(id1, id2), $startpos) }
;

arithmentic_expression:
  | INT
    { annotateEmpty(ArithmenticExpression.Literal($1), $startpos) }
  | id = IDENTIFIER
    { annotateEmpty(ArithmenticExpression.Variable(id), $startpos) }
  | a1 = arithmentic_expression o = arithmetic_operator a2 = arithmentic_expression
    { annotateEmpty(ArithmenticExpression.BinaryOperation(o, a1, a2), $startpos) }
;

arithmetic_operator:
  | PLUS
    { annotateEmpty(ArithmeticOperation.Plus, $startpos) }
  | MINUS
    { annotateEmpty(ArithmeticOperation.Minus, $startpos) }
  | TIMES
    { annotateEmpty(ArithmeticOperation.Times, $startpos) }
  | DIV
    { annotateEmpty(ArithmeticOperation.Division, $startpos) }
  | MOD
    { annotateEmpty(ArithmeticOperation.Modulo, $startpos) }
;

boolean_expression:
  | TRUE
    { annotateEmpty(BooleanExpression.True, $startpos) }
  | FALSE
    { annotateEmpty(BooleanExpression.False, $startpos) }
  | NOT b = boolean_expression
    { annotateEmpty(BooleanExpression.Not(b), $startpos) }
  | b1 = boolean_expression AND b2 = boolean_expression
    { annotateEmpty(BooleanExpression.And(b1, b2), $startpos) }
  | b1 = boolean_expression OR b2 = boolean_expression
    { annotateEmpty(BooleanExpression.Or(b1, b2), $startpos) }
  | a1 = arithmentic_expression c = boolean_comparison_op a2 = arithmentic_expression
    { annotateEmpty(BooleanExpression.Comparison(c, a1, a2), $startpos) }
;

boolean_comparison_op:
  | EQEQ
    { annotateEmpty(BooleanComparison.Equal, $startpos) }
  | NEQ
    { annotateEmpty(BooleanComparison.NotEqual, $startpos) }
  | LT
    { annotateEmpty(BooleanComparison.LessThan, $startpos) }
  | LE
    { annotateEmpty(BooleanComparison.LessOrEqual, $startpos) }
  | GT
    { annotateEmpty(BooleanComparison.GreaterThan, $startpos) }
  | GE
    { annotateEmpty(BooleanComparison.GreaterOrEqual, $startpos) }
;

sequence:
  | atomic_command SEMICOLON atomic_command
    {annotateEmpty(HeapRegularCommand.Sequence($1, $3), $startpos) }
;

nondetchoice:
  | toplevel_command PLUS toplevel_command
    { annotateEmpty(HeapRegularCommand.NondeterministicChoice($1, $3),$startpos) }
;

star:
  | atomic_command STAR
    { annotateEmpty(HeapRegularCommand.Star($1), $startpos)  }
;
