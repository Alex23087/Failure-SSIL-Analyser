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
%token NONDET
%token STAR
%token EOF

/* precedences */
%nonassoc SEMICOLON
%nonassoc EQ
%left OR
%left AND
%right NOT
%left PLUS MINUS
%left TIMES DIV MOD
%left STAR


%start <Prelude.Ast.Commands.HeapRegularCommand.t> program
%type <Prelude.Ast.Commands.HeapRegularCommand.t> toplevel_command
%type <Prelude.Ast.Commands.HeapAtomicCommand.t> atomic_command
%type <Prelude.Ast.Commands.ArithmeticExpression.t> arithmetic_expression
%type <Prelude.Ast.Commands.BooleanExpression.t> boolean_expression
%type <Prelude.Ast.Commands.HeapRegularCommand.t> sequence
%type <Prelude.Ast.Commands.HeapRegularCommand.t> nondetchoice
%type <Prelude.Ast.Commands.HeapRegularCommand.t> star

%%

program:
  | toplevel_command EOF                                                                    { $1 }

toplevel_command:
  | atomic_command
    { annotateEmpty (HeapRegularCommand.Command($1)) $startpos }
  | sequence
    { $1 }
  | nondetchoice
    { $1 }
  | star
    { $1 }
;

atomic_command:
  | SKIP
    { annotateEmpty (HeapAtomicCommand.Skip) $startpos }
  | id = IDENTIFIER EQ a = arithmetic_expression
    { annotateEmpty (HeapAtomicCommand.Assignment(id, a)) $startpos }
  | id = IDENTIFIER NONDET
    { annotateEmpty (HeapAtomicCommand.NonDet(id)) $startpos }
  | b = boolean_expression QUESTION
    { annotateEmpty (HeapAtomicCommand.Guard(b)) $startpos }
  | id = IDENTIFIER EQ ALLOC
    { annotateEmpty (HeapAtomicCommand.Allocation(id)) $startpos }
  | FREE LPAREN id = IDENTIFIER RPAREN
    { annotateEmpty (HeapAtomicCommand.Free(id)) $startpos }
  | id1 = IDENTIFIER EQ LBRACKET id2 = IDENTIFIER RBRACKET
    { annotateEmpty (HeapAtomicCommand.ReadHeap(id1, id2)) $startpos }
  | LBRACKET id1 = IDENTIFIER RBRACKET EQ a = arithmetic_expression
    { annotateEmpty (HeapAtomicCommand.WriteHeap(id1, a)) $startpos }
;

arithmetic_expression:
  | INT
    { annotateEmpty (ArithmeticExpression.Literal($1)) $startpos }
  | id = IDENTIFIER
    { annotateEmpty (ArithmeticExpression.Variable(id)) $startpos }
  | a1 = arithmetic_expression o = arithmetic_operator a2 = arithmetic_expression
    { annotateEmpty (ArithmeticExpression.BinaryOperation(o, a1, a2)) $startpos }
;

%inline arithmetic_operator:
  | PLUS
    { ArithmeticOperation.Plus }
  | MINUS
    { ArithmeticOperation.Minus }
  | TIMES
    { ArithmeticOperation.Times }
  | DIV
    { ArithmeticOperation.Division }
  | MOD
    { ArithmeticOperation.Modulo }
;

boolean_expression:
  | TRUE
    { annotateEmpty (BooleanExpression.True) $startpos }
  | FALSE
    { annotateEmpty (BooleanExpression.False) $startpos }
  | NOT b = boolean_expression
    { annotateEmpty (BooleanExpression.Not(b)) $startpos }
  | b1 = boolean_expression AND b2 = boolean_expression
    { annotateEmpty (BooleanExpression.And(b1, b2)) $startpos }
  | b1 = boolean_expression OR b2 = boolean_expression
    { annotateEmpty (BooleanExpression.Or(b1, b2)) $startpos }
  | a1 = arithmetic_expression c = boolean_comparison_op a2 = arithmetic_expression
    { annotateEmpty (BooleanExpression.Comparison(c, a1, a2)) $startpos }
;

%inline boolean_comparison_op:
  | EQEQ
    { BooleanComparison.Equal }
  | NEQ
    { BooleanComparison.NotEqual }
  | LT
    { BooleanComparison.LessThan }
  | LE
    { BooleanComparison.LessOrEqual }
  | GT
    { BooleanComparison.GreaterThan }
  | GE
    { BooleanComparison.GreaterOrEqual }
;

sequence:
  | toplevel_command SEMICOLON toplevel_command
    {annotateEmpty (HeapRegularCommand.Sequence($1, $3)) $startpos }
;

nondetchoice:
  | toplevel_command PLUS toplevel_command
    { annotateEmpty (HeapRegularCommand.NondeterministicChoice($1, $3)) $startpos }
;

star:
  | toplevel_command STAR
    { annotateEmpty (HeapRegularCommand.Star($1)) $startpos  } 
;
