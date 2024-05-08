%{
  open Prelude.Ast.Commands

  let annotateCommand command position formula = Prelude.Ast.Commands.annotate_parser command position.Lexing.pos_lnum position.Lexing.pos_cnum formula
  let annotateEmptyCommand command position = annotateCommand command position None

  let annotateFormula formula position = Prelude.Ast.LogicFormulas.annotate_parser formula position.Lexing.pos_lnum position.Lexing.pos_cnum
%}

%token EqualEqual
%token Skip
%token Alloc
%token Free
%token LBracket
%token RBracket
%token Semicolon
%token Question
%token NonDet
%token Eof
%token LShift RShift
%token LParen RParen
%token True
%token False
%token Exists
%token <string> Identifier
%token And Or Emp Arrow Void Not
%token LessThan GreaterThan LessOrEqual GreaterOrEqual Equal NotEqual
%token <int> Integer
%token Plus Minus Times Div Mod
%token Dot

/* precedences */

%nonassoc LOW
%left Or
%left And
%right Not
%left Equal NotEqual
%left LessThan GreaterThan LessOrEqual GreaterOrEqual
%left Plus Minus
%left Times Div Mod
%left Semicolon
%nonassoc PREC

%start <Prelude.Ast.Commands.HeapRegularCommand.t> program
%type <Prelude.Ast.Commands.HeapRegularCommand.t> toplevel_command
%type <Prelude.Ast.Commands.HeapRegularCommand.t> toplevel_command_noformula
%type <Prelude.Ast.Commands.HeapAtomicCommand.t> atomic_command
%type <Prelude.Ast.Commands.ArithmeticExpression.t> arithmetic_expression
%type <Prelude.Ast.Commands.BooleanExpression.t> boolean_expression
%type <Prelude.Ast.Commands.HeapRegularCommand.t> sequence
%type <Prelude.Ast.Commands.HeapRegularCommand.t> nondetchoice
%type <Prelude.Ast.Commands.HeapRegularCommand.t> star
%type <Prelude.Ast.Commands.HeapRegularCommand.t> nondetchoice_noformula
%type <Prelude.Ast.Commands.HeapRegularCommand.t> star_noformula

%type <Prelude.Ast.LogicFormulas.Formula.t> formula
%type <Prelude.Ast.LogicFormulas.ArithmeticExpression.t> arithmetic_expression_of_formula

%%

program:
  | toplevel_command Eof
    { $1 }
  | LShift formula RShift Eof
    { annotateCommand (HeapRegularCommand.Command(annotateEmptyCommand HeapAtomicCommand.Skip $startpos)) $startpos (Some $2) }

toplevel_command:
  | atomic_command LShift formula RShift
    { annotateCommand (HeapRegularCommand.Command($1)) $startpos (Some $3) }
  | sequence
    { $1 }
  | nondetchoice
    { $1 }
  | star
    { $1 }
  | toplevel_command_noformula
    { $1 }
  | LParen toplevel_command RParen
    { $2 }
  ;

toplevel_command_noformula:
  | atomic_command
    { annotateEmptyCommand (HeapRegularCommand.Command($1)) $startpos }
  | nondetchoice_noformula
    { $1 }
  | star_noformula
    { $1 }
  ;

atomic_command:
  | Skip
    { annotateEmptyCommand (HeapAtomicCommand.Skip) $startpos }
  | id = Identifier Equal a = arithmetic_expression
    { annotateEmptyCommand (HeapAtomicCommand.Assignment(id, a)) $startpos }
  | id = Identifier Equal NonDet LParen RParen
    { annotateEmptyCommand (HeapAtomicCommand.NonDet(id)) $startpos }
  | b = boolean_expression Question
    { annotateEmptyCommand (HeapAtomicCommand.Guard(b)) $startpos }
  | id = Identifier Equal Alloc LParen RParen
    { annotateEmptyCommand (HeapAtomicCommand.Allocation(id)) $startpos }
  | Free LParen id = Identifier RParen
    { annotateEmptyCommand (HeapAtomicCommand.Free(id)) $startpos }
  | id1 = Identifier Equal LBracket id2 = Identifier RBracket
    { annotateEmptyCommand (HeapAtomicCommand.ReadHeap(id1, id2)) $startpos }
  | LBracket id1 = Identifier RBracket Equal a = arithmetic_expression
    { annotateEmptyCommand (HeapAtomicCommand.WriteHeap(id1, a)) $startpos }
;

arithmetic_expression:
  | Integer
    { annotateEmptyCommand (ArithmeticExpression.Literal($1)) $startpos }
  | id = Identifier
    { annotateEmptyCommand (ArithmeticExpression.Variable(id)) $startpos }
  | a1 = arithmetic_expression o = arithmetic_operator a2 = arithmetic_expression
    { annotateEmptyCommand (ArithmeticExpression.BinaryOperation(o, a1, a2)) $startpos }
  | LParen a = arithmetic_expression RParen
    { a }
;

%inline arithmetic_operator:
  | Plus
    { ArithmeticOperation.Plus }
  | Minus
    { ArithmeticOperation.Minus }
  | Times
    { ArithmeticOperation.Times }
  | Div
    { ArithmeticOperation.Division }
  | Mod
    { ArithmeticOperation.Modulo }
;

boolean_expression:
  | True
    { annotateEmptyCommand (BooleanExpression.True) $startpos }
  | False
    { annotateEmptyCommand (BooleanExpression.False) $startpos }
  | Not b = boolean_expression
    { annotateEmptyCommand (BooleanExpression.Not(b)) $startpos }
  | b1 = boolean_expression And b2 = boolean_expression
    { annotateEmptyCommand (BooleanExpression.And(b1, b2)) $startpos }
  | b1 = boolean_expression Or b2 = boolean_expression
    { annotateEmptyCommand (BooleanExpression.Or(b1, b2)) $startpos }
  | a1 = arithmetic_expression c = boolean_comparison_op a2 = arithmetic_expression
    { annotateEmptyCommand (BooleanExpression.Comparison(c, a1, a2)) $startpos }
  | LParen b = boolean_expression RParen
    { b }
;

%inline boolean_comparison_op:
  | EqualEqual
    { BooleanComparison.Equal }
  | NotEqual
    { BooleanComparison.NotEqual }
  | LessThan
    { BooleanComparison.LessThan }
  | LessOrEqual
    { BooleanComparison.LessOrEqual }
  | GreaterThan
    { BooleanComparison.GreaterThan }
  | GreaterOrEqual
    { BooleanComparison.GreaterOrEqual }
;

sequence:
  | toplevel_command Semicolon toplevel_command
    {annotateEmptyCommand (HeapRegularCommand.Sequence($1, $3)) $startpos }
;

nondetchoice:
  | toplevel_command_noformula Plus toplevel_command_noformula LShift formula RShift
    { annotateCommand (HeapRegularCommand.NondeterministicChoice($1, $3)) $startpos (Some $5)}
;

nondetchoice_noformula:
  | toplevel_command_noformula Plus toplevel_command_noformula
    { annotateEmptyCommand (HeapRegularCommand.NondeterministicChoice($1, $3)) $startpos }
;

star:
  | toplevel_command_noformula Times LShift formula RShift
    { annotateCommand (HeapRegularCommand.Star($1)) $startpos (Some $4) }
;

star_noformula:
  | toplevel_command_noformula Times
    { annotateEmptyCommand (HeapRegularCommand.Star($1)) $startpos  }
;

formula:
    | True
      { annotateFormula (Prelude.Ast.LogicFormulas.Formula.True) $startpos }
    | False
      { annotateFormula (Prelude.Ast.LogicFormulas.Formula.False) $startpos }
    | Exists Identifier Dot formula
      { annotateFormula (Prelude.Ast.LogicFormulas.Formula.Exists($2, $4)) $startpos } %prec PREC
    | formula And formula
      { annotateFormula (Prelude.Ast.LogicFormulas.Formula.And($1, $3)) $startpos }
    | formula Or formula
      { annotateFormula (Prelude.Ast.LogicFormulas.Formula.Or($1, $3)) $startpos }
    | arithmetic_expression_of_formula binary_comparison_of_formula arithmetic_expression_of_formula
      { annotateFormula (Prelude.Ast.LogicFormulas.Formula.Comparison($2, $1, $3)) $startpos }
    | Emp
      { annotateFormula (Prelude.Ast.LogicFormulas.Formula.EmptyHeap) $startpos }
    | Identifier Arrow arithmetic_expression_of_formula
      { annotateFormula (Prelude.Ast.LogicFormulas.Formula.Allocation($1, $3)) $startpos } %prec LOW
    | Identifier Void
      { annotateFormula (Prelude.Ast.LogicFormulas.Formula.NonAllocated($1)) $startpos }
    | formula Times formula
      { annotateFormula (Prelude.Ast.LogicFormulas.Formula.AndSeparately($1, $3)) $startpos }
    | LParen formula RParen
      { $2 }
    ;

arithmetic_expression_of_formula:
    | Integer
      { annotateFormula (Prelude.Ast.LogicFormulas.ArithmeticExpression.Literal($1)) $startpos }
    | Identifier
      { annotateFormula (Prelude.Ast.LogicFormulas.ArithmeticExpression.Variable($1)) $startpos }
    | arithmetic_expression_of_formula binary_operator_of_formula arithmetic_expression_of_formula
      { annotateFormula (Prelude.Ast.LogicFormulas.ArithmeticExpression.Operation($2, $1, $3)) $startpos }
    | LParen arithmetic_expression_of_formula RParen
      { $2 }

%inline binary_comparison_of_formula:
  | LessThan { Prelude.Ast.LogicFormulas.BinaryComparison.LessThan }
  | GreaterThan { Prelude.Ast.LogicFormulas.BinaryComparison.GreaterThan }
  | LessOrEqual { Prelude.Ast.LogicFormulas.BinaryComparison.LessOrEqual }
  | GreaterOrEqual { Prelude.Ast.LogicFormulas.BinaryComparison.GreaterOrEqual }
  | Equal { Prelude.Ast.LogicFormulas.BinaryComparison.Equals }
  | NotEqual { Prelude.Ast.LogicFormulas.BinaryComparison.NotEquals }
  ;

%inline binary_operator_of_formula:
  | Plus  { Prelude.Ast.LogicFormulas.BinaryOperator.Plus }
  | Minus { Prelude.Ast.LogicFormulas.BinaryOperator.Minus }
  | Times { Prelude.Ast.LogicFormulas.BinaryOperator.Times }
  | Div   { Prelude.Ast.LogicFormulas.BinaryOperator.Division }
  | Mod   { Prelude.Ast.LogicFormulas.BinaryOperator.Modulo }
  ;