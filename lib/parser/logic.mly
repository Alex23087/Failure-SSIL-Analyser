%{
  open Prelude.Ast.LogicFormulas

  open Prelude.Ast

  let from_menhir_pos (position) =
    let line = position.Lexing.pos_lnum in
    let column = position.Lexing.pos_cnum in
    make_position line column

  let annotate formula position = Prelude.Ast.LogicFormulas.annotate formula (from_menhir_pos position)
%}

%token True
%token False
%token Exists
%token <string> Identifier
%token And Or Star Emp Arrow Void
%token LT GT LE GE EQ NE
%token <int> Integer
%token Plus Minus Times Div Mod
%token EOF /*end of formula */


/* precedences */
%left Or
%left And
%left Star
%left Plus Minus
%left Times Div Mod
%nonassoc PREC


/* Starting symbol */

%start <Prelude.Ast.LogicFormulas.Formula.t> formula
%type <Prelude.Ast.LogicFormulas.Formula.t> the_formula
%type <Prelude.Ast.LogicFormulas.ArithmeticExpression.t> arithmetic_expression


%%

/* Grammar specification */

formula:
  | the_formula EOF                                                     { $1 }

the_formula:
    | True
      { annotate (Formula.True) $startpos }
    | False
      { annotate (Formula.False) $startpos }
    | Exists Identifier the_formula
      { annotate (Formula.Exists($2, $3)) $startpos } %prec PREC
    | the_formula And the_formula
      { annotate (Formula.And($1, $3)) $startpos }
    | the_formula Or the_formula
      { annotate (Formula.Or($1, $3)) $startpos }
    | arithmetic_expression BinaryComparison arithmetic_expression
      { annotate (Formula.Comparison($2, $1, $3)) $startpos }
    | Emp
      { annotate (Formula.EmptyHeap) $startpos }
    | Identifier Arrow arithmetic_expression
      { annotate (Formula.Allocation($1, $3)) $startpos }
    | Identifier Void
      { annotate (Formula.NonAllocated($1)) $startpos }
    | the_formula Star the_formula
      { annotate (Formula.AndSeparately($1, $3)) $startpos }
    ;

arithmetic_expression:
    | Integer
      { annotate (ArithmeticExpression.Literal($1)) $startpos }
    | Identifier
      { annotate (ArithmeticExpression.Variable($1)) $startpos }
    | arithmetic_expression BinaryOperator arithmetic_expression
      { annotate (ArithmeticExpression.Operation($2, $1, $3)) $startpos }

%inline BinaryComparison:
  | LT { BinaryComparison.LessThan }
  | GT { BinaryComparison.GreaterThan }
  | LE { BinaryComparison.LessOrEqual }
  | GE { BinaryComparison.GreaterOrEqual }
  | EQ { BinaryComparison.Equals }
  | NE { BinaryComparison.NotEquals }
  ;

%inline BinaryOperator:
  | Plus  { BinaryOperator.Plus }
  | Minus { BinaryOperator.Minus }
  | Times { BinaryOperator.Times }
  | Div   { BinaryOperator.Division }
  | Mod   { BinaryOperator.Modulo }
  ;