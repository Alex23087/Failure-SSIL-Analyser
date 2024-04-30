// Formula ::= True | False | Exists Identifier Formula | Formula && Formula | Formula || Formula | ArithmeticExpression BinaryComparison ArithmeticExpression | Emp | x -> y | x -/> | Formula * Formula
// BinaryComparison ::= < | > | <= | >= | == | !=
// ArithmeticExpression ::= Int(n) | Identifier | ArithmeticExpression BinaryOperator ArithmeticExpression
// BinaryOperator ::= + | - | * | / | %

%{
  open Prelude.Ast.LogicFormulas
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

/* Starting symbol */

%start <Formula.t>formula

%%

/* Grammar specification */

formula:
  | the_formula EOF                                                     { $1 }

the_formula:
    | True                                                              { Formula.True }
    | False                                                             { Formula.False }
    | Exists Identifier the_formula                                     { Formula.Exists($2, $3) }
    | the_formula And the_formula                                       { Formula.And($1, $3) }
    | the_formula Or the_formula                                        { Formula.Or($1, $3) }
    | arithmetic_expression LT arithmetic_expression                    { Formula.Comparison(BinaryComparison.LessThan, $1, $3) }
    | arithmetic_expression GT arithmetic_expression                    { Formula.Comparison(BinaryComparison.GreaterThan, $1, $3) }
    | arithmetic_expression LE arithmetic_expression                    { Formula.Comparison(BinaryComparison.LessOrEqual, $1, $3) }
    | arithmetic_expression GE arithmetic_expression                    { Formula.Comparison(BinaryComparison.GreaterOrEqual, $1, $3) }
    | arithmetic_expression EQ arithmetic_expression                    { Formula.Comparison(BinaryComparison.Equals, $1, $3) }
    | arithmetic_expression NE arithmetic_expression                    { Formula.Comparison(BinaryComparison.NotEquals, $1, $3) }
    | Emp                                                               { Formula.EmptyHeap }
    | Identifier Arrow arithmetic_expression                            { Formula.Allocation($1, $3) }
    | Identifier Void                                                   { Formula.Deallocation($1) }
    | the_formula Star the_formula                                      { Formula.Separation($1, $3) }
    ;

arithmetic_expression:
    | Integer                                                           { ArithmeticExpression.Literal($1) }
    | Identifier                                                        { ArithmeticExpression.Variable($1) }
    | arithmetic_expression Plus arithmetic_expression                  { ArithmeticExpression.Operation(BinaryOperator.Plus, $1, $3) }
    | arithmetic_expression Minus arithmetic_expression                 { ArithmeticExpression.Operation(BinaryOperator.Minus, $1, $3) }
    | arithmetic_expression Times arithmetic_expression                 { ArithmeticExpression.Operation(BinaryOperator.Times, $1, $3) }
    | arithmetic_expression Div arithmetic_expression                   { ArithmeticExpression.Operation(BinaryOperator.Div, $1, $3) }
    | arithmetic_expression Mod arithmetic_expression                   { ArithmeticExpression.Operation(BinaryOperator.Mod, $1, $3) }
    ;