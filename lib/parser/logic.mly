// Formula ::= True | False | Exists Identifier Formula | Formula && Formula | Formula || Formula | ArithmeticExpression BinaryComparison ArithmeticExpression | Emp | x -> y | x -/> | Formula * Formula
// BinaryComparison ::= < | > | <= | >= | == | !=
// ArithmeticExpression ::= Int(n) | Identifier | ArithmeticExpression BinaryOperator ArithmeticExpression
// BinaryOperator ::= + | - | * | / | %

%token True
%token False
%token Exists
%token <string> Identifier
%token And Or Star Emp Arrow Void
%token LT GT LE GE EQ NE
%token <int> Integer
%token Plus Minus Times Div Mod
%token EOF

/* Starting symbol */

%start formula

%%

/* Grammar specification */

formula:
    | True           { Ast.Formula.True }
    | False          { Ast.Formula.False }
    // | Exists Identifier formula
    // | formula And formula
    // | formula Or formula
    // | formula LT formula
    // | formula GT formula
    // | formula LE formula
    // | formula GE formula
    // | formula EQ formula
    // | formula NE formula
    | Emp           { Ast.Formula.EmptyHeap }
    // | formula Star formula
    // | Integer
    // | Identifier
    // | formula Plus formula
    // | formula Minus formula
    // | formula Times formula
    // | formula Div formula
    // | formula Mod formula
    ;