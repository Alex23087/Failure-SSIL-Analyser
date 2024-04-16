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
