(* Grammar:
   RegularCommand ::=  AtomicCommand  |  RegularCommand; RegularCommand  |  RegularCommand + RegularCommand  |  RegularCommand*
   
   AtomicCommand ::=  SKIP  |  Identifier = ArithmeticExpression  |  BooleanExpression ?
   
   BooleanExpression ::=  TRUE  |  FALSE  |  !BooleanExpression  |  BooleanExpression && BooleanExpression  |  BooleanExpression || BooleanExpression  |  ArithmeticExpression BooleanComparison ArithmeticExpression
   
   BooleanComparison ::=  =  |  !=  |  <  |  <=  |  >  |  >=
   
   ArithmeticExpression ::=  INT(n)  |  Identifier  |  ArithmeticExpression BinaryOperator ArithmeticExpression
   
   BinaryOperator ::=  +  |  -  |  *  |  /  
*)

module RegularCommands(Annotation: Base.AnnotationType) = struct
  module AnnotatedNode = Base.AnnotatedNode(Annotation)

  module ArithmeticOperation = struct
    type t =
      | Plus
      | Minus
      | Times
      | Division
    [@@deriving show]
  end

  module BooleanComparison = struct
    type t =
      | Equal
      | NotEqual
      | LessThan
      | LessOrEqual
      | GreaterThan
      | GreaterOrEqual
    [@@deriving show]
  end

  module ArithmeticExpression = struct
    type t_node =
      | Literal of int
      | Variable of Base.identifier
      | BinaryOperation of ArithmeticOperation.t * t * t
    and t = t_node AnnotatedNode.t
    [@@deriving show]
  end

  module BooleanExpression = struct
    type t_node =
      | True
      | False
      | Not of t
      | And of t * t
      | Or of t * t
      | Comparison of BooleanComparison.t * ArithmeticExpression.t * ArithmeticExpression.t
    and t = t_node AnnotatedNode.t
    [@@deriving show]
  end

  module AtomicCommand = struct
    type t_node =
      | Skip
      | Assignment of Base.identifier * ArithmeticExpression.t
      | Guard of BooleanExpression.t
    and t = t_node AnnotatedNode.t
    [@@deriving show]
  end

  module RegularCommand = struct
    type t_node =
      | Command of AtomicCommand.t
      | Sequence of t * t
      | NondeterministicChoice of t * t
      | Star of t
    and t = t_node AnnotatedNode.t
    [@@deriving show]
  end

  let show = RegularCommand.show
end