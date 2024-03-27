(* Grammar:
   CL ::=  TRUE | FALSE | exists Identifier CL | CL && CL | CL || CL | ArithmeticExpression BinaryComparison ArithmeticExpression | TemporalExpression
   BinaryComparison ::= < | > | <= | >= | == | !=
   ArithmeticExpression ::= INT(n)  |  Identifier  |  ArithmeticExpression BinaryOperator ArithmeticExpression
   BinaryOperator ::=  + | - | * | / | % | ^
 *)

module AnnotationLogic(Annotation: Base.AnnotationType) = struct
  module AnnotatedNode = Base.AnnotatedNode(Annotation)

  module BinaryOperator = struct
    type t =
      | Plus
      | Minus
      | Times
      | Division
      | Modulo
    [@@deriving show]
  end

  module ArithmeticExpression = struct
    type t_node =
      | Literal of int
      | Variable of Base.identifier
      | Operation of BinaryOperator.t * t * t
    and t = t_node AnnotatedNode.t
    [@@deriving show]
  end

  module BinaryComparison = struct
    type t =
      | LessThan
      | GreaterThan
      | LessOrEqual
      | GreaterOrEqual
      | Equals
      | NotEquals
    [@@deriving show]
  end

  module LogicFormula = struct
    type t_node =
      | True
      | False
      | Exists of Base.identifier * t
      | And of t * t
      | Or of t * t
      | Comparison of BinaryComparison.t * ArithmeticExpression.t * ArithmeticExpression.t
    and t = t_node AnnotatedNode.t
    [@@deriving show]
  end

  type t = LogicFormula.t
  let pp = LogicFormula.pp
  let show = LogicFormula.show
end
