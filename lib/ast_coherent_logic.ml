(* Grammar:
   CL ::=  TRUE | FALSE | exists Identifier CL | CL && CL | CL || CL | ArithmeticExpression BinaryComparison ArithmeticExpression | TemporalExpression
   BinaryComparison ::= < | > | <= | >= | == | !=
   ArithmeticExpression ::= INT(n)  |  Identifier  |  ArithmeticExpression BinaryOperator ArithmeticExpression
   BinaryOperator ::=  + | - | * | / | % | ^
 *)

module type AnnotationType = sig
  type t
end

module ASTCoherentCommands(Annotation: AnnotationType) = struct
  type t = Annotation.t
  type identifier = string [@@deriving show]
  type 'a annotated_node = {node: 'a; annotation: t [@opaque]} [@@deriving show]
  let addAnnotation (node: 'a) (annotation: t) = {node; annotation}
  let getAnnotation (node: 'a annotated_node) = node.annotation
  let removeAnnotation (node: 'a annotated_node) = node.node

  module BinaryOperator = struct
    type t =
      | Plus
      | Minus
      | Times
      | Division
      | Modulo
      | Exponent
    [@@deriving show]
  end

  module ArithmeticExpression = struct
    type t_node =
      | Literal of int
      | Variable of identifier
      | Operation of BinaryOperator.t * t * t
    and t = t_node annotated_node
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

  module CoherentCommand = struct
    type t_node =
      | True
      | False
      | Exists of identifier * t
      | And of t * t
      | Or of t * t
      | Comparison of BinaryComparison.t * ArithmeticExpression.t * ArithmeticExpression.t
    and t = t_node annotated_node
    [@@deriving show]
  end

  let show = CoherentCommand.show
end
