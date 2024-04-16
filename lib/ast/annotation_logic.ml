(**{1 Annotation Logic}*)

(**This is the Abstract Syntax Tree which represents the logic formulas used to annotate our programs.
  The data structure allows to add generic annotations to most of the grammar nodes, which
  in out case will be used to store position information in the source files.

  The following is the grammar definition for our programs:
  - {{! AnnotationLogic.Formula}Formula} ::= True | False | Exists Identifier Formula | Formula && Formula | Formula || Formula | ArithmeticExpression BinaryComparison ArithmeticExpression | Emp | x -> y | x -/> | Formula * Formula
  - {{! AnnotationLogic.BinaryComparison}BinaryComparison} ::= < | > | <= | >= | == | !=
  - {{! AnnotationLogic.ArithmeticExpression}ArithmeticExpression} ::= Int(n) | Identifier | ArithmeticExpression BinaryOperator ArithmeticExpression
  - {{! AnnotationLogic.BinaryOperator}BinaryOperator} ::= + | - | * | / | %
*)
module AnnotationLogic(Annotation: Base.AnnotationType) = struct
  open Base
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
      | Variable of identifier
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

  module Formula = struct
    type t_node =
      | True
      | False
      | Exists of identifier * t
      | And of t * t
      | Or of t * t
      | Comparison of BinaryComparison.t * ArithmeticExpression.t * ArithmeticExpression.t

      (* Spatial Formulas *)
      | EmptyHeap
      | NonAllocated of identifier
      | Allocation of identifier * ArithmeticExpression.t
      | AndSeparately of t * t
    and t = t_node AnnotatedNode.t
    [@@deriving show]
  end

  type t = Formula.t
  let pp = Formula.pp
  let show = Formula.show
end
