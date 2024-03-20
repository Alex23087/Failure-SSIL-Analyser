(* Grammar:
   RegularCommand ::=  BasicCommand  |  RegularCommand; RegularCommand  |  RegularCommand + RegularCommand  |  RegularCommand*
   
   BasicCommand ::=  SKIP  |  Identifier = ArithmeticExpression  |  BooleanExpression ?
   
   BooleanExpression ::=  TRUE  |  FALSE  |  !BooleanExpression  |  BooleanExpression && BooleanExpression  |  BooleanExpression || BooleanExpression  |  ArithmeticExpression BooleanComparison ArithmeticExpression
   
   BooleanComparison ::=  =  |  !=  |  <  |  <=  |  >  |  >=
   
   ArithmeticExpression ::=  INT(n)  |  Identifier  |  ArithmeticExpression BinaryOperator ArithmeticExpression
   
   BinaryOperator ::=  +  |  -  |  *  |  /  
*)
module type AnnotationType = sig
  type t
end

module ASTRegularCommands(Annotation: AnnotationType) = struct
  type t = Annotation.t
  type identifier = string [@@deriving show]
  type 'a annotated_node = {node: 'a; annotation: t [@opaque]} [@@deriving show]
  let getAnnotation (node: 'a annotated_node) = node.annotation
  let removeAnnotation (node: 'a annotated_node) = node.node

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
      | Variable of identifier
      | BinaryOperation of ArithmeticOperation.t * t * t
    and t = t_node annotated_node
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
    and t = t_node annotated_node
    [@@deriving show]
  end

  module BasicCommand = struct
    type t_node =
      | Skip
      | Assignment of identifier * ArithmeticExpression.t
      | Guard of BooleanExpression.t
    and t = t_node annotated_node
    [@@deriving show]
  end

  module RegularCommand = struct
    type t_node =
      | Command of BasicCommand.t
      | Sequence of t * t
      | NondeterministicChoice of t * t
      | Star of t
    and t = t_node annotated_node
    [@@deriving show]
  end

  let show = RegularCommand.show
end