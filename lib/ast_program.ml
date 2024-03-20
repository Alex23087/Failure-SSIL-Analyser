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
    type arithmeticOperation =
      | Plus
      | Minus
      | Times
      | Division
    [@@deriving show]
  end

  module BooleanComparison = struct
    type comparison =
      | Equal
      | NotEqual
      | LessThan
      | LessOrEqual
      | GreaterThan
      | GreaterOrEqual
    [@@deriving show]
  end

  module ArithmeticExpression = struct
    type arithmeticExpression_node =
      | Literal of int
      | Variable of identifier
      | BinaryOperation of ArithmeticOperation.arithmeticOperation * arithmeticExpression * arithmeticExpression
    and arithmeticExpression = arithmeticExpression_node annotated_node
    [@@deriving show]
  end

  module BooleanExpression = struct
    type booleanExpression_node =
      | True
      | False
      | Not of booleanExpression
      | And of booleanExpression * booleanExpression
      | Or of booleanExpression * booleanExpression
      | Comparison of BooleanComparison.comparison * ArithmeticExpression.arithmeticExpression * ArithmeticExpression.arithmeticExpression
    and booleanExpression = booleanExpression_node annotated_node
    [@@deriving show]
  end

  module BasicCommand = struct
    type basicCommand_node =
      | Skip
      | Assignment of identifier * ArithmeticExpression.arithmeticExpression
      | Guard of BooleanExpression.booleanExpression
    and basicCommand = basicCommand_node annotated_node
    [@@deriving show]
  end

  module RegularCommand = struct
    type regularCommand_node =
      | Command of BasicCommand.basicCommand
      | Sequence of regularCommand * regularCommand
      | NondeterministicChoice of regularCommand * regularCommand
      | Star of regularCommand
    and regularCommand = regularCommand_node annotated_node
    [@@deriving show]
  end

  let show = RegularCommand.show_regularCommand
end