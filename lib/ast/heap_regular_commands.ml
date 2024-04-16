(**{1 Heap Regular Commands}*)

(**This is the Abstract Syntax Tree which represents the programs we want to analyze.
  The data structure allows to add generic annotations to most of the grammar nodes, which
  in out case will be used to store position information in the source files and logic formulas
  associated to a specific command.

  The following is the grammar definition for our programs: 
  
  - {{! HeapRegularCommands.HeapRegularCommand}HeapRegularCommand} ::= HeapAtomicCommand | HeapRegularCommand; HeapRegularCommand | HeapRegularCommand + HeapRegularCommand | HeapRegularCommand*
  - {{! HeapRegularCommands.HeapAtomicCommand}HeapAtomicCommand} ::= skip | Identifier = ArithmeticExpression | BooleanExpression ? | Identifier = alloc() | free(Identifier) | Identifier = \[Identifier\]  | \[Identifier\] = ArithmeticExpression
  - {{! HeapRegularCommands.BooleanExpression}BooleanExpression} ::= True | False | !BooleanExpression | BooleanExpression && BooleanExpression	 |  BooleanExpression || BooleanExpression  |  ArithmeticExpression BooleanComparison ArithmeticExpression
  - {{! HeapRegularCommands.BooleanComparison}BooleanComparison} ::= == | != | < | <= | > | >=
  - {{! HeapRegularCommands.ArithmeticExpression}ArithmeticExpression} ::= Int(n) | Identifier | ArithmeticExpression BinaryOperator ArithmeticExpression
  - {{! HeapRegularCommands.ArithmeticOperation}BinaryOperator} ::= + | - | * | / | %
*)
module HeapRegularCommands(Annotation: Base.AnnotationType) = struct
  open Base
  module AnnotatedNode = Base.AnnotatedNode(Annotation)

  module ArithmeticOperation = struct
    type t =
      | Plus
      | Minus
      | Times
      | Division
      | Modulo
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

  module HeapAtomicCommand = struct
    type t_node =
      | Skip
      | Assignment of identifier * ArithmeticExpression.t
      | NonDet of identifier
      | Guard of BooleanExpression.t
      | Allocation of identifier
      | Free of identifier
      | ReadHeap of identifier * identifier
      | WriteHeap of identifier * ArithmeticExpression.t
    and t = t_node AnnotatedNode.t
    [@@deriving show]

    let modifiedVariables (command: t) =
      match command.node with
      | Skip                  -> (IdentifierSet.empty)
      | Assignment(id, _)     -> (IdentifierSet.singleton id)
      | NonDet(id)            -> (IdentifierSet.singleton id)
      | Guard(_)              -> (IdentifierSet.empty) 
      | Allocation(id)        -> (IdentifierSet.singleton id)
      | Free(_)               -> (IdentifierSet.empty)
      | ReadHeap(id, _)       -> (IdentifierSet.singleton id)
      | WriteHeap(_, _)       -> (IdentifierSet.empty)
  end

  module HeapRegularCommand = struct
    type t_node =
      | Command of HeapAtomicCommand.t
      | Sequence of t * t
      | NondeterministicChoice of t * t
      | Star of t
    and t = t_node AnnotatedNode.t
    [@@deriving show]

    let rec modifiedVariables (command: t) = 
      match command.node with
      | Command(atomicCommand) ->
         HeapAtomicCommand.modifiedVariables atomicCommand
      | NondeterministicChoice(regularCommand1, regularCommand2) ->
         IdentifierSet.union (modifiedVariables regularCommand1) (modifiedVariables regularCommand2)
      | Sequence(regularCommand1, regularCommand2) ->
         IdentifierSet.union (modifiedVariables regularCommand1) (modifiedVariables regularCommand2)
      | Star(regularCommand) ->
         modifiedVariables regularCommand
  end

  type t = HeapRegularCommand.t
  let pp = HeapRegularCommand.pp
  let show = HeapRegularCommand.show
  let modifiedVariables = HeapRegularCommand.modifiedVariables
end
