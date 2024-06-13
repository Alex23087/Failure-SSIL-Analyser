open Prelude.Ast.Commands
open Prelude.Ast.LogicFormulas
open Prelude.Ast.Commands.AnnotatedNode
open Prelude.Ast.LogicFormulas.AnnotatedNode
open Utils

let emptyAnnotation = {Prelude.Ast.position = dummy_position; logic_formula = None}
let annotateCommand formula =
  Prelude.Ast.Commands.annotate formula emptyAnnotation

let source = {|
  x = alloc();
  while (x < 10) {
    x = x + 1
  }
|}

let expected: HeapRegularCommand.t = 
  annotateCommand (HeapRegularCommand.Sequence (
    annotateCommand(HeapRegularCommand.Command (
      annotateCommand(HeapAtomicCommand.Allocation "x")
    )),
    annotateCommand(HeapRegularCommand.Sequence(
      annotateCommand (HeapRegularCommand.Star (
        annotateCommand (HeapRegularCommand.Sequence(
          annotateCommand (HeapRegularCommand.Command (
            annotateCommand (HeapAtomicCommand.Guard (
              annotateCommand (BooleanExpression.Comparison (
                BooleanComparison.LessThan,
                annotateCommand (Prelude.Ast.Commands.ArithmeticExpression.Variable "x"),
                annotateCommand (Prelude.Ast.Commands.ArithmeticExpression.Literal 10)
              ))
            ))
          )),
          annotateCommand (HeapRegularCommand.Command (
            annotateCommand (HeapAtomicCommand.Assignment (
              "x",
              annotateCommand (Prelude.Ast.Commands.ArithmeticExpression.BinaryOperation (
                ArithmeticOperation.Plus,
                annotateCommand (Prelude.Ast.Commands.ArithmeticExpression.Variable "x"),
                annotateCommand (Prelude.Ast.Commands.ArithmeticExpression.Literal 1)
              ))
            ))
          ))
        ))
      )),
      annotateCommand (HeapRegularCommand.Command (
        annotateCommand (HeapAtomicCommand.Guard (
          annotateCommand (BooleanExpression.Not (
            annotateCommand (BooleanExpression.Comparison (
              BooleanComparison.GreaterOrEqual,
              annotateCommand (Prelude.Ast.Commands.ArithmeticExpression.Variable "x"),
              annotateCommand (Prelude.Ast.Commands.ArithmeticExpression.Literal 10)
            ))
          ))
        ))
      ))
    ))
  ))
;;
(*
let%test_unit "test commands n. 06" =
  [%test_eq: HeapRegularCommand.t] (parse_command source) expected
*)