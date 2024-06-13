open Prelude.Ast.Commands
open Prelude.Ast.LogicFormulas
open Prelude.Ast.Commands.AnnotatedNode
open Prelude.Ast.LogicFormulas.AnnotatedNode
open Utils

let emptyAnnotation = {Prelude.Ast.position = dummy_position; logic_formula = None}
let annotateCommand formula =
  Prelude.Ast.Commands.annotate formula emptyAnnotation

let source = {|
  if (x == 0) then {
    x = 1
  } else {
    skip
  }
|}

let expected: HeapRegularCommand.t = 
  annotateCommand (HeapRegularCommand.NondeterministicChoice (
    annotateCommand (HeapRegularCommand.Sequence (
      annotateCommand (HeapRegularCommand.Command (
        annotateCommand (HeapAtomicCommand.Guard (
          annotateCommand (BooleanExpression.Comparison (
            BooleanComparison.Equal,
            annotateCommand (Prelude.Ast.Commands.ArithmeticExpression.Variable "x"),
            annotateCommand (Prelude.Ast.Commands.ArithmeticExpression.Literal 0)
          ))
        ))
      )),
      annotateCommand (HeapRegularCommand.Command (
        annotateCommand (HeapAtomicCommand.Assignment (
          "x",
          annotateCommand (Prelude.Ast.Commands.ArithmeticExpression.Literal 1)
        ))
      ))
    )),
    annotateCommand (HeapRegularCommand.Sequence (
      annotateCommand (HeapRegularCommand.Command (
        annotateCommand (HeapAtomicCommand.Guard (
          annotateCommand (BooleanExpression.Not(
            annotateCommand (BooleanExpression.Comparison (
              BooleanComparison.Equal,
              annotateCommand (Prelude.Ast.Commands.ArithmeticExpression.Variable "x"),
              annotateCommand (Prelude.Ast.Commands.ArithmeticExpression.Literal 0)
            ))
          ))
          
        ))
      )),
      annotateCommand (HeapRegularCommand.Command (
        annotateCommand (HeapAtomicCommand.Skip)
      ))
    ))
  ))

;;

let%test_unit "test commands n. 05" =
  [%test_eq: HeapRegularCommand.t] (parse_command source) expected
