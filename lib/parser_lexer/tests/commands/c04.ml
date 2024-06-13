open Prelude.Ast.Commands
open Prelude.Ast.LogicFormulas
open Prelude.Ast.Commands.AnnotatedNode
open Prelude.Ast.LogicFormulas.AnnotatedNode
open Utils

let emptyAnnotation = {Prelude.Ast.position = dummy_position; logic_formula = None}
let annotateCommand formula =
  Prelude.Ast.Commands.annotate formula emptyAnnotation

let source = {|x = alloc();
y = alloc();
if (x == y || x != y && true) then {
  [x] = 42;
  free(x)
} else {
  [y] = 0;
  free(y)
};
skip
|}

let expected: HeapRegularCommand.t = 
  annotateCommand (HeapRegularCommand.Sequence (
    annotateCommand (HeapRegularCommand.Sequence (
      annotateCommand (HeapRegularCommand.Sequence (
        annotateCommand (HeapRegularCommand.Command (
          annotateCommand (HeapAtomicCommand.Allocation "x")
        )),
        annotateCommand (HeapRegularCommand.Command (
          annotateCommand (HeapAtomicCommand.Allocation "y")
        ))
      )),
      annotateCommand (HeapRegularCommand.NondeterministicChoice(
        annotateCommand (HeapRegularCommand.Sequence(
          annotateCommand (HeapRegularCommand.Command ( annotateCommand (HeapAtomicCommand.Guard(
            annotateCommand (BooleanExpression.Or (
              annotateCommand (BooleanExpression.Comparison (
                BooleanComparison.Equal,
                annotateCommand (Prelude.Ast.Commands.ArithmeticExpression.Variable "x"),
                annotateCommand (Prelude.Ast.Commands.ArithmeticExpression.Variable "y")
              )),
              annotateCommand (BooleanExpression.And (
                annotateCommand (BooleanExpression.Comparison (
                  BooleanComparison.NotEqual,
                  annotateCommand (Prelude.Ast.Commands.ArithmeticExpression.Variable "x"),
                  annotateCommand (Prelude.Ast.Commands.ArithmeticExpression.Variable "y")
                )),
                annotateCommand (BooleanExpression.True)
              ))
            ))
          )))), (*body*)
          annotateCommand (HeapRegularCommand.Sequence (
            annotateCommand (HeapRegularCommand.Command (
              annotateCommand (HeapAtomicCommand.WriteHeap (
                "x",
                annotateCommand (Prelude.Ast.Commands.ArithmeticExpression.Literal 42)
              ))
            )),
            annotateCommand (HeapRegularCommand.Command (
              annotateCommand (HeapAtomicCommand.Free "x")
            ))
          ))
        )), (*not b?;else*)
        annotateCommand (HeapRegularCommand.Sequence(
          annotateCommand (HeapRegularCommand.Command ( annotateCommand (HeapAtomicCommand.Guard(
            annotateCommand (BooleanExpression.Not(
              annotateCommand (BooleanExpression.Or (
                annotateCommand (BooleanExpression.Comparison (
                  BooleanComparison.Equal,
                  annotateCommand (Prelude.Ast.Commands.ArithmeticExpression.Variable "x"),
                  annotateCommand (Prelude.Ast.Commands.ArithmeticExpression.Variable "y")
                )),
                annotateCommand (BooleanExpression.And (
                  annotateCommand (BooleanExpression.Comparison (
                    BooleanComparison.NotEqual,
                    annotateCommand (Prelude.Ast.Commands.ArithmeticExpression.Variable "x"),
                    annotateCommand (Prelude.Ast.Commands.ArithmeticExpression.Variable "y")
                  )),
                  annotateCommand (BooleanExpression.True)
                ))
              ))
            ))
          )))),
          annotateCommand (HeapRegularCommand.Sequence( (*else body*)
            annotateCommand (HeapRegularCommand.Command (
              annotateCommand (HeapAtomicCommand.WriteHeap (
                "y",
                annotateCommand (Prelude.Ast.Commands.ArithmeticExpression.Literal 0)
              ))
            )),
            annotateCommand (HeapRegularCommand.Command (
              annotateCommand (HeapAtomicCommand.Free "y")
            ))
          ))
        ))
      ))
    )),
    annotateCommand (HeapRegularCommand.Command (
        annotateCommand HeapAtomicCommand.Skip
    ))
  ))
;;

let%test_unit "test commands n. 04" =
  [%test_eq: HeapRegularCommand.t] (parse_command source) expected
