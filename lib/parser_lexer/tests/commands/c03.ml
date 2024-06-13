open Prelude.Ast.Commands
open Prelude.Ast.LogicFormulas
open Prelude.Ast.Commands.AnnotatedNode
open Prelude.Ast.LogicFormulas.AnnotatedNode
open Utils

let emptyAnnotation = {Prelude.Ast.position = dummy_position; logic_formula = None}
let annotateCommand formula =
  Prelude.Ast.Commands.annotate formula emptyAnnotation

let source = {|z = alloc();
[z] = 5 * (10 + 2);
while (z != 0) {
  z = z - 1
};
free(z)
|}

let expected: HeapRegularCommand.t = 

  annotateCommand (HeapRegularCommand.Sequence( 
    annotateCommand (HeapRegularCommand.Sequence (
      annotateCommand (HeapRegularCommand.Sequence (
        annotateCommand (HeapRegularCommand.Command (
          annotateCommand (HeapAtomicCommand.Allocation "z")
        )),
        annotateCommand (HeapRegularCommand.Command (
          annotateCommand (HeapAtomicCommand.WriteHeap (
            "z",
            annotateCommand (Prelude.Ast.Commands.ArithmeticExpression.BinaryOperation (
              ArithmeticOperation.Times,
              annotateCommand (Prelude.Ast.Commands.ArithmeticExpression.Literal 5),
              annotateCommand (Prelude.Ast.Commands.ArithmeticExpression.BinaryOperation (
                ArithmeticOperation.Plus,
                annotateCommand (Prelude.Ast.Commands.ArithmeticExpression.Literal 10),
                annotateCommand (Prelude.Ast.Commands.ArithmeticExpression.Literal 2)
              ))
            ))
          ))
        ))
      )),
      annotateCommand (HeapRegularCommand.Sequence (
        annotateCommand (HeapRegularCommand.Star (
          annotateCommand (HeapRegularCommand.Sequence (
            annotateCommand (HeapRegularCommand.Command (annotateCommand (HeapAtomicCommand.Guard(
              annotateCommand (BooleanExpression.Comparison(
                BooleanComparison.NotEqual,
                annotateCommand (Prelude.Ast.Commands.ArithmeticExpression.Variable "z"),
                annotateCommand (Prelude.Ast.Commands.ArithmeticExpression.Literal 0)
              ))
            )))),
            annotateCommand (HeapRegularCommand.Command(
              annotateCommand (HeapAtomicCommand.Assignment(
                "z",
                annotateCommand (Prelude.Ast.Commands.ArithmeticExpression.BinaryOperation (
                  ArithmeticOperation.Minus,
                  annotateCommand (Prelude.Ast.Commands.ArithmeticExpression.Variable "z"),
                  annotateCommand (Prelude.Ast.Commands.ArithmeticExpression.Literal 1)
                ))
              ))
            ))
          ))
        )),
        annotateCommand (HeapRegularCommand.Command (annotateCommand (HeapAtomicCommand.Guard(
          annotateCommand (BooleanExpression.Not( 
            annotateCommand (BooleanExpression.Comparison(
              BooleanComparison.NotEqual,
              annotateCommand (Prelude.Ast.Commands.ArithmeticExpression.Variable "z"),
              annotateCommand (Prelude.Ast.Commands.ArithmeticExpression.Literal 0)
            ))
          ))
        ))))
      ))
    )),
    annotateCommand (HeapRegularCommand.Command( annotateCommand (HeapAtomicCommand.Free "z") ))
    
  ))

  
;;

let%test_unit "test commands n. 03" =
  [%test_eq: HeapRegularCommand.t] (parse_command source) expected
