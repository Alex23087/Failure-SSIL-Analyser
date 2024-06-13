open Prelude.Ast.Commands
open Prelude.Ast.LogicFormulas
open Prelude.Ast.Commands.AnnotatedNode
open Prelude.Ast.LogicFormulas.AnnotatedNode
open Utils

let emptyAnnotation = {Prelude.Ast.position = dummy_position; logic_formula = None}
let annotateCommand formula =
  Prelude.Ast.Commands.annotate formula emptyAnnotation

let source = {|
  skip;
  skip;
  skip;
  skip
|}

let expected: HeapRegularCommand.t = 
  annotateCommand (HeapRegularCommand.Sequence (
    annotateCommand (HeapRegularCommand.Sequence (
      annotateCommand (HeapRegularCommand.Sequence (
        annotateCommand (HeapRegularCommand.Command (annotateCommand (HeapAtomicCommand.Skip))),
        annotateCommand (HeapRegularCommand.Command (annotateCommand (HeapAtomicCommand.Skip)))
      )),
      annotateCommand (HeapRegularCommand.Command (annotateCommand (HeapAtomicCommand.Skip)))
    )),
    annotateCommand (HeapRegularCommand.Command (annotateCommand (HeapAtomicCommand.Skip)))
  ))

;;

let%test_unit "test commands n. 07" =
  [%test_eq: HeapRegularCommand.t] (parse_command source) expected