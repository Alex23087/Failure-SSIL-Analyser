open Prelude.Ast.Commands
open Prelude.Ast.Commands.AnnotatedNode
open Utils

let source = {|skip|}

let expected: HeapRegularCommand.t = {
  node = HeapRegularCommand.Command {
    node = HeapAtomicCommand.Skip;
    annotation = {position = dummy_position; logic_formula = None}
  };
  annotation = {position = dummy_position; logic_formula = None}
}
;;

let%test_unit "test commands n. 01" =
  [%test_eq: HeapRegularCommand.t] (parse_command source) expected