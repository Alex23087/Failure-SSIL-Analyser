open Prelude.Ast.LogicFormulas
open Prelude.Ast.LogicFormulas.AnnotatedNode
open Utils

let source_00 = {|<< true >>|}

let expected_00: Formula.t = {
  node = True;
  annotation = {position = dummy_position}
}
;;

let%test_unit "parser_test_00" =
  [%test_eq: Formula.t] (parse_formula source_00) expected_00