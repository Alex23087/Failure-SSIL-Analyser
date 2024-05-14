open Prelude.Ast.LogicFormulas
open Prelude.Ast.LogicFormulas.AnnotatedNode
open Utils

let source = {|<< true >>|}

let expected: Formula.t = {
  node = True;
  annotation = {position = dummy_position}
}
;;

let%test_unit "test formulae n. 01" =
  [%test_eq: Formula.t] (parse_formula source) expected