open Prelude.Ast.LogicFormulas
open Prelude.Ast.LogicFormulas.Formula
open Prelude.Ast.LogicFormulas.BinaryComparison
open Prelude.Ast.LogicFormulas.ArithmeticExpression
open Prelude.Ast.LogicFormulas.BinaryOperator

open F_utils
open Utils

let source = {|<< (5 + 5) = 5 && (true) >>|}

let expected: Formula.t =
  test_node (And (
    test_node (Comparison(
      Equals,
      test_node (Operation(Plus, test_node(Literal 5), test_node (Literal 5))),
      test_node (Literal 5)
    )),
    test_node (True)
  ))

let%test_unit "test formulae n. 06" =
  [%test_eq: Formula.t] (parse_formula source) expected