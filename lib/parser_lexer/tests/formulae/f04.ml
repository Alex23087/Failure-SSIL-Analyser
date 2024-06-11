open Prelude.Ast.LogicFormulas
open Prelude.Ast.LogicFormulas.Formula
open Prelude.Ast.LogicFormulas.BinaryComparison
open Prelude.Ast.LogicFormulas.ArithmeticExpression
open Prelude.Ast.LogicFormulas.BinaryOperator

open F_utils
open Utils

let source = {|<< exists n . ((0 + n >= 20000) && (n > 0))>>|}

let expected: Formula.t =
  test_node (Exists(
    "n",
    test_node (And(
      test_node (Comparison(
        GreaterOrEqual,
        test_node (Operation (Plus, test_node (Literal 0), test_node (Variable "n"))),
        test_node (Literal 20000))),
      test_node (Comparison(
        GreaterThan,
        test_node (Variable "n"),
        test_node (Literal 0)))
    ))
  ))

;;

let%test_unit "test formulae n. 04" =
  [%test_eq: Formula.t] (parse_formula source) expected