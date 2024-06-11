open Prelude.Ast.LogicFormulas
open Prelude.Ast.LogicFormulas.Formula
open Prelude.Ast.LogicFormulas.BinaryComparison
open Prelude.Ast.LogicFormulas.ArithmeticExpression
open Prelude.Ast.LogicFormulas.BinaryOperator

open F_utils
open Utils

let source = {|<< exists n . exists m . 5 % n = m >>|}

let expected: Formula.t =
  test_node ( Exists (
    "n",
    test_node ( Exists (
      "m",
      test_node (Comparison (
        Equals,
        test_node (Operation ( Modulo, test_node (Literal 5), test_node (Variable "n"))),
        test_node (Variable "m")
      ))
    ))
  ))
;;

let%test_unit "test formulae n. 08" =
  [%test_eq: Formula.t] (parse_formula source) expected