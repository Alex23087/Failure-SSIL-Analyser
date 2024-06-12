open Prelude.Ast.LogicFormulas
open Prelude.Ast.LogicFormulas.Formula
open Prelude.Ast.LogicFormulas.BinaryComparison
open Prelude.Ast.LogicFormulas.ArithmeticExpression
open Prelude.Ast.LogicFormulas.BinaryOperator

open F_utils
open Utils

let source = {|<< exists y . ( (y < 0) && (x = (0 - y)) ) >>|}

let expected: Formula.t =
  test_node (Exists(
    "y",
    test_node (And(
      test_node (Comparison(
        LessThan,
        test_node (Variable "y"),
        test_node (Literal 0)
      )),
      test_node (Comparison(
        Equals,
        test_node (Variable "x"),
        test_node (Operation( Minus, test_node (Literal 0), test_node (Variable "y")))
      ))
    ))
  ))
;;

let%test_unit "test formulae n. 05" =
  [%test_eq: Formula.t] (parse_formula source) expected