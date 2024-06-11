open Prelude.Ast.LogicFormulas
open Prelude.Ast.LogicFormulas.Formula
open Prelude.Ast.LogicFormulas.BinaryComparison
open Prelude.Ast.LogicFormulas.ArithmeticExpression
open Prelude.Ast.LogicFormulas.BinaryOperator

open F_utils
open Utils

let source = {|<< n * 2 < (1 + 2) - (3 / 4) >>|}

let expected: Formula.t = 
  test_node (Comparison (
    LessThan,
    test_node (Operation (
      Times, test_node (Variable "n"), test_node (Literal 2)
    )),
    test_node (Operation (
      Minus,
      test_node (Operation (Plus, test_node (Literal 1), test_node (Literal 2))),
      test_node (Operation (Division, test_node (Literal 3), test_node (Literal 4)))
    ))
  ))
;;

let%test_unit "test formulae n. 10" =
  [%test_eq: Formula.t] (parse_formula source) expected