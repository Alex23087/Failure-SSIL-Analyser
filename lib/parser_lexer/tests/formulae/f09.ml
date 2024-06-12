open Prelude.Ast.LogicFormulas
open Prelude.Ast.LogicFormulas.Formula
open Prelude.Ast.LogicFormulas.BinaryComparison
open Prelude.Ast.LogicFormulas.ArithmeticExpression
open Prelude.Ast.LogicFormulas.BinaryOperator

open F_utils
open Utils

let source = {|<< (5 != x) * (true && false) >>|}

let expected: Formula.t =
  test_node (AndSeparately ( 
    test_node (Comparison (
      NotEquals, test_node (Literal 5), test_node (Variable "x")
    )),
    test_node (And (
      test_node True,
      test_node False
    ))
  ))
;;

let%test_unit "test formulae n. 09" =
  [%test_eq: Formula.t] (parse_formula source) expected