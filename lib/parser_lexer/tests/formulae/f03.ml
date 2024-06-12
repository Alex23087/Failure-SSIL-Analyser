open Prelude.Ast.LogicFormulas
open Prelude.Ast.LogicFormulas.Formula
open Prelude.Ast.LogicFormulas.BinaryComparison
open Prelude.Ast.LogicFormulas.ArithmeticExpression
open Prelude.Ast.LogicFormulas.BinaryOperator

open F_utils
open Utils

let source = {|<< True || False >>|}

let expected: Formula.t =
  test_node (And( test_node (True), test_node (False)))
;;

let%test_unit "test formulae n. 03" =
  [%test_eq: Formula.t] (parse_formula source) expected