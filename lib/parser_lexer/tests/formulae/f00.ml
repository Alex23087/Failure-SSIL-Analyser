open Prelude.Ast.LogicFormulas
open Prelude.Ast.LogicFormulas.Formula
open Prelude.Ast.LogicFormulas.BinaryComparison
open Prelude.Ast.LogicFormulas.ArithmeticExpression
open Prelude.Ast.LogicFormulas.BinaryOperator

open F_utils
open Utils

let source = {|<< true >>|}

let expected: Formula.t = test_node (True)
;;

let%test_unit "test formulae n. 00" =
  [%test_eq: Formula.t] (parse_formula source) expected