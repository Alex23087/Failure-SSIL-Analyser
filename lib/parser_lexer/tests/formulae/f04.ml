open Prelude.Ast.LogicFormulas
open Prelude.Ast.LogicFormulas.Formula
open Prelude.Ast.LogicFormulas.BinaryComparison
open Prelude.Ast.LogicFormulas.ArithmeticExpression
open Prelude.Ast.LogicFormulas.BinaryOperator

open F_utils
open Utils

let source = {|<< x = 1 >>|}

let expected: Formula.t =
  test_node (Comparison(Equals, test_node (Variable "x"), test_node (Literal 1)))
;;

let%test_unit "test formulae n. 04 - 1" =
  [%test_eq: Formula.t] (parse_formula source) expected


let source = {|<< 1 < 2 >>|}

let expected: Formula.t =
  test_node (Comparison(LessThan, test_node (Literal 1), test_node (Literal 2)))
;;

let%test_unit "test formulae n. 04 - 2" =
  [%test_eq: Formula.t] (parse_formula source) expected


let source = {|<< x > 3 >>|}

let expected: Formula.t =
  test_node (Comparison(GreaterThan, test_node (Variable "x"), test_node (Literal 3)))
;;

let%test_unit "test formulae n. 04 - 3" =
  [%test_eq: Formula.t] (parse_formula source) expected


let source = {|<< x <= y >>|}

let expected: Formula.t =
  test_node (Comparison(LessOrEqual, test_node (Variable "x"), test_node (Variable "y")))
;;

let%test_unit "test formulae n. 04 - 4" =
  [%test_eq: Formula.t] (parse_formula source) expected


let source = {|<< x >= 1 >>|}

let expected: Formula.t =
  test_node (Comparison(GreaterOrEqual, test_node (Variable "x"), test_node (Literal 1)))
;;

let%test_unit "test formulae n. 04 - 5" =
  [%test_eq: Formula.t] (parse_formula source) expected


let source = {|<< x != 1 >>|}

let expected: Formula.t =
  test_node (Comparison(NotEquals, test_node (Variable "x"), test_node (Literal 1)))
;;

let%test_unit "test formulae n. 04 - 6" =
  [%test_eq: Formula.t] (parse_formula source) expected