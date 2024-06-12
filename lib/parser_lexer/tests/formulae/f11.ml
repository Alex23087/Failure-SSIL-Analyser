open Prelude.Ast.LogicFormulas
open Prelude.Ast.LogicFormulas.Formula
open Prelude.Ast.LogicFormulas.BinaryComparison
open Prelude.Ast.LogicFormulas.ArithmeticExpression
open Prelude.Ast.LogicFormulas.BinaryOperator

open F_utils
open Utils

let source = {|<< (y -> v) * (v = x) >>|}

let expected: Formula.t = 
  test_node (AndSeparately (
    test_node (Allocation ("y", test_node (Variable "v"))),
    test_node (Comparison (Equals, test_node (Variable "v"), test_node(Variable "x")))
  ))
;;

let%test_unit "test formulae n. 11" =
  [%test_eq: Formula.t] (parse_formula source) expected