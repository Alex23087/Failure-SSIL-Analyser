open Prelude.Ast.LogicFormulas
open Prelude.Ast.LogicFormulas.Formula
open Prelude.Ast.LogicFormulas.BinaryComparison
open Prelude.Ast.LogicFormulas.ArithmeticExpression
open Prelude.Ast.LogicFormulas.BinaryOperator

open F_utils
open Utils

let source = {|<< ((y -> v) * (x = y)) && (x = v) >>|}

let expected: Formula.t =
  test_node (And (
    test_node (AndSeparately (
        test_node (Allocation ("y", test_node(Variable "v"))), 
        test_node (Comparison ( Equals, test_node (Variable "x"), test_node (Variable "y")))
    )),
    test_node (Comparison ( Equals, test_node (Variable "x"), test_node (Variable "v")))
  ))
;;


let%test_unit "test formulae n. 12" =
  [%test_eq: Formula.t] (parse_formula source) expected