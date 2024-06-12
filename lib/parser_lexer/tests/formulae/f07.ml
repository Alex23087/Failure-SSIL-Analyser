open Prelude.Ast.LogicFormulas
open Prelude.Ast.LogicFormulas.Formula
open Prelude.Ast.LogicFormulas.BinaryComparison
open Prelude.Ast.LogicFormulas.ArithmeticExpression
open Prelude.Ast.LogicFormulas.BinaryOperator

open F_utils
open Utils

let source = {|<< x -/> || emp >>|}

let expected: Formula.t =
  test_node (Or (
    test_node (NonAllocated "x"),
    test_node (EmptyHeap)
  ))
;;

let%test_unit "test formulae n. 07" =
  [%test_eq: Formula.t] (parse_formula source) expected