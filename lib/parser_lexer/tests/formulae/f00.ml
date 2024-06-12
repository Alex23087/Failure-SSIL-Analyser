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

let%test_unit "test formulae n. 00 - 1" =
  [%test_eq: Formula.t] (parse_formula source) expected


let source = {|<< false >>|}

let expected: Formula.t = test_node (False)
;;

let%test_unit "test formulae n. 00 - 2" =
  [%test_eq: Formula.t] (parse_formula source) expected


let source = {|<< emp >>|}

let expected: Formula.t = test_node (EmptyHeap)
;;

let%test_unit "test formulae n. 00 - 3" =
  [%test_eq: Formula.t] (parse_formula source) expected


let source = {|<< x -> 0 >>|}

let expected: Formula.t = test_node (Allocation("x", test_node (Literal 0)))
;;

let%test_unit "test formulae n. 00 - 4" =
  [%test_eq: Formula.t] (parse_formula source) expected


let source = {|<< x -/> >>|}

let expected: Formula.t = test_node (NonAllocated("x"))
;;

let%test_unit "test formulae n. 00 - 5" =
  [%test_eq: Formula.t] (parse_formula source) expected