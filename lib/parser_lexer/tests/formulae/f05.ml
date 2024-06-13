open Prelude.Ast.LogicFormulas
open Prelude.Ast.LogicFormulas.Formula
open Prelude.Ast.LogicFormulas.BinaryComparison
open Prelude.Ast.LogicFormulas.ArithmeticExpression
open Prelude.Ast.LogicFormulas.BinaryOperator

open F_utils
open Utils



let source = {|<< x -> 1 + 2 >>|}

let expected: Formula.t =
  test_node (Allocation(
    "x", 
    test_node (Operation(Plus, test_node (Literal 1), test_node (Literal 2)))))
;;

let%test_unit "test formulae n. 05 - 1" =
  [%test_eq: Formula.t] (parse_formula source) expected


let source = {|<< x -> 1 - x >>|}

let expected: Formula.t =
  test_node (Allocation(
    "x", 
    test_node (Operation(Minus, test_node (Literal 1), test_node (Variable "x")))))
;;

let%test_unit "test formulae n. 05 - 2" =
  [%test_eq: Formula.t] (parse_formula source) expected


let source = {|<< x -> x * 2 >>|}

let expected: Formula.t =
  test_node (Allocation(
    "x", 
    test_node (Operation(Times, test_node (Variable "x"), test_node (Literal 2)))))
;;

let%test_unit "test formulae n. 05 - 3" =
  [%test_eq: Formula.t] (parse_formula source) expected


let source = {|<< x -> 1 / 2 >>|}

let expected: Formula.t =
  test_node (Allocation(
    "x", 
    test_node (Operation(Division, test_node (Literal 1), test_node (Literal 2)))))
;;

let%test_unit "test formulae n. 05 - 4" =
  [%test_eq: Formula.t] (parse_formula source) expected


let source = {|<< x -> x % y >>|}

let expected: Formula.t =
  test_node (Allocation(
    "x", 
    test_node (Operation(Modulo, test_node (Variable "x"), test_node (Variable "y")))))
;;

let%test_unit "test formulae n. 05 - 5" =
  [%test_eq: Formula.t] (parse_formula source) expected


let source = {|<< x -> -1 >>|}

let expected: Formula.t =
  test_node (Allocation(
    "x",
    test_node (Operation (Minus, test_node (Literal 0), test_node (Literal 1)))
  ))
;;

let%test_unit "test formulae n. 05 - 6" =
  [%test_eq: Formula.t] (parse_formula source) expected