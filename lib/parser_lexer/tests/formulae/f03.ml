open Prelude.Ast.LogicFormulas
open Prelude.Ast.LogicFormulas.Formula
open Prelude.Ast.LogicFormulas.BinaryComparison
open Prelude.Ast.LogicFormulas.ArithmeticExpression
open Prelude.Ast.LogicFormulas.BinaryOperator

open F_utils
open Utils

let source = {|<< (exists a . v -> z) * ( (z = a) * ( true * ( (exists b . a -> b) * ( (x = a) || (x -/>) ) ) ) ) >>|}

let expected: Formula.t =
  test_node ( AndSeparately (
    test_node ( Exists ("a", test_node (Allocation ("v", test_node (Variable "z"))))),
    test_node ( AndSeparately (
      test_node (Comparison (Equals, test_node (Variable "z"), test_node (Variable "a"))),
      test_node (AndSeparately(
        test_node (True), 
        test_node (AndSeparately (
          test_node (Exists ("b", test_node (Allocation ( "a", test_node (Variable "b"))))),
          test_node (Or(
            test_node (Comparison( Equals, test_node (Variable "x"), test_node (Variable "a"))), 
            test_node (NonAllocated "x")
            ))
        ))
      ))
    ))
  ))
;;

let%test_unit "test formulae n. 03" =
  [%test_eq: Formula.t] (parse_formula source) expected