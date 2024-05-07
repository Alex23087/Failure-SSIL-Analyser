open Atomic
open Normalization
open Analysis_Utils
open DataStructures.Parser.LogicFormulas
module Commands = Ast.HeapRegularCommands

open Analysis_TestCommon

let annotation_conversion = command_annotation_to_logic_annotation

let%test "weakest precondition on skip" =
  let command = annot_cmd Commands.HeapAtomicCommand.Skip in
  let post_condition =
    annot (Formula.Exists(
      "x",
      annot (Formula.NonAllocated("x"))
    ))
  in
  let post_condition = existential_disjuntive_normal_form post_condition 0 in
  let pre_condition = weakest_precondition command post_condition annotation_conversion in
  let expected_variables = "x" :: [] in
  let expected_disjoints = annot (Formula.NonAllocated("x")) :: [] in
  test_expected_free_variables pre_condition expected_variables &&
  test_expected_disjoints pre_condition expected_disjoints 

let%test "weakest precondition on assignment" =
  let command =
    annot_cmd (Commands.HeapAtomicCommand.Assignment(
      "x",
      annot_cmd (Commands.ArithmeticExpression.Literal(5))
    ))
  in
  let post_condition = 
    annot (Formula.Or(
      annot (Formula.Comparison(
        BinaryComparison.Equals,
        annot (ArithmeticExpression.Variable("x")),
        annot (ArithmeticExpression.Variable("y"))
      )),
      annot (Formula.NonAllocated("x"))
    ))
  in
  let post_condition = existential_disjuntive_normal_form post_condition 0 in
  let pre_condition = weakest_precondition command post_condition annotation_conversion in
  let expected_disjoints =
    annot (Formula.And(
      annot (Formula.NonAllocated("0$x")),
      annot (Formula.Comparison(
        BinaryComparison.Equals,
        annot (ArithmeticExpression.Variable("0$x")),
        annot (ArithmeticExpression.Literal(5))
      ))
    )) ::
    annot (Formula.Comparison(
      BinaryComparison.Equals,
      annot (ArithmeticExpression.Literal(5)),
      annot (ArithmeticExpression.Variable("y"))
    )) :: []
  in
  test_expected_free_variables pre_condition [] &&
  test_expected_disjoints pre_condition expected_disjoints 

let%test "weakest precondition on guard" =
  let command =
    annot_cmd (Commands.HeapAtomicCommand.Guard(
      annot_cmd (Commands.BooleanExpression.Not(
        annot_cmd (Commands.BooleanExpression.And(
          annot_cmd (Commands.BooleanExpression.Comparison(
            Commands.BooleanComparison.GreaterThan,
            annot_cmd (Commands.ArithmeticExpression.Variable("x")),
            annot_cmd (Commands.ArithmeticExpression.Literal(17))
          )),
          annot_cmd (Commands.BooleanExpression.True)
        ))
      ))
    ))
  in
  let post_condition =
    annot (Formula.NonAllocated("y"))
  in
  let post_condition = existential_disjuntive_normal_form post_condition 0 in
  let pre_condition = weakest_precondition command post_condition annotation_conversion in
  let expected_disjoints =
    annot (Formula.And(
      annot (Formula.Comparison(
        BinaryComparison.LessOrEqual,
        annot (ArithmeticExpression.Variable("x")),
        annot (ArithmeticExpression.Literal(17))
      )),
      annot (Formula.NonAllocated("y"))
    )) :: 
    annot (Formula.And(
      annot (Formula.False),
      annot (Formula.NonAllocated("y"))
    )) :: []
  in
  test_expected_disjoints pre_condition expected_disjoints 

let%test "weakest precondition on non deterministic assignment" =
  let command =
    annot_cmd (Commands.HeapAtomicCommand.NonDet("x"))
  in
  let post_condition =
    annot (Formula.Comparison(
      BinaryComparison.GreaterThan,
      annot (ArithmeticExpression.Variable("x")),
      annot (ArithmeticExpression.Variable("y"))
    ))
  in
  let post_condition = existential_disjuntive_normal_form post_condition 0 in
  let pre_condition = weakest_precondition command post_condition annotation_conversion in
  let expected_variables = "x" :: [] in
  let expected_disjoints =
    annot (Formula.Comparison(
      BinaryComparison.GreaterThan,
      annot (ArithmeticExpression.Variable("x")),
      annot (ArithmeticExpression.Variable("y"))
    )) :: []
  in
  test_expected_free_variables pre_condition expected_variables &&
  test_expected_disjoints pre_condition expected_disjoints 