open AtomicBase
open Normalization
open DataStructures.Parser.LogicFormulas
module Commands = Ast.HeapRegularCommands

open Analysis_TestCommon

let%test "weakest precondition on skip" =
  let command = annot_cmd Commands.HeapAtomicCommand.Skip in
  let post_condition =
    annot (Formula.Exists(
      "x",
      annot (Formula.NonAllocated("x"))
    ))
  in
  let post_condition = existential_disjuntive_normal_form post_condition 0 in
  let pre_condition = weakest_precondition command post_condition in
  let expected_variables = "x" :: [] in
  let expected_disjoints = annot_unit (Formula.NonAllocated("x")) :: [] in
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
  let pre_condition = weakest_precondition command post_condition in
  let expected_disjoints =
    annot_unit (Formula.And(
      annot_unit (Formula.NonAllocated("0$x")),
      annot_unit (Formula.Comparison(
        BinaryComparison.Equals,
        annot_unit (ArithmeticExpression.Variable("0$x")),
        annot_unit (ArithmeticExpression.Literal(5))
      ))
    )) ::
    annot_unit (Formula.Comparison(
      BinaryComparison.Equals,
      annot_unit (ArithmeticExpression.Literal(5)),
      annot_unit (ArithmeticExpression.Variable("y"))
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
  let pre_condition = weakest_precondition command post_condition in
  let expected_disjoints =
    annot_unit (Formula.And(
      annot_unit (Formula.Comparison(
        BinaryComparison.LessOrEqual,
        annot_unit (ArithmeticExpression.Variable("x")),
        annot_unit (ArithmeticExpression.Literal(17))
      )),
      annot_unit (Formula.NonAllocated("y"))
    )) :: 
    annot_unit (Formula.And(
      annot_unit (Formula.False),
      annot_unit (Formula.NonAllocated("y"))
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
  let pre_condition = weakest_precondition command post_condition in
  let expected_variables = "x" :: [] in
  let expected_disjoints =
    annot_unit (Formula.Comparison(
      BinaryComparison.GreaterThan,
      annot_unit (ArithmeticExpression.Variable("x")),
      annot_unit (ArithmeticExpression.Variable("y"))
    )) :: []
  in
  test_expected_free_variables pre_condition expected_variables &&
  test_expected_disjoints pre_condition expected_disjoints 