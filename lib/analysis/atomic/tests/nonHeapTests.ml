open AtomicBase
open Normalization
open DataStructures.Analysis.NormalForm
open Analysis_TestUtils

let%test "weakest precondition on skip" =
  let command = annot_cmd Commands.HeapAtomicCommand.Skip in
  let post_condition =
    annot (PFormula.Exists(
      "x",
      annot (PFormula.NonAllocated("x"))
    ))
  in
  let post_condition = existential_disjuntive_normal_form post_condition in
  let pre_condition = compute_precondition command post_condition in
  let expected_disjoints = Formula.NonAllocated("x") :: [] in
  test_expected_bound_variables pre_condition 1 &&
  test_expected_disjoints pre_condition expected_disjoints 

let%test "weakest precondition on assignment" =
  let command =
    annot_cmd (Commands.HeapAtomicCommand.Assignment(
      "x",
      annot_cmd (Commands.ArithmeticExpression.Literal(5))
    ))
  in
  let post_condition = 
    annot (PFormula.Or(
      annot (PFormula.Comparison(
        PBinaryComparison.Equals,
        annot (PArithmeticExpression.Variable("x")),
        annot (PArithmeticExpression.Variable("y"))
      )),
      annot (PFormula.NonAllocated("x"))
    ))
  in
  let post_condition = existential_disjuntive_normal_form post_condition in
  let pre_condition = compute_precondition command post_condition in
  let expected_disjoints =
    Formula.And(
      Formula.NonAllocated("0$x"),
      Formula.Comparison(
        BinaryComparison.Equals,
        ArithmeticExpression.Variable("0$x"),
        ArithmeticExpression.Literal(5)
      )
    ) ::
    Formula.Comparison(
      BinaryComparison.Equals,
      ArithmeticExpression.Literal(5),
      ArithmeticExpression.Variable("y")
    ) :: []
  in
  test_expected_bound_variables pre_condition 0 &&
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
    annot (PFormula.NonAllocated("y"))
  in
  let post_condition = existential_disjuntive_normal_form post_condition in
  let pre_condition = compute_precondition command post_condition in
  let expected_disjoints =
    Formula.And(
      Formula.Comparison(
        BinaryComparison.LessOrEqual,
        ArithmeticExpression.Variable("x"),
        ArithmeticExpression.Literal(17)
      ),
      Formula.NonAllocated("y")
    ) :: 
    Formula.And(
      Formula.False,
      Formula.NonAllocated("y")
    ) :: []
  in
  test_expected_disjoints pre_condition expected_disjoints 

let%test "weakest precondition on non deterministic assignment" =
  let command =
    annot_cmd (Commands.HeapAtomicCommand.NonDet("x"))
  in
  let post_condition =
    annot (PFormula.Comparison(
      PBinaryComparison.GreaterThan,
      annot (PArithmeticExpression.Variable("x")),
      annot (PArithmeticExpression.Variable("y"))
    ))
  in
  let post_condition = existential_disjuntive_normal_form post_condition in
  let pre_condition = compute_precondition command post_condition in
  let expected_disjoints =
    Formula.Comparison(
      BinaryComparison.GreaterThan,
      ArithmeticExpression.Variable("x"),
      ArithmeticExpression.Variable("y")
    ) :: []
  in
  test_expected_bound_variables pre_condition 1 &&
  test_expected_disjoints pre_condition expected_disjoints 