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
  let expected_disjoints = Formula.EmptyHeap :: [] in
  test_expected_bound_variables pre_condition 0 &&
  test_expected_disjoints pre_condition expected_disjoints []

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
      Formula.EmptyHeap,
      Formula.Comparison(
        BinaryComparison.Equals,
        ArithmeticExpression.Variable("a"),
        ArithmeticExpression.Literal(5)
      )
    ) ::
    Formula.Comparison(
      BinaryComparison.Equals,
      ArithmeticExpression.Variable("y"),
      ArithmeticExpression.Literal(5)
    ) :: []
  in
  test_expected_bound_variables pre_condition 1 &&
  test_expected_disjoints pre_condition expected_disjoints ["a"]
  
let%test "weakest precondition on assignment 2" =
  let command =
    annot_cmd (Commands.HeapAtomicCommand.Assignment(
      "x",
      annot_cmd (Commands.ArithmeticExpression.BinaryOperation(
        Commands.ArithmeticOperation.Plus,
        annot_cmd (Commands.ArithmeticExpression.Variable("w")),
        annot_cmd (Commands.ArithmeticExpression.Literal(5))
      ))
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
      Formula.NonAllocated("a"),
      Formula.Comparison(
        BinaryComparison.Equals,
        ArithmeticExpression.Variable("a"),
        ArithmeticExpression.Operation(
          BinaryOperator.Plus,
          ArithmeticExpression.Variable("w"),
          ArithmeticExpression.Literal(5)
        )
      )
    ) ::
    Formula.Comparison(
      BinaryComparison.Equals,
      ArithmeticExpression.Variable("w"),
      ArithmeticExpression.Operation(
        BinaryOperator.Plus,
        ArithmeticExpression.Variable("y"),
        ArithmeticExpression.Literal(-5)
      )
    ) :: []
  in
  test_expected_bound_variables pre_condition 1 &&
  test_expected_disjoints pre_condition expected_disjoints ["a"]

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
        BinaryComparison.LessThan,
        ArithmeticExpression.Variable("x"),
        ArithmeticExpression.Literal(18)
      ),
      Formula.NonAllocated("y")
    ) :: []
  in
  test_expected_bound_variables pre_condition 0 &&
  test_expected_disjoints pre_condition expected_disjoints []

(* << Exists x. x > y >> x = nondet() << x > y >> *)
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
  test_expected_disjoints pre_condition expected_disjoints ["x"]