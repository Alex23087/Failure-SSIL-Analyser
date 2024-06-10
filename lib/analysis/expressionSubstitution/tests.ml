open ExpressionSubstitutionBase
open Normalization
open DataStructures.Analysis.NormalForm

(* Alias for better readability *)
module PFormula = DataStructures.Parser.LogicFormulas.Formula
module PBinaryComparison = DataStructures.Parser.LogicFormulas.BinaryComparison
module PArithmeticExpression = DataStructures.Parser.LogicFormulas.ArithmeticExpression
module PBinaryOperator = DataStructures.Parser.LogicFormulas.BinaryOperator

open Analysis_TestCommon

let%test "substitute identifer only expression" =
  let formula = annot (PFormula.Or(
    annot (PFormula.NonAllocated("x")),
    annot (PFormula.Comparison(
        PBinaryComparison.Equals,
        annot (PArithmeticExpression.Operation(
          PBinaryOperator.Plus,
          annot (PArithmeticExpression.Literal(5)),
          annot (PArithmeticExpression.Variable("x"))
        )),
        annot (PArithmeticExpression.Variable("y"))
    ))
  )) in
  let normalized = existential_disjuntive_normal_form formula in
  let substituted_id = "x" in
  let substituting_expression = ArithmeticExpression.Variable("z") in
  let normalized = substitute_expression_in_normalized_formula normalized substituting_expression substituted_id in
  let expected_disjoints =
    Formula.NonAllocated("z") ::
    Formula.Comparison(
        BinaryComparison.Equals,
        ArithmeticExpression.Operation(
          BinaryOperator.Plus,
          ArithmeticExpression.Literal(5),
          ArithmeticExpression.Variable("z")
        ),
        ArithmeticExpression.Variable("y")
    ) :: []
  in
  test_expected_disjoints normalized expected_disjoints

let%test "substitute non identifier only expression" = 
  let formula = annot (PFormula.Or(
    annot (PFormula.NonAllocated("x")),
    annot (PFormula.Comparison(
        PBinaryComparison.Equals,
        annot (PArithmeticExpression.Operation(
          PBinaryOperator.Plus,
          annot (PArithmeticExpression.Literal(5)),
          annot (PArithmeticExpression.Variable("x"))
        )),
        annot (PArithmeticExpression.Variable("y"))
    ))
  )) in
  let normalized = existential_disjuntive_normal_form formula in
  let substituted_id = "x" in
  let substituting_expression =
    ArithmeticExpression.Operation(
      BinaryOperator.Minus,
      ArithmeticExpression.Literal(17),
      ArithmeticExpression.Variable("z")
    )
  in
  let renamed_equal_formula =
    Formula.Comparison(
      BinaryComparison.Equals,
      ArithmeticExpression.Variable("0$x"),
      substituting_expression
    )
  in
  let normalized = substitute_expression_in_normalized_formula normalized substituting_expression substituted_id in
  let expected_disjoints =
    Formula.And(
      Formula.NonAllocated("0$x"),
      renamed_equal_formula
    ) ::
    Formula.Comparison(
      BinaryComparison.Equals,
      ArithmeticExpression.Operation(
        BinaryOperator.Plus,
        ArithmeticExpression.Literal(5),
        substituting_expression
      ),
      ArithmeticExpression.Variable("y")
    ) :: []
  in
  test_expected_disjoints normalized expected_disjoints