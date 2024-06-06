open ExpressionSubstitutionBase
open Normalization
open DataStructures.Parser.LogicFormulas

open Analysis_TestCommon

let%test "substitute identifer only expression" =
  let formula = annot (Formula.Or(
    annot(Formula.NonAllocated("x")),
    annot (Formula.Comparison(
        BinaryComparison.Equals,
        annot (ArithmeticExpression.Operation(
          BinaryOperator.Plus,
          annot (ArithmeticExpression.Literal(5)),
          annot (ArithmeticExpression.Variable("x"))
        )),
        annot (ArithmeticExpression.Variable("y"))
    ))
  )) in
  let normalized = existential_disjuntive_normal_form formula 0 in
  let substituted_id = "x" in
  let substituting_expression = annot_unit (ArithmeticExpression.Variable("z")) in
  let normalized = substitute_expression_in_normalized_formula normalized substituting_expression substituted_id in
  let expected_disjoints =
    annot_unit (Formula.NonAllocated("z")) ::
    annot_unit (Formula.Comparison(
        BinaryComparison.Equals,
        annot_unit (ArithmeticExpression.Operation(
          BinaryOperator.Plus,
          annot_unit (ArithmeticExpression.Literal(5)),
          annot_unit (ArithmeticExpression.Variable("z"))
        )),
        annot_unit (ArithmeticExpression.Variable("y"))
    )) :: []
  in
  test_expected_disjoints normalized expected_disjoints

let%test "substitute non identifier only expression" = 
  let formula = annot (Formula.Or(
    annot (Formula.NonAllocated("x")),
    annot (Formula.Comparison(
        BinaryComparison.Equals,
        annot (ArithmeticExpression.Operation(
          BinaryOperator.Plus,
          annot (ArithmeticExpression.Literal(5)),
          annot (ArithmeticExpression.Variable("x"))
        )),
        annot (ArithmeticExpression.Variable("y"))
    ))
  )) in
  let normalized = existential_disjuntive_normal_form formula 0 in
  let substituted_id = "x" in
  let substituting_expression =
    annot_unit (ArithmeticExpression.Operation(
      BinaryOperator.Minus,
      annot_unit (ArithmeticExpression.Literal(17)),
      annot_unit (ArithmeticExpression.Variable("z"))
    ))
  in
  let renamed_equal_formula =
    annot_unit (Formula.Comparison(
      BinaryComparison.Equals,
      annot_unit (ArithmeticExpression.Variable("0$x")),
      substituting_expression
    ))
  in
  let normalized = substitute_expression_in_normalized_formula normalized substituting_expression substituted_id in
  let expected_disjoints =
    annot_unit (Formula.And(
      annot_unit (Formula.NonAllocated("0$x")),
      renamed_equal_formula
    )) ::
    annot_unit (Formula.Comparison(
      BinaryComparison.Equals,
      annot_unit (ArithmeticExpression.Operation(
        BinaryOperator.Plus,
        annot_unit (ArithmeticExpression.Literal(5)),
        substituting_expression
      )),
      annot_unit (ArithmeticExpression.Variable("y"))
    )) :: []
  in
  test_expected_disjoints normalized expected_disjoints