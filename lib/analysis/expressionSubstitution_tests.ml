open ExpressionSubstitution
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
  let substituting_expression = annot (ArithmeticExpression.Variable("z")) in
  let normalized = substitute_expression_in_normalized_formula normalized substituting_expression substituted_id in
  let expected_disjoints =
    annot(Formula.NonAllocated("z")) ::
    annot (Formula.Comparison(
        BinaryComparison.Equals,
        annot (ArithmeticExpression.Operation(
          BinaryOperator.Plus,
          annot (ArithmeticExpression.Literal(5)),
          annot (ArithmeticExpression.Variable("z"))
        )),
        annot (ArithmeticExpression.Variable("y"))
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
    annot (ArithmeticExpression.Operation(
      BinaryOperator.Minus,
      annot (ArithmeticExpression.Literal(17)),
      annot (ArithmeticExpression.Variable("z"))
    ))
  in
  let renamed_equal_formula =
    annot (Formula.Comparison(
      BinaryComparison.Equals,
      annot (ArithmeticExpression.Variable("0$x")),
      substituting_expression
    ))
  in
  let normalized = substitute_expression_in_normalized_formula normalized substituting_expression substituted_id in
  let expected_disjoints =
    annot (Formula.And(
      annot (Formula.NonAllocated("0$x")),
      renamed_equal_formula
    )) ::
    annot (Formula.Comparison(
      BinaryComparison.Equals,
      annot (ArithmeticExpression.Operation(
        BinaryOperator.Plus,
        annot (ArithmeticExpression.Literal(5)),
        substituting_expression
      )),
      annot (ArithmeticExpression.Variable("y"))
    )) :: []
  in
  test_expected_disjoints normalized expected_disjoints