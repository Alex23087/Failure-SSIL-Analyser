open DataStructures.Analysis
open Analysis_TestUtils
open NormalForm

let%test "identity" =
  let disjoints = [
    Formula.And(
      Formula.Comparison(BinaryComparison.Equals,
        ArithmeticExpression.Variable("x"),
        ArithmeticExpression.Variable("w")
      ),
      Formula.NonAllocated("x")
    );
  ] in 
  let formula = make_normal_form [] disjoints in
  let formula = DisequationSimplification.f formula in

  let expected_disjoints = disjoints in
  test_expected_disjoints formula expected_disjoints []

let%test "literal disequations" = 
  let disjoints = [
    Formula.Comparison(
      BinaryComparison.LessThan,
      ArithmeticExpression.Literal(17),
      ArithmeticExpression.Literal(2)
    );
    Formula.Comparison(
      BinaryComparison.LessOrEqual,
      ArithmeticExpression.Operation(
        BinaryOperator.Plus,
        ArithmeticExpression.Literal(2),
        ArithmeticExpression.Literal(2)
      ),
      ArithmeticExpression.Literal(4)
    );
    Formula.Comparison(
      BinaryComparison.GreaterThan,
      ArithmeticExpression.Literal(17),
      ArithmeticExpression.Literal(20)
    );
    Formula.Comparison(
      BinaryComparison.GreaterOrEqual,
      ArithmeticExpression.Literal(17),
      ArithmeticExpression.Literal(17)
    );
  ] in
  let formula = make_normal_form [] disjoints in
  let formula = DisequationSimplification.f formula in

  let expected_disjoints = [
    Formula.False;
    Formula.True;
    Formula.False;
    Formula.True
  ] in
  test_expected_disjoints formula expected_disjoints []

let%test "simple disequation simplification 0" = 
  let disjoints = [
    Formula.Comparison(
      BinaryComparison.GreaterThan,
      ArithmeticExpression.Variable("x"),
      ArithmeticExpression.Literal(17)
    );
  ] in 
  let formula = make_normal_form [] disjoints in
  let formula = DisequationSimplification.f formula in

  let expected_disjoints = [
    Formula.Comparison(
      BinaryComparison.GreaterThan,
      ArithmeticExpression.Variable("x"),
      ArithmeticExpression.Literal(17)
    );
  ] in
  test_expected_disjoints formula expected_disjoints []

let%test "simple disequation simplification 1" = 
  let disjoints = [
    Formula.Comparison(
      BinaryComparison.GreaterThan,
      ArithmeticExpression.Literal(17),
      ArithmeticExpression.Variable("x")
    );
  ] in 
  let formula = make_normal_form [] disjoints in
  let formula = DisequationSimplification.f formula in

  let expected_disjoints = [
    Formula.Comparison(
      BinaryComparison.LessThan,
      ArithmeticExpression.Variable("x"),
      ArithmeticExpression.Literal(17)
    );
  ] in
  test_expected_disjoints formula expected_disjoints []

let%test "simple disequation simplification 2" = 
  let disjoints = [
    Formula.Comparison(
      BinaryComparison.GreaterThan,
      ArithmeticExpression.Literal(17),
      ArithmeticExpression.Operation(
        BinaryOperator.Times,
        ArithmeticExpression.Literal(-1),
        ArithmeticExpression.Variable("x")
      )
    );
  ] in 
  let formula = make_normal_form [] disjoints in
  let formula = DisequationSimplification.f formula in

  let expected_disjoints = [
    Formula.Comparison(
      BinaryComparison.GreaterThan,
      ArithmeticExpression.Variable("x"),
      ArithmeticExpression.Literal(-17)
    );
  ] in
  test_expected_disjoints formula expected_disjoints []

let%test "simple disequation simplification 3" = 
  let disjoints = [
    Formula.Comparison(BinaryComparison.LessThan,
      ArithmeticExpression.Operation(
        BinaryOperator.Plus,
        ArithmeticExpression.Operation(
          BinaryOperator.Times,
          ArithmeticExpression.Literal(-1),
          ArithmeticExpression.Variable("x")
        ),
        ArithmeticExpression.Literal(4)
      ),
      ArithmeticExpression.Literal(17)
    );
  ] in 
  let formula = make_normal_form [] disjoints in
  let formula = DisequationSimplification.f formula in

  let expected_disjoints = [
    Formula.Comparison(BinaryComparison.GreaterThan,
      ArithmeticExpression.Variable("x"),
      ArithmeticExpression.Literal(-13)
    );
  ] in
  test_expected_disjoints formula expected_disjoints []