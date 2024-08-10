open DataStructures.Analysis
open Analysis_TestUtils
open NormalForm

let%test "identity" =
  let disjoints = [
    Formula.And(
      Formula.Comparison(
        BinaryComparison.GreaterThan,
        ArithmeticExpression.Variable("x"),
        ArithmeticExpression.Variable("w")
      ),
      Formula.NonAllocated("x")
    );
  ] in 
  let formula = make_normal_form [] disjoints in
  let formula = EquationSimplification.f formula in

  let expected_disjoints = disjoints in
  test_expected_disjoints formula expected_disjoints []

let%test "literal simplification" =
  let disjoints = [
    Formula.Comparison(
      BinaryComparison.Equals,
      ArithmeticExpression.Literal(17),
      ArithmeticExpression.Operation(
        BinaryOperator.Plus,
        ArithmeticExpression.Literal(20),
        ArithmeticExpression.Literal(-3)
      )
    );
  ] in 
  let formula = make_normal_form [] disjoints in
  let formula = EquationSimplification.f formula in

  let expected_disjoints = [ Formula.True ] in
  test_expected_disjoints formula expected_disjoints []

let%test "simple equation simplification" =
  let disjoints = [
    Formula.Comparison(
      BinaryComparison.Equals,
      ArithmeticExpression.Operation(
        BinaryOperator.Times,
        ArithmeticExpression.Literal(21),
        ArithmeticExpression.Variable("x")
      ),
      ArithmeticExpression.Operation(
        BinaryOperator.Plus,
        ArithmeticExpression.Literal(20),
        ArithmeticExpression.Variable("x")
      )
    );
  ] in 
  let formula = make_normal_form [] disjoints in
  let formula = EquationSimplification.f formula in

  let expected_disjoints = [
    Formula.Comparison(
      BinaryComparison.Equals,
      ArithmeticExpression.Variable("x"),
      ArithmeticExpression.Literal(1)
    );
  ] in
  test_expected_disjoints formula expected_disjoints []

  
let%test "recursion" =
  let disjoints = [
    Formula.And(
      Formula.Comparison(
        BinaryComparison.NotEquals,
        ArithmeticExpression.Operation(
          BinaryOperator.Times,
          ArithmeticExpression.Literal(51),
          ArithmeticExpression.Variable("k")
        ),
        ArithmeticExpression.Operation(
          BinaryOperator.Plus,
          ArithmeticExpression.Literal(50),
          ArithmeticExpression.Variable("k")
        )
      ),
      Formula.Comparison(
        BinaryComparison.Equals,
        ArithmeticExpression.Variable("x"),
        ArithmeticExpression.Operation(
          BinaryOperator.Plus,
          ArithmeticExpression.Literal(50),
          ArithmeticExpression.Variable("k")
        )
      );
    );    
  ] in 
  let formula = make_normal_form [] disjoints in
  let formula = EquationSimplification.f formula in

  let expected_disjoints = [
    Formula.And(
      Formula.Comparison(
        BinaryComparison.NotEquals,
        ArithmeticExpression.Variable("k"),
        ArithmeticExpression.Literal(1)
      ),
      Formula.Comparison(
        BinaryComparison.Equals,
        ArithmeticExpression.Variable("k"),
        ArithmeticExpression.Operation(
          BinaryOperator.Plus,
          ArithmeticExpression.Variable("x"),
          ArithmeticExpression.Literal(-50)
        )
      )
    );
  ] in
  test_expected_disjoints formula expected_disjoints []