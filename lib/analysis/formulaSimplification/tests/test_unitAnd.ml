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
  let formula = UnitAnd.f formula in

  let expected_disjoints = disjoints in
  test_expected_disjoints formula expected_disjoints []

let%test "one true" =
  let disjoints = [
    Formula.And(
      Formula.Comparison(BinaryComparison.Equals,
        ArithmeticExpression.Variable("x"),
        ArithmeticExpression.Variable("w")
      ),
      Formula.True
    );
  ] in 
  let formula = make_normal_form [] disjoints in
  let formula = UnitAnd.f formula in

  let expected_disjoints = 
    Formula.Comparison(BinaryComparison.Equals,
      ArithmeticExpression.Variable("x"),
      ArithmeticExpression.Variable("w")
    ) :: []
  in
  test_expected_disjoints formula expected_disjoints []

let%test "recursion" =
  let disjoints = [
    Formula.AndSeparately(
      Formula.Comparison(BinaryComparison.Equals,
        ArithmeticExpression.Variable("x"),
        ArithmeticExpression.Variable("w")
      ),
      Formula.And(
        Formula.True,
        Formula.NonAllocated("x")
      )
    );
  ] in 
  let formula = make_normal_form [] disjoints in
  let formula = UnitAnd.f formula in

  let expected_disjoints = 
    Formula.AndSeparately(
      Formula.Comparison(BinaryComparison.Equals,
        ArithmeticExpression.Variable("x"),
        ArithmeticExpression.Variable("w")
      ),
      Formula.NonAllocated("x")
    ) :: []
  in
  test_expected_disjoints formula expected_disjoints []