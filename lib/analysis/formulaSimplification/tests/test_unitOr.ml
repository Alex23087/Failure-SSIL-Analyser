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
  let formula = UnitOr.f formula in

  let expected_disjoints = disjoints in
  test_expected_disjoints formula expected_disjoints []

let%test "one false" =
  let disjoints = [
    Formula.Comparison(BinaryComparison.Equals,
      ArithmeticExpression.Variable("x"),
      ArithmeticExpression.Variable("w")
    );
    Formula.False;
  ] in 
  let formula = make_normal_form [] disjoints in
  let formula = UnitOr.f formula in

  let expected_disjoints = 
    Formula.Comparison(BinaryComparison.Equals,
      ArithmeticExpression.Variable("x"),
      ArithmeticExpression.Variable("w")
    ) :: []
  in
  test_expected_disjoints formula expected_disjoints []

let%test "many false" =
  let disjoints = [
    Formula.False;
    Formula.Comparison(BinaryComparison.Equals,
      ArithmeticExpression.Variable("x"),
      ArithmeticExpression.Variable("w")
    );
    Formula.False;
    Formula.False;
  ] in 
  let formula = make_normal_form [] disjoints in
  let formula = UnitOr.f formula in

  let expected_disjoints = 
    Formula.Comparison(BinaryComparison.Equals,
      ArithmeticExpression.Variable("x"),
      ArithmeticExpression.Variable("w")
    ) :: []
  in
  test_expected_disjoints formula expected_disjoints []

let%test "test 'p || false = p' simplification" =
  let p = Formula.Allocation("x", ArithmeticExpression.Literal(1)) in
  let disjoints = [p; Formula.False] in
  let formula = make_normal_form [] disjoints in
  let formula = UnitOr.f formula in

  let expected_disjoints = [ p ] in
  test_expected_disjoints formula expected_disjoints []