open DataStructures.Analysis
open Analysis_TestUtils
open NormalForm

let%test "identity" =
  let disjoints = [
    Formula.AndSeparately(
      Formula.Comparison(BinaryComparison.Equals,
        ArithmeticExpression.Variable("x"),
        ArithmeticExpression.Variable("w")
      ),
      Formula.NonAllocated("x")
    );
  ] in
  let formula = make_normal_form [] disjoints in
  let formula = UnitAndSep.f formula in

  let expected_disjoints = disjoints in
  test_expected_disjoints formula expected_disjoints []

let%test "one emp" =
  let disjoints = [
    Formula.AndSeparately(
      Formula.Comparison(BinaryComparison.Equals,
        ArithmeticExpression.Variable("x"),
        ArithmeticExpression.Variable("w")
      ),
      Formula.EmptyHeap
    );
  ] in
  let formula = make_normal_form [] disjoints in
  let formula = UnitAndSep.f formula in

  let expected_disjoints =
    Formula.Comparison(BinaryComparison.Equals,
      ArithmeticExpression.Variable("x"),
      ArithmeticExpression.Variable("w")
    ) :: []
  in
  test_expected_disjoints formula expected_disjoints []

let%test "recursion" =
  let disjoints = [
    Formula.And(
      Formula.Comparison(BinaryComparison.Equals,
        ArithmeticExpression.Variable("x"),
        ArithmeticExpression.Variable("w")
      ),
      Formula.AndSeparately(
        Formula.EmptyHeap,
        Formula.NonAllocated("x")
      )
    );
  ] in 
  let formula = make_normal_form [] disjoints in
  let formula = UnitAndSep.f formula in

  let expected_disjoints = 
    Formula.And(
      Formula.Comparison(BinaryComparison.Equals,
        ArithmeticExpression.Variable("x"),
        ArithmeticExpression.Variable("w")
      ),
      Formula.NonAllocated("x")
    ) :: []
  in
  test_expected_disjoints formula expected_disjoints []

let%test "test 'p * emp = p' simplification" =
  let p = Formula.Allocation("x", ArithmeticExpression.Literal(1)) in
  let disjoints = [ Formula.AndSeparately(p, Formula.EmptyHeap); ] in
  let formula = make_normal_form [] disjoints in
  let formula = UnitAndSep.f formula in

  let expected_disjoints = [ p ] in
  test_expected_disjoints formula expected_disjoints []