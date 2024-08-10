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
  let formula = ZeroAnd.f formula in

  let expected_disjoints = disjoints in
  test_expected_disjoints formula expected_disjoints []

let%test "one false" =
  let disjoints = [
    Formula.And(
      Formula.Comparison(BinaryComparison.Equals,
        ArithmeticExpression.Variable("x"),
        ArithmeticExpression.Variable("w")
      ),
      Formula.False
    );
  ] in 
  let formula = make_normal_form [] disjoints in
  let formula = ZeroAnd.f formula in

  let expected_disjoints = Formula.False :: [] in
  test_expected_disjoints formula expected_disjoints []

let%test "recursion" =
  let disjoints = [
    Formula.AndSeparately(
      Formula.Comparison(BinaryComparison.Equals,
        ArithmeticExpression.Variable("x"),
        ArithmeticExpression.Variable("w")
      ),
      Formula.And(
        Formula.False,
        Formula.NonAllocated("x")
      )
    );
  ] in 
  let formula = make_normal_form [] disjoints in
  let formula = ZeroAnd.f formula in

  let expected_disjoints = 
    Formula.AndSeparately(
      Formula.Comparison(BinaryComparison.Equals,
        ArithmeticExpression.Variable("x"),
        ArithmeticExpression.Variable("w")
      ),
      Formula.False
    ) :: []
  in
  test_expected_disjoints formula expected_disjoints []

let%test "test 'p && false = false' simplification" =  
  let disjoints = [
    Formula.And(
      Formula.False,
      Formula.Allocation("x", ArithmeticExpression.Literal(1))
    );
  ] in 
  let formula = make_normal_form [] disjoints in
  let formula = ZeroAnd.f formula in

  let expected_disjoints = [ Formula.False ] in
  test_expected_disjoints formula expected_disjoints []