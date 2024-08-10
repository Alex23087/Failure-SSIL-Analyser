open DataStructures.Analysis
open Analysis_TestUtils
open NormalForm

let%test "x -> w * x -> z = false" =
  let disjoints = [
    Formula.AndSeparately(
      Formula.Allocation("x", ArithmeticExpression.Variable("w")),
      Formula.Allocation("x", ArithmeticExpression.Variable("z"))
    );
  ] in 
  let formula = make_normal_form [] disjoints in
  let formula = NoSharedIdentifierAndSep.f formula in

  let expected_disjoints = [ Formula.False ] in
  test_expected_disjoints formula expected_disjoints []
  
let%test "x -> w * y -> z remains as is" =
  let disjoints = [
    Formula.AndSeparately(
      Formula.Allocation("x", ArithmeticExpression.Variable("w")),
      Formula.Allocation("y", ArithmeticExpression.Variable("z"))
    );
  ] in 
  let formula = make_normal_form [] disjoints in
  let formula = NoSharedIdentifierAndSep.f formula in

  let expected_disjoints = disjoints in
  test_expected_disjoints formula expected_disjoints []

  
let%test "x -> w * (y -> z * x -> w) nested error" =
  let disjoints = [
    Formula.AndSeparately(
      Formula.Allocation("x", ArithmeticExpression.Variable("w")),
      Formula.AndSeparately(
        Formula.Allocation("y", ArithmeticExpression.Variable("z")),
        Formula.Allocation("x", ArithmeticExpression.Variable("w"))
      )
    );
  ] in 
  let formula = make_normal_form [] disjoints in
  let formula = NoSharedIdentifierAndSep.f formula in

  let expected_disjoints = [ Formula.False ] in
  test_expected_disjoints formula expected_disjoints []
  
let%test "(x -> v && z -> w) * x -> t must be false" =
  let disjoints = [
    Formula.AndSeparately(
      Formula.And(
        Formula.Allocation("x", ArithmeticExpression.Variable("v")),
        Formula.Allocation("z", ArithmeticExpression.Variable("w"))
      ),
      Formula.Allocation("x", ArithmeticExpression.Variable("t"))
    )
  ] in
  let formula = make_normal_form [] disjoints in
  let formula = NoSharedIdentifierAndSep.f formula in

  let expected_disjoints = [ Formula.False ] in
  test_expected_disjoints formula expected_disjoints []