open DataStructures.Analysis
open Analysis_TestUtils
open NormalForm

let%test "bound variables cleanup identity" =
  let disjoints = [
    Formula.And(Formula.True, Formula.NonAllocated("x"));
    Formula.Allocation("y", ArithmeticExpression.Variable("z"))
  ] in 
  let formula = make_normal_form ["x"] disjoints in
  let formula = BoundVariableCleanup.f formula in

  test_expected_bound_variables formula 1

let%test "bound variables cleanup remove one var" =
  let disjoints = [
    Formula.And(Formula.True, Formula.NonAllocated("x"));
    Formula.Allocation("y", ArithmeticExpression.Variable("z"))
  ] in 
  let formula = make_normal_form ["x"; "z"; "w"] disjoints in
  let formula = BoundVariableCleanup.f formula in

  test_expected_bound_variables formula 2