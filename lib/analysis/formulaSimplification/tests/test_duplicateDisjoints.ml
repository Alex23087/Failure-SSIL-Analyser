open DataStructures.Analysis
open Analysis_TestUtils
open NormalForm

let%test "duplicate disjoints identity" =
  let disjoints = [
    Formula.And(Formula.True, Formula.NonAllocated("x"));
    Formula.Allocation("y", ArithmeticExpression.Variable("z"))
  ] in 
  let formula = make_normal_form [] disjoints in
  let formula = DuplicateDisjoints.f formula in

  let expected_disjoints = disjoints in
  test_expected_disjoints formula expected_disjoints []

let%test "duplicate disjoints one copy" =
  let disjoints = [
    Formula.And(Formula.True, Formula.NonAllocated("x"));
    Formula.And(Formula.True, Formula.NonAllocated("x"));
    Formula.Allocation("y", ArithmeticExpression.Variable("z"))
  ] in 
  let formula = make_normal_form [] disjoints in
  let formula = DuplicateDisjoints.f formula in

  let expected_disjoints = [
    Formula.And(Formula.True, Formula.NonAllocated("x"));
    Formula.Allocation("y", ArithmeticExpression.Variable("z"))
  ] in 
  test_expected_disjoints formula expected_disjoints []

let%test "duplicate disjoints two copies" =
  let disjoints = [
    Formula.Comparison(
      BinaryComparison.Equals,
      ArithmeticExpression.Variable("w"),
      ArithmeticExpression.Variable("z")
    );
    Formula.Comparison(
      BinaryComparison.Equals,
      ArithmeticExpression.Variable("w"),
      ArithmeticExpression.Variable("z")
    );
    Formula.Comparison(
      BinaryComparison.Equals,
      ArithmeticExpression.Variable("w"),
      ArithmeticExpression.Variable("z")
    );
    Formula.Allocation("y", ArithmeticExpression.Variable("z"))
  ] in 
  let formula = make_normal_form [] disjoints in
  let formula = DuplicateDisjoints.f formula in

  let expected_disjoints = [
    Formula.Comparison(
      BinaryComparison.Equals,
      ArithmeticExpression.Variable("w"),
      ArithmeticExpression.Variable("z")
    );
    Formula.Allocation("y", ArithmeticExpression.Variable("z"))
  ] in 
  test_expected_disjoints formula expected_disjoints []

let%test "duplicate disjoints two doubles" =
  let disjoints = [
    Formula.And(Formula.True, Formula.NonAllocated("x"));
    Formula.And(Formula.True, Formula.NonAllocated("x"));
    Formula.Allocation("y", ArithmeticExpression.Variable("z"));
    Formula.Allocation("y", ArithmeticExpression.Variable("z"))
  ] in 
  let formula = make_normal_form [] disjoints in
  let formula = DuplicateDisjoints.f formula in

  let expected_disjoints = [
    Formula.And(Formula.True, Formula.NonAllocated("x"));
    Formula.Allocation("y", ArithmeticExpression.Variable("z"))
  ] in 
  test_expected_disjoints formula expected_disjoints []