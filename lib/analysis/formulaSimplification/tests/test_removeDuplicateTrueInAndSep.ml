open DataStructures.Analysis
open Analysis_TestUtils
open NormalForm

let%test "identity" =
  let disjoints = [
    Formula.AndSeparately(
      Formula.Allocation("x", ArithmeticExpression.Variable("w")),
      Formula.Allocation("z", ArithmeticExpression.Variable("z"))
    );
  ] in 
  let formula = make_normal_form [] disjoints in
  let formula = RemoveDuplicateTrueInAndSep.f formula in

  let expected_disjoints = disjoints in
  test_expected_disjoints formula expected_disjoints []

let%test "identity with true" =
  let disjoints = [
    Formula.AndSeparately(
      Formula.Allocation("x", ArithmeticExpression.Variable("w")),
      Formula.True
    );
  ] in 
  let formula = make_normal_form [] disjoints in
  let formula = RemoveDuplicateTrueInAndSep.f formula in

  let expected_disjoints = disjoints in
  test_expected_disjoints formula expected_disjoints []

let%test "duplicate true" =
  let disjoints = [
    Formula.AndSeparately(
      Formula.AndSeparately(
        Formula.Allocation("x", ArithmeticExpression.Variable("w")),
        Formula.True
      ),
      Formula.True
    );
  ] in 
  let formula = make_normal_form [] disjoints in
  let formula = RemoveDuplicateTrueInAndSep.f formula in

  let expected_disjoints = [
    Formula.AndSeparately(
      Formula.Allocation("x", ArithmeticExpression.Variable("w")),
      Formula.True
    )
  ] in
  test_expected_disjoints formula expected_disjoints []

let%test "triple true" =
  let disjoints = [
    Formula.AndSeparately(
      Formula.AndSeparately(
        Formula.Allocation("x", ArithmeticExpression.Variable("w")),
        Formula.True
      ),
      Formula.AndSeparately(
        Formula.True,
        Formula.AndSeparately(
          Formula.True,
          Formula.NonAllocated("w")
        )
      )
    );
  ] in 
  let formula = make_normal_form [] disjoints in
  let formula = RemoveDuplicateTrueInAndSep.f formula in

  let expected_disjoints = [
    Formula.AndSeparately(
      Formula.Allocation("x", ArithmeticExpression.Variable("w")),
      Formula.AndSeparately(
        Formula.True,
        Formula.NonAllocated("w")
      )
    )
  ] in
  test_expected_disjoints formula expected_disjoints []

let%test "nest true" =
  let disjoints = [
    Formula.And(
      Formula.AndSeparately(
        Formula.AndSeparately(
          Formula.Allocation("x", ArithmeticExpression.Variable("w")),
          Formula.True
        ),
        Formula.True
      ),
      Formula.True
    );
  ] in 
  let formula = make_normal_form [] disjoints in
  let formula = RemoveDuplicateTrueInAndSep.f formula in

  let expected_disjoints = [
    Formula.And(
      Formula.AndSeparately(
        Formula.Allocation("x", ArithmeticExpression.Variable("w")),
        Formula.True
      ),
      Formula.True
    )
  ] in
  test_expected_disjoints formula expected_disjoints []