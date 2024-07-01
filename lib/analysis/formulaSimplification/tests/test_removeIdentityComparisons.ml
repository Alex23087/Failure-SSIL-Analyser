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
      Formula.True
    );
  ] in 
  let formula = make_normal_form [] disjoints in
  let formula = RemoveIdentityComparisons.f formula in

  let expected_disjoints = disjoints in
  test_expected_disjoints formula expected_disjoints []

  let%test "equal sub-expressions 0" =
    let disjoints = [
      Formula.AndSeparately(
        Formula.Comparison(BinaryComparison.Equals,
          ArithmeticExpression.Variable("x"),
          ArithmeticExpression.Variable("x")
        ),
        Formula.True
      );
    ] in 
    let formula = make_normal_form [] disjoints in
    let formula = RemoveIdentityComparisons.f formula in
  
    let expected_disjoints =
      Formula.AndSeparately(
        Formula.True,
        Formula.True
      ) :: []
    in
    test_expected_disjoints formula expected_disjoints []

let%test "equal sub-expressions 1" =
  let disjoints = [
    Formula.AndSeparately(
      Formula.Comparison(BinaryComparison.Equals,
        ArithmeticExpression.Operation(BinaryOperator.Plus,
          ArithmeticExpression.Variable("x"),
          ArithmeticExpression.Literal(17)
        ),
        ArithmeticExpression.Operation(BinaryOperator.Plus,
          ArithmeticExpression.Variable("x"),
          ArithmeticExpression.Literal(17)
        )
      ),
      Formula.True
    );
  ] in 
  let formula = make_normal_form [] disjoints in
  let formula = RemoveIdentityComparisons.f formula in

  let expected_disjoints =
    Formula.AndSeparately(
      Formula.True,
      Formula.True
    ) :: []
  in
  test_expected_disjoints formula expected_disjoints []
  
  let%test "not equal sub-expressions 0" =
    let disjoints = [
      Formula.AndSeparately(
        Formula.Comparison(BinaryComparison.NotEquals,
          ArithmeticExpression.Variable("z"),
          ArithmeticExpression.Variable("z")
        ),
        Formula.True
      );
    ] in 
    let formula = make_normal_form [] disjoints in
    let formula = RemoveIdentityComparisons.f formula in
  
    let expected_disjoints =
      Formula.AndSeparately(
        Formula.True,
        Formula.False
      ) :: []
    in
    test_expected_disjoints formula expected_disjoints []

let%test "not equal sub-expressions 1" =
  let disjoints = [
    Formula.AndSeparately(
      Formula.Comparison(BinaryComparison.NotEquals,
        ArithmeticExpression.Operation(BinaryOperator.Division,
          ArithmeticExpression.Variable("k"),
          ArithmeticExpression.Literal(4475)
        ),
        ArithmeticExpression.Operation(BinaryOperator.Division,
          ArithmeticExpression.Variable("k"),
          ArithmeticExpression.Literal(4475)
        )
      ),
      Formula.True
    );
  ] in 
  let formula = make_normal_form [] disjoints in
  let formula = RemoveIdentityComparisons.f formula in

  let expected_disjoints =
    Formula.AndSeparately(
      Formula.True,
      Formula.False
    ) :: []
  in
  test_expected_disjoints formula expected_disjoints []