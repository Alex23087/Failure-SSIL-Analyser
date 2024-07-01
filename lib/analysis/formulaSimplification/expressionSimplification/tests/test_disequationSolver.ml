open DataStructures.Analysis
open Analysis_TestUtils
open NormalForm

let%test "identity" =
  let disjoints = [
    Formula.And(
      Formula.Comparison(
        BinaryComparison.Equals,
        ArithmeticExpression.Variable("x"),
        ArithmeticExpression.Variable("w")
      ),
      Formula.NonAllocated("x")
    );
  ] in 
  let formula = make_normal_form [] disjoints in
  let formula = DisequationSolver.f formula in

  let expected_disjoints = disjoints in
  test_expected_disjoints formula expected_disjoints []

let%test "couple of disequations" =
  let disjoints = [
    Formula.And(
      Formula.Comparison(
        BinaryComparison.LessThan,
        ArithmeticExpression.Literal(17),
        ArithmeticExpression.Variable("w")
      ),
      Formula.Comparison(
        BinaryComparison.LessThan,
        ArithmeticExpression.Literal(15),
        ArithmeticExpression.Variable("w")
      )
    );
  ] in 
  let formula = make_normal_form [] disjoints in
  let formula = DisequationSolver.f formula in

  let expected_disjoints = [
    Formula.Comparison(
      BinaryComparison.GreaterThan,
      ArithmeticExpression.Variable("w"),
      ArithmeticExpression.Literal(17)
    )
  ] in
  test_expected_disjoints formula expected_disjoints []
  
let%test "upper and lower bounds" =
  let disjoints = [
    Formula.And(
      Formula.And(
        Formula.Comparison(
          BinaryComparison.LessOrEqual,
          ArithmeticExpression.Literal(17),
          ArithmeticExpression.Variable("w")
        ),
        Formula.Comparison(
          BinaryComparison.LessThan,
          ArithmeticExpression.Literal(15),
          ArithmeticExpression.Variable("w")
        )
      ),
      Formula.And(
        Formula.Comparison(
          BinaryComparison.GreaterThan,
          ArithmeticExpression.Literal(75),
          ArithmeticExpression.Variable("w")
        ),
        Formula.Comparison(
          BinaryComparison.GreaterOrEqual,
          ArithmeticExpression.Literal(88),
          ArithmeticExpression.Variable("w")
        )
      )
    );
  ] in 
  let formula = make_normal_form [] disjoints in
  let formula = DisequationSolver.f formula in

  let expected_disjoints = [
    Formula.And(
      Formula.Comparison(
        BinaryComparison.GreaterThan,
        ArithmeticExpression.Variable("w"),
        ArithmeticExpression.Literal(16)
      ),
      Formula.Comparison(
        BinaryComparison.LessThan,
        ArithmeticExpression.Variable("w"),
        ArithmeticExpression.Literal(75)
      )
    )
  ] in
  test_expected_disjoints formula expected_disjoints []
   
let%test "mixed variables" =
  let disjoints = [
    Formula.And(
      Formula.And(
        Formula.And(
          Formula.Comparison(
            BinaryComparison.GreaterThan,
            ArithmeticExpression.Literal(75),
            ArithmeticExpression.Variable("w")
          ),
          Formula.Comparison(
            BinaryComparison.LessThan,
            ArithmeticExpression.Literal(15),
            ArithmeticExpression.Variable("x")
          )
        ),
        Formula.And(
          Formula.Comparison(
            BinaryComparison.LessOrEqual,
            ArithmeticExpression.Literal(17),
            ArithmeticExpression.Variable("x")
          ),
          Formula.Comparison(
            BinaryComparison.GreaterOrEqual,
            ArithmeticExpression.Literal(88),
            ArithmeticExpression.Variable("w")
          )
        )
      ),
      Formula.NonAllocated("k")
    );
  ] in 
  let formula = make_normal_form [] disjoints in
  let formula = DisequationSolver.f formula in

  let expected_disjoints = [
    Formula.And(
      Formula.And(
        Formula.Comparison(
          BinaryComparison.GreaterThan,
          ArithmeticExpression.Variable("x"),
          ArithmeticExpression.Literal(16)
        ),
        Formula.Comparison(
          BinaryComparison.LessThan,
          ArithmeticExpression.Variable("w"),
          ArithmeticExpression.Literal(75)
        )
      ),
      Formula.NonAllocated("k")
    );
  ] in
  test_expected_disjoints formula expected_disjoints []

let%test "three disequations" =
  let disjoints = [
    Formula.And(
      Formula.And(
        Formula.Comparison(
          BinaryComparison.LessThan,
          ArithmeticExpression.Literal(28),
          ArithmeticExpression.Variable("w")
        ),
        Formula.Comparison(
          BinaryComparison.LessThan,
          ArithmeticExpression.Literal(42),
          ArithmeticExpression.Variable("w")
        )
      ),
      Formula.Comparison(
        BinaryComparison.LessThan,
        ArithmeticExpression.Literal(77),
        ArithmeticExpression.Variable("w")
      )
    );
  ] in 
  let formula = make_normal_form [] disjoints in
  let formula = DisequationSolver.f formula in

  let expected_disjoints = [
    Formula.Comparison(
      BinaryComparison.GreaterThan,
      ArithmeticExpression.Variable("w"),
      ArithmeticExpression.Literal(77)
    )
  ] in
  test_expected_disjoints formula expected_disjoints []

let%test "recursion" =
  let disjoints = [
    Formula.AndSeparately(
      Formula.And(
        Formula.Comparison(
          BinaryComparison.LessThan,
          ArithmeticExpression.Literal(28),
          ArithmeticExpression.Variable("w")
        ),
        Formula.Comparison(
          BinaryComparison.LessThan,
          ArithmeticExpression.Literal(42),
          ArithmeticExpression.Variable("w")
        )
      ),
      Formula.NonAllocated("k")
    );
  ] in 
  let formula = make_normal_form [] disjoints in
  let formula = DisequationSolver.f formula in

  let expected_disjoints = [
    Formula.AndSeparately(
      Formula.Comparison(
        BinaryComparison.GreaterThan,
        ArithmeticExpression.Variable("w"),
        ArithmeticExpression.Literal(42)
      ),
      Formula.NonAllocated("k")
    )
  ] in
  test_expected_disjoints formula expected_disjoints []