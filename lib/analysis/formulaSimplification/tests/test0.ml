open DataStructures.Analysis
open NormalForm
open Base

let%test_unit "test 'p && false = false' simplification" =
  let formula = NormalForm.make_from_formula (
    Formula.And(Formula.False, Formula.Allocation("x", ArithmeticExpression.Literal(1)))
  ) in
  let simplified_formula = FormulaSimplificationBase.simplify_formula formula in
  let expected = NormalForm.make_from_formula Formula.False in
  [%test_eq: Formula.t list] simplified_formula.disjoints expected.disjoints

let%test_unit "test 'p && true = p' simplification" =
  let p = Formula.Allocation("x", ArithmeticExpression.Literal(1)) in
  let formula = NormalForm.make_from_formula (
    Formula.And(Formula.True, p)
  ) in
  let simplified_formula = FormulaSimplificationBase.simplify_formula formula in
  let expected = NormalForm.make_from_formula p in
  [%test_eq: Formula.t list] simplified_formula.disjoints expected.disjoints