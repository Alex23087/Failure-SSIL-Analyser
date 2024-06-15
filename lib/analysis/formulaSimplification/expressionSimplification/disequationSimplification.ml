open DataStructures.Analysis
open NormalForm
open ExpressionSimplificationUtils

let disequation_simplification (formula: NormalForm.t) =
  let disequation_simplification (formula: Formula.t) =
    match formula with
    | Comparison(op, lexpr, rexpr) -> (
      match op with
      | Equals | NotEquals -> formula
      | _ ->
        let expr = ArithmeticExpression.Operation(
          BinaryOperator.Minus,
          lexpr,
          rexpr
        ) in
        let expr = simplify_expression expr in
        Formula.Comparison(op, expr, ArithmeticExpression.Literal(0))
    )
    | _ -> formula
  in
  let disequation_simplification = apply_comparison_simplification disequation_simplification in

  let disjoints = List.map disequation_simplification formula.disjoints in
  NormalForm.make formula.variables disjoints formula.id_generator

let f = disequation_simplification