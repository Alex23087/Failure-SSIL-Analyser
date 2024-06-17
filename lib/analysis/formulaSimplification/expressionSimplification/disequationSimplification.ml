open DataStructures
open DataStructures.Analysis
open NormalForm
open ExpressionSimplificationUtils

let disequation_simplification (formula: NormalForm.t) =
  let bool_to_formula bool = if bool then Formula.True else Formula.False in
  let disequation_simplification (formula: Formula.t) =
    match formula with
    | Comparison(op, lexpr, rexpr) -> (
      match op with
      | Equals | NotEquals -> formula
      | _ when IdentifierSet.is_empty (Analysis_Utils.get_normal_form_disjoint_identifiers formula) -> (
        let expr = ArithmeticExpression.Operation(BinaryOperator.Minus, lexpr, rexpr) in
        let expr = simplify_expression expr in
        let result =
          match expr, op with
          | Literal(value), LessThan -> value < 0
          | Literal(value), LessOrEqual -> value <= 0
          | Literal(value), GreaterThan -> value > 0
          | Literal(value), GreaterOrEqual -> value >= 0
          | _ -> failwith "unexpected"
        in
        bool_to_formula result
      )
      | _ -> formula        
    )
    | _ -> formula
  in
  let disequation_simplification = apply_expression_simplification disequation_simplification in

  let disjoints = List.map disequation_simplification formula.disjoints in
  NormalForm.make formula.variables disjoints formula.id_generator

let f = disequation_simplification