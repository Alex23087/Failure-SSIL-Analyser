open DataStructures
open DataStructures.Analysis
open NormalForm
open ExpressionSimplificationUtils

let equation_simplification (formula: NormalForm.t) =
  let equation_simplification (formula: Formula.t) =
    match formula with
    | Comparison(op, lexpr, rexpr) -> (
      match op with
      | Equals
      | NotEquals ->
        let any_var = IdentifierSet.choose_opt (Analysis_Utils.get_normal_form_disjoint_identifiers formula) in
        if Option.is_some any_var then
          let lexpr, rexpr = simplify_equation lexpr rexpr (Option.get any_var) in
          Formula.Comparison(op, lexpr, rexpr)
        else
          formula
      | _ -> formula
    )
    | _ -> formula
  in
  let equation_simplification = apply_expression_simplification equation_simplification in

  let disjoints = List.map equation_simplification formula.disjoints in
  NormalForm.make formula.variables disjoints formula.id_generator

let f = equation_simplification