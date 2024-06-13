open DataStructures
open DataStructures.Analysis
open NormalForm
open Analysis_Utils

let removeBoundNonAllocations (formula: NormalForm.t) =
  (* Bound variables that are in comparison relation with free variables should not considered to be removed *)
  let rec extend_bound_vars (formula: Formula.t) (bound_vars: IdentifierSet.t) =
    match formula with
    | Comparison(_, lexpr, rexpr) ->
      let lexpr_vars = get_normal_form_expr_identifiers lexpr in
      let rexpr_vars = get_normal_form_expr_identifiers rexpr in
      let expr_vars = IdentifierSet.union lexpr_vars rexpr_vars in
      if IdentifierSet.diff expr_vars bound_vars |> IdentifierSet.is_empty |> not then
        IdentifierSet.diff bound_vars expr_vars
      else bound_vars
    | And(lformula, rformula) ->
      bound_vars |> extend_bound_vars lformula |> extend_bound_vars rformula
    | _ -> bound_vars
  in
  let rec removeBoundNonAllocations (formula: Formula.t) (bound_vars: IdentifierSet.t) =
    match formula with
    | NonAllocated(id) when IdentifierSet.find_opt id bound_vars |> Option.is_some ->
      Formula.EmptyHeap
    | And(lformula, rformula) -> 
      Formula.And(removeBoundNonAllocations lformula bound_vars, removeBoundNonAllocations rformula bound_vars)
    | AndSeparately(lformula, rformula) -> 
      Formula.AndSeparately(removeBoundNonAllocations lformula bound_vars, removeBoundNonAllocations rformula bound_vars)
    | _ -> formula
  in
  let removeBoundNonAllocations (formula: Formula.t) (bound_vars: IdentifierSet.t) =
    let extended_bound_vars = extend_bound_vars formula bound_vars in
    removeBoundNonAllocations formula extended_bound_vars
  in

  let disjoints = List.map (fun x -> removeBoundNonAllocations x formula.variables) formula.disjoints in
  NormalForm.make formula.variables disjoints formula.id_generator

let f = removeBoundNonAllocations