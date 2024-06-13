open DataStructures
open DataStructures.Analysis
open NormalForm

let removeBoundNonAllocations (formula: NormalForm.t) =
  let rec removeBoundNonAllocations (formula: Formula.t) (bound_vars: IdentifierSet.t) =
    match formula with
    | And(lformula, rformula) -> 
      Formula.And(removeBoundNonAllocations lformula bound_vars, removeBoundNonAllocations rformula bound_vars)
    | AndSeparately(lformula, rformula) -> 
      Formula.AndSeparately(removeBoundNonAllocations lformula bound_vars, removeBoundNonAllocations rformula bound_vars)
    | NonAllocated(id) when IdentifierSet.find_opt id bound_vars |> Option.is_some -> Formula.EmptyHeap
    | _ -> formula
  in

  let disjoints = List.map (fun x -> removeBoundNonAllocations x formula.variables) formula.disjoints in
  NormalForm.make formula.variables disjoints formula.id_generator

let f = removeBoundNonAllocations