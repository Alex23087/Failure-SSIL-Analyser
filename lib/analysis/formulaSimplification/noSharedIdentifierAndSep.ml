open DataStructures
open DataStructures.Analysis
open NormalForm
open Analysis_Utils

let rec get_heap_identifiers (formula: Formula.t) =
  match formula with
  | True | False | EmptyHeap | Comparison _ ->
    IdentifierSet.empty
  | Allocation(id, _) | NonAllocated(id) ->
    IdentifierSet.singleton id
  | And(lformula, rformula) | AndSeparately(lformula, rformula) ->
    IdentifierSet.union (get_heap_identifiers lformula) (get_heap_identifiers rformula)

(** x -> w * x -> z = false *)
let unit_and_sep (formula: NormalForm.t) =
  let rec are_separated formula = match formula with
    | Formula.AndSeparately (f1, f2) ->
      (* poor man's short circuiting *)
      let is_ok = are_separated f1 in
      let is_ok = is_ok && are_separated f2 in
      if is_ok then
        let f1_identifiers = get_heap_identifiers f1 in
        let f2_identifiers = get_heap_identifiers f2 in
        IdentifierSet.disjoint f1_identifiers f2_identifiers
      else false
    | Formula.And (f1, f2) -> are_separated f1 && are_separated f2
    | _ -> true in
  NormalForm.make formula.variables (List.map (fun disjoint -> if are_separated disjoint then disjoint else Formula.False) formula.disjoints) formula.id_generator

let f = unit_and_sep