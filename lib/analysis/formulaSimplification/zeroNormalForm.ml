open DataStructures.Analysis
open NormalForm

(** p && false = false = p * false, therefore if false appears at any point of a normal form formula, the result is false *)
let zero_normal_form (formula: NormalForm.t) =
  let rec contains_false (formula : Formula.t) : bool =
    match formula with
    | True -> false
    | False -> true
    | And(q1, q2) -> contains_false q1 || contains_false q2
    | Comparison(_) -> false
    | EmptyHeap -> false
    | NonAllocated(_) -> false
    | Allocation(_) -> false
    | AndSeparately(q1, q2) -> contains_false q1 || contains_false q2
  in
  NormalForm.make formula.variables (List.filter (fun q -> not (contains_false q)) formula.disjoints) formula.id_generator

let f = zero_normal_form