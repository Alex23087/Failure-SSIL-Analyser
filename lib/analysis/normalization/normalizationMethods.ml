open DataStructures
open DataStructures.Analysis
open NormalForm

open Utils
open Analysis_Utils
open NormalizationUtils

let conjunction_of_normalized_formulas (lformula: NormalForm.t) (rformula: NormalForm.t) =
  let make_and_disjoints (lformula: NormalForm.t) (rformula: NormalForm.t) =
    let cartesian = list_cartesian lformula.disjoints rformula.disjoints in
    List.map (fun (l, r) -> Formula.And(l, r)) cartesian
  in
  let last_id_generator = compute_last_id_generator lformula rformula in
  merge_two_formulas lformula rformula last_id_generator make_and_disjoints

let separate_conjunction_of_normalized_formulas (lformula: NormalForm.t) (rformula: NormalForm.t) =
  let make_and_separately_disjoints (lformula: NormalForm.t) (rformula: NormalForm.t) =
    let cartesian = list_cartesian lformula.disjoints rformula.disjoints in
    List.map (fun (l, r) -> Formula.AndSeparately(l, r)) cartesian
  in
  let last_id_generator = compute_last_id_generator lformula rformula in
  merge_two_formulas lformula rformula last_id_generator make_and_separately_disjoints

let disjunction_of_normalized_formulas (lformula: NormalForm.t) (rformula: NormalForm.t) =
  let make_or_disjoints (lformula: NormalForm.t) (rformula: NormalForm.t) =
    lformula.disjoints @ rformula.disjoints
  in
  let last_id_generator = compute_last_id_generator lformula rformula in
  merge_two_formulas lformula rformula last_id_generator make_or_disjoints

let existentialization_of_identifier (exist_id: identifier) (subformula: NormalForm.t) =
  let variables = subformula.variables in
  let disjoints = subformula.disjoints in
  let id_generator = subformula.last_id_generator in

  match IdentifierSet.find_opt exist_id subformula.variables with
  | Some(_) ->
    (* if the given identifier has already been existentialized, we don't add
        it to the free variables as it cannot occur in the subformulas *)
    NormalForm.make variables disjoints id_generator
  | None ->
    (* if the existentialized variable does not occur in the subformula disjoints, we don't need to add it *)
    let free_variables = normal_form_free_variables subformula in
    match IdentifierSet.find_opt exist_id free_variables with
    | Some(_) -> NormalForm.make (IdentifierSet.add exist_id variables) disjoints id_generator
    | None -> NormalForm.make variables disjoints id_generator