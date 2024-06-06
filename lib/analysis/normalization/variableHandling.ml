open DataStructures
open DataStructures.Analysis
open Analysis_Utils

let normal_form_free_variables (formula: NormalForm.t) =
  let ids = List.map get_normal_form_disjoint_identifiers formula.disjoints in
  let ids = List.fold_left (fun acc ids -> IdentifierSet.union acc ids) IdentifierSet.empty ids in
  IdentifierSet.diff ids formula.variables

let rename_common_free_variables (lformula: NormalForm.t) (rformula: NormalForm.t) (last_phantom_id: int) =
  let lformula_free, lformula_bound = normal_form_free_variables lformula, lformula.variables in
  let rformula_free, rformula_bound = normal_form_free_variables rformula, rformula.variables in

  (* rename the bound variables in lformula that are free in rformula *)
  let lformula_vars_to_rename = IdentifierSet.inter lformula_bound rformula_free in
  let (variables, disjoints, last_phantom_id) =
    IdentifierSet.fold (fun elem (variables, disjoints, phantom_id) -> rename_variable_in_disjoints elem variables disjoints phantom_id)
    lformula_vars_to_rename (lformula.variables, lformula.disjoints, last_phantom_id)
  in
  let lformula = NormalForm.make variables disjoints last_phantom_id in

  (* rename the bound variables in rformula that are free in lformula *)
  let rformula_vars_to_rename = IdentifierSet.inter rformula_bound lformula_free in
  let (variables, disjoints, last_phantom_id) =
    IdentifierSet.fold (fun elem (variables, disjoints, phantom_id) -> rename_variable_in_disjoints elem variables disjoints phantom_id)
    rformula_vars_to_rename (rformula.variables, rformula.disjoints, last_phantom_id)
  in
  let rformula = NormalForm.make variables disjoints last_phantom_id in

  (* rename the common bound variables only in the rformulas (it would have been indifferent if were renamed them in lformulas) *)
  let common_vars_to_rename = IdentifierSet.inter lformula_bound rformula_bound in
  let (variables, disjoints, last_phantom_id) =
    IdentifierSet.fold (fun elem (variables, disjoints, phantom_id) -> rename_variable_in_disjoints elem variables disjoints phantom_id)
    common_vars_to_rename (rformula.variables, rformula.disjoints, last_phantom_id)
  in
  let rformula = NormalForm.make variables disjoints last_phantom_id in
  (lformula, rformula, last_phantom_id)

let rename_variable_in_normal_formula (formula: NormalForm.t) (var: identifier) (new_name: identifier) =
  let variables = rename_variable_in_set formula.variables var new_name in
  let disjoints = List.map (function x -> rename_variable_in_formula x var new_name) formula.disjoints in
  NormalForm.make variables disjoints formula.last_phantom_id

let merge_two_formulas (lformula: NormalForm.t) (rformula: NormalForm.t) (last_phantom_id: int) make_disjoints =
  let (lformula, rformula, last_phantom_id) = rename_common_free_variables lformula rformula last_phantom_id in
  let bound_variables = IdentifierSet.union lformula.variables rformula.variables in
  let disjoints = make_disjoints lformula rformula in
  NormalForm.make bound_variables disjoints last_phantom_id