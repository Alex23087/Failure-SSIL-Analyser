open DataStructures
open DataStructures.Analysis
open NormalForm
open Analysis_Utils
open VariableHandling

let conjunction_of_normalized_formulas (lformula: NormalForm.t) (rformula: NormalForm.t) (last_phantom_id: int) =
  let make_and_disjoints (lformula: NormalForm.t) (rformula: NormalForm.t) =
    let cartesian = list_cartesian lformula.disjoints rformula.disjoints in
    List.map (fun (l, r) -> Formula.And(l, r)) cartesian
  in
  merge_two_formulas lformula rformula last_phantom_id make_and_disjoints

let separate_conjunction_of_normalized_formulas (lformula: NormalForm.t) (rformula: NormalForm.t) (last_phantom_id: int) =
  let make_and_separately_disjoints (lformula: NormalForm.t) (rformula: NormalForm.t) =
    let cartesian = list_cartesian lformula.disjoints rformula.disjoints in
    List.map (fun (l, r) -> Formula.AndSeparately(l, r)) cartesian
  in
  merge_two_formulas lformula rformula last_phantom_id make_and_separately_disjoints

let disjunction_of_normalized_formulas (lformula: NormalForm.t) (rformula: NormalForm.t) (last_phantom_id: int) =
  let make_or_disjoints (lformula: NormalForm.t) (rformula: NormalForm.t) =
    lformula.disjoints @ rformula.disjoints
  in
  merge_two_formulas lformula rformula last_phantom_id make_or_disjoints

let existentialization_of_identifier (exist_id: identifier) (subformula: NormalForm.t) =
  let variables = subformula.variables in
  let disjoints = subformula.disjoints in
  let phantom_id = subformula.last_phantom_id in

  match IdentifierSet.find_opt exist_id subformula.variables with
  | Some(_) ->
    (* if the given identifier has already been existentialized, we don't add
        it to the free variables as it cannot occur in the subformulas *)
    NormalForm.make variables disjoints phantom_id
  | None ->
    (* if the existentialized variable does not occur in the subformula disjoints, we don't need to add it *)
    let free_variables = normal_form_free_variables subformula in
    match IdentifierSet.find_opt exist_id free_variables with
    | Some(_) -> NormalForm.make (IdentifierSet.add exist_id variables) disjoints phantom_id
    | None -> NormalForm.make variables disjoints phantom_id

let rec existential_disjuntive_normal_form (formula: 'a Ast.AnnotationLogic.t) (last_phantom_id: int) =
  match formula.node with
  | True | False | EmptyHeap | NonAllocated(_)
  | Comparison(_, _, _) | Allocation(_, _) ->
    let formula = remove_annotation_in_formula formula in
    NormalForm.make IdentifierSet.empty [formula] last_phantom_id
  | And(lformula, rformula) ->
    (* normalize recursively *)
    let lformula = existential_disjuntive_normal_form lformula last_phantom_id in
    let rformula = existential_disjuntive_normal_form rformula lformula.last_phantom_id in
    conjunction_of_normalized_formulas lformula rformula rformula.last_phantom_id
  | AndSeparately(lformula, rformula) ->
    (* normalize recursively *)
    let lformula = existential_disjuntive_normal_form lformula last_phantom_id in
    let rformula = existential_disjuntive_normal_form rformula lformula.last_phantom_id in
    separate_conjunction_of_normalized_formulas lformula rformula rformula.last_phantom_id
  | Exists(id, subformula) ->
    (* normalize recursively *)
    let subformula = existential_disjuntive_normal_form subformula last_phantom_id in
    existentialization_of_identifier id subformula
  | Or(lformula, rformula) ->
    (* normalize recursively *)
    let lformula = existential_disjuntive_normal_form lformula last_phantom_id in
    let rformula = existential_disjuntive_normal_form rformula lformula.last_phantom_id in
    disjunction_of_normalized_formulas lformula rformula rformula.last_phantom_id