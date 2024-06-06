open DataStructures
open DataStructures.Analysis
open NormalForm
open NormalizationMethods
open NormalizationUtils

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
and existential_disjuntive_normal_expr (expr: 'a Ast.AnnotationLogic.ArithmeticExpression.t) =
  remove_annotation_in_expr expr