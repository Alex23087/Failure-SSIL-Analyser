open DataStructures
open DataStructures.Analysis
open NormalForm
open NormalizationMethods
open NormalizationUtils

(** [existential_disjuntive_normal_form] [formula] [last_id_generator] converts a [formula] in Existential Disjunctive Normal Form.
The parameter [last_id_generator] is used to build fresh names.
*)
let rec existential_disjuntive_normal_form (formula: 'a Ast.AnnotationLogic.t) (last_id_generator: int) =
  match formula.node with
  | True | False | EmptyHeap | NonAllocated(_)
  | Comparison(_, _, _) | Allocation(_, _) ->
    let formula = remove_annotation_in_formula formula in
    NormalForm.make IdentifierSet.empty [formula] last_id_generator
  | And(lformula, rformula) ->
    (* normalize recursively *)
    let lformula = existential_disjuntive_normal_form lformula last_id_generator in
    let rformula = existential_disjuntive_normal_form rformula lformula.last_id_generator in
    conjunction_of_normalized_formulas lformula rformula
  | AndSeparately(lformula, rformula) ->
    (* normalize recursively *)
    let lformula = existential_disjuntive_normal_form lformula last_id_generator in
    let rformula = existential_disjuntive_normal_form rformula lformula.last_id_generator in
    separate_conjunction_of_normalized_formulas lformula rformula
  | Exists(id, subformula) ->
    (* normalize recursively *)
    let subformula = existential_disjuntive_normal_form subformula last_id_generator in
    existentialization_of_identifier id subformula
  | Or(lformula, rformula) ->
    (* normalize recursively *)
    let lformula = existential_disjuntive_normal_form lformula last_id_generator in
    let rformula = existential_disjuntive_normal_form rformula lformula.last_id_generator in
    disjunction_of_normalized_formulas lformula rformula
(** [existential_disjuntive_normal_expr] [expr] transforms the given arithmetic expression [expr] in Normal Form *)
and existential_disjuntive_normal_expr (expr: 'a Ast.AnnotationLogic.ArithmeticExpression.t) =
  remove_annotation_in_expr expr