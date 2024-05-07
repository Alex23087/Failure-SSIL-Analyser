open NormalForm
open ExpressionSubstitution
open Prelude.Ast
open NormalForm
open Utils

let command_bexpression_to_normalized_formula (expr: 'a Commands.BooleanExpression.t) (last_phantom_id: int) (annotation_conversion) =
  let formula = command_bexpression_to_logic_formula expr annotation_conversion in
  existential_disjuntive_normal_form formula last_phantom_id

let weakest_precondition (command: 'a Commands.HeapAtomicCommand.t) (post_condition: NormalForm.t) (annotation_conversion) =
  let annotation = annotation_conversion command.annotation in
  match command.node with
  | Skip ->
    post_condition
  | Assignment(id, expr) ->
    let expr = command_expression_to_logic_expression expr annotation_conversion in
    substitute_expression_in_normalized_formula post_condition expr id
  | NonDet(id) ->
    existentialization_of_identifier id post_condition annotation
  | Guard(expr) ->
    let formula = command_bexpression_to_normalized_formula expr post_condition.last_phantom_id annotation_conversion in
    conjunction_of_normalized_formulas formula post_condition formula.last_phantom_id
  | Allocation(id) ->
    raise (Failure "not implemented")
  | Free(id) ->
    raise (Failure "not implemented")
  | ReadHeap(mem_id, id) ->
    raise (Failure "not implemented")
  | WriteHeap(mem_id, expr) ->
    raise (Failure "not implemented")