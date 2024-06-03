open Normalization
open ExpressionSubstitution
open Ast.HeapRegularCommands
open DataStructures.Analysis
open Analysis_Utils

let weakest_precondition (command: 'a HeapAtomicCommand.t) (post_condition: NormalForm.t) (annotation_conversion) =
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
    let formula = command_bexpression_to_logic_formula expr annotation_conversion in
    let formula = existential_disjuntive_normal_form formula post_condition.last_phantom_id in
    conjunction_of_normalized_formulas formula post_condition formula.last_phantom_id
  | Allocation(_id) ->
    raise (Failure "not implemented")
  | Free(_id) ->
    raise (Failure "not implemented")
  | ReadHeap(_mem_id, _id) ->
    raise (Failure "not implemented")
  | WriteHeap(_mem_id, _expr) ->
    raise (Failure "not implemented")
