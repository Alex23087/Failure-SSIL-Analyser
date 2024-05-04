open Prelude.Ast.Commands
open Prelude.Ast.LogicFormulas
open NormalForm
open ExpressionSubstitution
open Utils

let command_bexpression_to_normalized_formula (expr: Prelude.Ast.Commands.BooleanExpression.t) (last_phantom_id: int) =
  let formula = command_bexpression_to_logic_formula expr in
  existential_disjuntive_normal_form formula last_phantom_id

let weakest_precondition (command: HeapAtomicCommand.t) (post_condition: normal_form) =
  let annotation = command_annotation_to_logic_annotation command.annotation in
  match command.node with
  | Skip ->
    post_condition
  | Assignment(id, expr) ->
    let expr = command_expression_to_logic_expression expr in
    substitute_expression_in_normalized_formula post_condition expr id
  | NonDet(id) ->
    existentialization_of_identifier id post_condition annotation
  | Guard(expr) ->
    let formula = command_bexpression_to_normalized_formula expr post_condition.last_phantom_id in
    conjunction_of_normalized_formulas formula post_condition formula.last_phantom_id
  | Allocation(id) ->
    raise (Failure "not implemented")
  | Free(id) ->
    raise (Failure "not implemented")
  | ReadHeap(mem_id, id) ->
    raise (Failure "not implemented")
  | WriteHeap(mem_id, expr) ->
    raise (Failure "not implemented")