open Analysis_Prelude
open Ast.HeapRegularCommands

let compute_precondition (command: 'a HeapAtomicCommand.t) (post_condition: NormalForm.t) =
  match command.node with
  | Skip ->
    post_condition
  | Assignment(id, expr) ->
    let expr = command_expression_to_logic_expression expr (fun _ -> ()) in
    let expr = existential_disjuntive_normal_expr expr in
    substitute_expression_in_normalized_formula post_condition expr id
  | NonDet(id) ->
    existentialization_of_identifier id post_condition
  | Guard(expr) ->
    let formula = command_bexpression_to_logic_formula expr (fun _ -> ()) in
    let formula = existential_disjuntive_normal_form formula post_condition.last_phantom_id in
    conjunction_of_normalized_formulas formula post_condition formula.last_phantom_id
  | Allocation(id) ->
    raise (Failure "not implemented")
  | Free(id) ->
    raise (Failure "not implemented")
  | ReadHeap(mem_id, id) ->
    raise (Failure "not implemented")
  | WriteHeap(mem_id, expr) ->
    raise (Failure "not implemented")