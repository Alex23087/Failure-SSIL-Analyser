open Analysis_Prelude
open NormalForm
open HeapSemantics
open Ast.HeapRegularCommands
open Analysis_TestUtils

(** Computes the pre-condition of the given atomic command and post-condition *)
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
    let formula = existential_disjuntive_normal_form formula in
    conjunction_of_normalized_formulas formula post_condition
  | Allocation(id) -> (* solve each disjunction indipendentely (disj rule) *)
    let disjoints = List.map (apply_alloc (post_condition.variables) id) (post_condition.disjoints) in
    make (post_condition.variables) disjoints (post_condition.id_generator)
  | Free(id) -> (* generate a fresh variable and compute precondition 
                    if the fresh variable is not used, returns old set *)
    let fresh_var, fresh_post_condition = generate_fresh_existentialized_variable post_condition in 
    let disjoints = List.map (apply_free (fresh_post_condition.variables) id fresh_var) (fresh_post_condition.disjoints) in
    let identifiers = List.map get_normal_form_disjoint_identifiers disjoints in 
    let check_fresh vars = IdentifierSet.find_opt fresh_var vars |> Option.is_some in 
    ( match (List.filter check_fresh identifiers) with
      | [] -> make (post_condition.variables) disjoints (post_condition.id_generator)
      | _  -> make (fresh_post_condition.variables) disjoints (fresh_post_condition.id_generator)
    )
  | ReadHeap(mem_id, id) ->
    raise (Failure "not implemented")
  | WriteHeap(mem_id, expr) ->
    raise (Failure "not implemented")