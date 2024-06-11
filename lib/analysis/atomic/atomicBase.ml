open Analysis_Prelude
open NormalForm
open HeapSemantics
open Ast.HeapRegularCommands
open Analysis_TestUtils

(** Computes the pre-condition of the given atomic command and post-condition *)
let compute_precondition (command: 'a HeapAtomicCommand.t) (post_condition: NormalForm.t) =
  let precondition = 
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
    | Free(id) -> 
      let f (list, pc) dj = 
        let name, pc = generate_fresh_existentialized_variable pc in
        ((name, dj)::list, pc) in
      let (l, fresh_post_condition) = 
        List.fold_left f ([], post_condition) post_condition.disjoints in
      let disjoints = 
        List.map (fun (x,y) -> apply_free (fresh_post_condition.variables) id x y) l in
      make (fresh_post_condition.variables) disjoints (fresh_post_condition.id_generator)
    | ReadHeap(mem_id, id) ->
      raise (Failure "not implemented")
    | WriteHeap(mem_id, expr) ->
      raise (Failure "not implemented")
  in simplify_formula precondition
