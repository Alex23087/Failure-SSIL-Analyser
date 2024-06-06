open Normalization
open ExpressionSubstitution
open Ast.HeapRegularCommands
open DataStructures.Analysis
open Analysis_Utils

let rec weakest_precondition (command: 'a HeapAtomicCommand.t) (post_condition: NormalForm.t) (annotation_conversion) =
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
  | Allocation(id) -> (* idea: apply the disj rule, solving each disjunction indipendentely *)
    List.map (apply_alloc (post_condition.vars) id) (post_condition.disjoints) (*TODO*)
  | Free(id) ->
    raise (Failure "not implemented")
  | ReadHeap(mem_id, id) ->
    raise (Failure "not implemented")
  | WriteHeap(mem_id, expr) ->
    raise (Failure "not implemented")

(* expand_andSeparately take a formula and returns a list of formulas,
   where each element is a separately-conjunct of the original formula
   i.e. AndSeparately(e1, AndSeparately(e2, e3)) -> [e1; e2; e3]
 *)
and expand_andSeparately (formula : LogicFormulas.t) : LogicFormulas.t list = 
  match formula.node with
  | AndSeparately(exp1, exp2) -> 
    (expand_andSeparately exp1) @ (expand_andSeparately exp2)
  | _ -> [formula]

(* apply alloc semantics to single formula *)
and apply_alloc (vars : IdentifierSet.t) (id : Identifier) (post : LogicFormulas.t) : LogicFormulas.t = 
  let final = 
    match post.node with
    | True -> True
    | Exists(_,_) ->
      raise (Failure "Cannot have an existentialization as subformula")
    | Or(_,_) -> 
      raise (Failure "Cannot have a disjunction as subformula")
    | Allocation(x, _) when x <> id -> False
    | Allocation(x, _) when x = id  -> 
      if (Set.find_opt x vars |> Option.is_some) then False else EmptyHeap
    | AndSeparately(_, _) as formula -> 
      let and_list = expand_andSeparately formula in 
      let (matching_list, non_matching_list) = 
        List.partition (
          fun x -> match x with | Allocation(x,_) when x=id -> true | _ -> false
        ) and_list in 
      match matching_list with 
      | [] -> if ( 
        List.find_opt (fun x -> match x with | True -> true | _ -> false ) |> Option.is_some
        ) then formula else False
      | [_] -> 
        let t = compress_andSeparately non_matching_list in 
        let id_t = get_formula_identifiers t in  (* identifiers in non-matching list (t) *)
        let fv_t = Set.diff id_t vars (* remove bound identifiers from id_t *)
        if (Set.find_opt x fv_t |> Option.is_none) 
          then AndSeparately((annotate EmptyHeap annotation), t)
          else False
      | _ -> False
    | _ -> False
  in annotate final annotation

and compress_andSeparately (formula : LogicFormulas.t list) : LogicFormulas.t =
  List.fold_left ()... (*TODO*)