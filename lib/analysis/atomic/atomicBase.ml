open Analysis_Prelude
open NormalForm
open Ast.HeapRegularCommands

let rec compute_precondition (command: 'a HeapAtomicCommand.t) (post_condition: NormalForm.t) =
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
    let formula = existential_disjuntive_normal_form formula post_condition.last_id_generator in
    conjunction_of_normalized_formulas formula post_condition
  | Allocation(id) -> (* solve each disjunction indipendentely (disj rule) *)
    let disjoints = List.map (apply_alloc (post_condition.variables) id) (post_condition.disjoints) in (*TODO*)
    make (post_condition.variables) disjoints (post_condition.last_id_generator)
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
and expand_andSeparately (formula : Formula.t) : Formula.t list = 
  match formula with
  | AndSeparately(exp1, exp2) -> 
    (expand_andSeparately exp1) @ (expand_andSeparately exp2)
  | _ -> [formula]

(* compress_andSeparately take a list of formulas and returns 
   an andSeparately between each item of the list
   i.e. [e1; e2; e3] -> AndSeparately(e1, AndSeparately(e2, e3))
 *)
and compress_andSeparately (formula : Formula.t list) : Formula.t =
  match formula with
  | []  -> EmptyHeap
  | [x] -> x
  | x::xs -> List.fold_left (fun x y -> Formula.AndSeparately(x, y)) x xs

(* apply alloc semantics to single formula *)
and apply_alloc (vars : IdentifierSet.t) (id : identifier) (post : Formula.t) : Formula.t = 
  match post with
  | True -> True
  | Allocation(x, _) when x <> id -> False
  | Allocation(x, _) when x = id  -> 
    if (IdentifierSet.find_opt x vars |> Option.is_some) then False else EmptyHeap
  | AndSeparately(_, _) as formula -> (
    let and_list = expand_andSeparately formula in 
    let (matching_list, non_matching_list) = 
      List.partition (fun (x : Formula.t) ->
        match x with | Allocation(x,_) when x=id -> true | _ -> false
      ) and_list in 
    match matching_list with 
    | [] -> if ( 
      List.find_opt (fun (x : Formula.t) ->
        match x with | True -> true | _ -> false ) non_matching_list |> Option.is_some
      ) then formula else False
    | [_] -> 
      let t = compress_andSeparately non_matching_list in 
      let id_t = get_normal_form_disjoint_identifiers t in  (* identifiers in non-matching list (t) *)
      let fv_t = IdentifierSet.diff id_t vars in (* remove bound identifiers from id_t *)
      if (IdentifierSet.find_opt id fv_t |> Option.is_none) 
        then AndSeparately(EmptyHeap, t)
        else False
    | _ -> False
    )
  | _ -> False