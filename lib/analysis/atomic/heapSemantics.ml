open Analysis_Prelude
open NormalForm
open Atomic_Utils
open Formula

open Analysis_TestUtils

let alloc_heap_partition (formula : Formula.t) (vars : IdentifierSet.t) (id : identifier) (f : Formula.t -> bool) : Formula.t =  
  let and_list = expand_andSeparately formula in 
  let (matching_list, non_matching_list) = List.partition f and_list in 
  let t = compress_andSeparately non_matching_list in 
  match matching_list with 
  | [] -> 
    if ( 
      List.find_opt (function | True -> true | _ -> false ) non_matching_list |> 
      Option.is_some
    ) then formula else False
  | [_] -> 
    if check_frame_rule_side_condition t vars id then AndSeparately(EmptyHeap, t) 
    else False
  | _ -> False

let free_heap_partition (formula : Formula.t) (id : identifier) (f : Formula.t -> bool) (fresh_var : identifier) : Formula.t =  
  let and_list = expand_andSeparately formula in 
  try
    let (matching_list, non_matching_list) = List.partition f and_list in
    let t = compress_andSeparately non_matching_list in 
    match matching_list with 
    | [] -> 
      if ( 
        List.find_opt (function | True -> true | _ -> false ) non_matching_list |> 
        Option.is_some
      ) then AndSeparately(Allocation(id, (Variable(fresh_var))), t)
      else formula
    | _ -> AndSeparately(Allocation(id, (Variable(fresh_var))), t)
  with 
  | Bound_Identifier(_) -> False (* bound identifier -> not handled; under-approximation *)

let write_heap_partition (formula : Formula.t) (id : identifier) (f : Formula.t -> bool) (fresh_var : identifier) : Formula.t =  
  let and_list = expand_andSeparately formula in 
  let (matching_list, non_matching_list) = List.partition f and_list in 
  let t = compress_andSeparately non_matching_list in 
  match matching_list with
  | [] ->
    if ( 
      List.find_opt (function | True -> true | _ -> false ) non_matching_list |> 
      Option.is_some
    ) then t else False
  | [_] -> AndSeparately(Allocation(id, (Variable(fresh_var))), t)
  | _ -> False

let read_heap_partition (formula : Formula.t) (vars : IdentifierSet.t) (l_id : identifier) (r_id : identifier) (fresh : identifier) (f : Formula.t -> bool) (last_id : NormalForm.id_generator) : NormalForm.t =  
  let and_list = expand_andSeparately formula in 
  let (matching_list, non_matching_list) = List.partition f and_list in 
  let t = compress_andSeparately non_matching_list in 
  let normal_form_of formula = NormalForm.make vars [formula] last_id in 
  match matching_list with
  | [] -> 
    if ( 
      List.find_opt (function | True -> true | _ -> false ) non_matching_list |> 
      Option.is_some 
    ) then (* True * t[fresh/x] * y -> fresh *)
      separate_conjunction_of_normalized_formulas
        (normal_form_of True)
        (separate_conjunction_of_normalized_formulas
          (substitute_expression_in_normalized_formula 
            (normal_form_of t) (Variable(fresh)) l_id
          )
          (normal_form_of (Allocation(r_id, Variable(fresh))))
        )
    else normal_form_of False
  | [Allocation(id,a) as matched] when id = r_id-> 
    let t' = 
      substitute_expression_in_normalized_formula (
        normal_form_of t
      ) a l_id in 
    let formula = normal_form_of matched in
    let fv_a = get_free_identifiers a vars in 
    if check_frame_rule_side_condition t vars l_id && is_identifier_free l_id fv_a
      (* non controlliamo che l_id compaia *libero* nella post !
      Però questo controllo è fatto (al contrario) dalla side condition della frame rule...
      Da togliere la frame rule ? *)
      then separate_conjunction_of_normalized_formulas formula t'
    else normal_form_of False
  | _ -> normal_form_of False

(* apply alloc semantics to single formula *)
let apply_alloc (vars : IdentifierSet.t) (id : identifier) (post : Formula.t) : Formula.t = 
  match post with
  | True -> True
  | Allocation(x, _) when x = id  -> 
    if is_identifier_free x vars then EmptyHeap else False
  | AndSeparately(_, _) as formula -> 
    let f = function 
    | Allocation(x,_) when x=id -> 
      if is_identifier_free x vars then true else false
    | _ -> false in 
    alloc_heap_partition formula vars id f
  | _ -> False

(* apply free semantics to single formula *)
let apply_free (vars : IdentifierSet.t) (id : identifier) (fresh_var : identifier) (post : Formula.t) : Formula.t = 
  match post with
  | True -> True
  | EmptyHeap -> Allocation(id, (Variable(fresh_var)))
  | NonAllocated(x) when x = id -> 
    if is_identifier_free x vars 
      then (Allocation(x, (Variable(fresh_var)))) 
    else False
  | AndSeparately(_, _) as formula -> 
    let f = function 
    | NonAllocated(x) when x=id -> 
      if is_identifier_free x vars then true 
      else raise (Bound_Identifier x) (* bound identifier -> not handled; under-approximation *)
    | EmptyHeap -> true
    | _ -> false in 
    free_heap_partition formula id f fresh_var
  | _ -> False

(* apply write semantics to single formula *)
let apply_write (vars : IdentifierSet.t) (mem_id : identifier) (expr : 'a Ast.HeapRegularCommands.ArithmeticExpression.t) (fresh_var : identifier) (post : Formula.t) : Formula.t = 
  match post with
  | Allocation(x, y) when x = mem_id && equal_command_expression expr y -> 
    let ids = get_normal_form_expr_identifiers y in
    if is_identifier_free x vars && are_all_identifiers_free ids vars
      then (Allocation(x, (Variable(fresh_var))))
    else False
  | AndSeparately(_, _) as formula -> 
    let f = function 
      | Allocation(x, y) when x = mem_id && equal_command_expression expr y -> 
        let ids = get_normal_form_expr_identifiers y in
        if is_identifier_free x vars && are_all_identifiers_free ids vars
          then true else false
      | _ -> false
    in write_heap_partition formula mem_id f fresh_var
  | True -> True
  | _ -> False

(* apply read semantics to single formula *)
let apply_read (vars : IdentifierSet.t) (l_id : identifier) (r_id : identifier) (fresh : identifier) (last_id : NormalForm.id_generator) (post : Formula.t) : NormalForm.t = 
  let normal_form_of formula = NormalForm.make vars [formula] last_id in 
  match post with
  | True -> normal_form_of True
  | Allocation(y, a) as formula when y = r_id -> 
    let fv_a = get_free_identifiers a vars in 
    if is_identifier_free y vars && is_identifier_free l_id fv_a
      then normal_form_of formula
    else normal_form_of False
  | AndSeparately(_, _) as formula -> 
    let f = function
      | Allocation(y, _) when y = r_id ->
        if is_identifier_free y vars then true else false
      | _ -> false
    in read_heap_partition formula vars l_id r_id fresh f last_id
  | _ -> normal_form_of False