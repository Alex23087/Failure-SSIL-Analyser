open Analysis_Prelude
open NormalForm
open Atomic_Utils
open Formula

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
    let res_then = AndSeparately(EmptyHeap, t) in 
    check_frame_rule_side_condition t vars id res_then False
  | _ -> False

let free_heap_partition (formula : Formula.t) (id : identifier) (f : Formula.t -> bool) (fresh_var : identifier) : Formula.t =  
  let and_list = expand_andSeparately formula in 
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

(* apply alloc semantics to single formula *)
let apply_alloc (vars : IdentifierSet.t) (id : identifier) (post : Formula.t) : Formula.t = 
  match post with
  | True -> True
  | Allocation(x, _) when x = id  -> 
    if is_identifier_free x vars then EmptyHeap else False
  | AndSeparately(_, _) as formula -> 
    let f = function | Allocation(x,_) when x=id -> true | _ -> false in 
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
    | NonAllocated(x) when x=id -> true
    | EmptyHeap -> true
    | _ -> false in 
    free_heap_partition formula id f fresh_var
  | _ -> False

(* apply write semantics to single formula *)
let apply_write (vars : IdentifierSet.t) (mem_id : identifier) (expr : 'a Ast.HeapRegularCommands.ArithmeticExpression.t) (fresh_var : identifier) (post : Formula.t) : Formula.t = 
  match post with
  | Allocation(x, y) when x = mem_id && equal_command_expression expr y -> 
    let ids = get_normal_form_expr_identifiers y in
    if is_identifier_free x vars && 
      IdentifierSet.for_all (fun id -> is_identifier_free id vars) ids
    then (Allocation(x, (Variable(fresh_var))))
    else False
  | AndSeparately(_, _) as formula -> 
    let f = function 
      | Allocation(x, y) when x = mem_id && equal_command_expression expr y -> true
      | _ -> false
    in write_heap_partition formula mem_id f fresh_var
  | True -> True
  | _ -> False