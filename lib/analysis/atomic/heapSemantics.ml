open Analysis_Prelude
open NormalForm
open Atomic_Utils
open Formula

let heap_partition (formula : Formula.t) (vars : IdentifierSet.t) (id : identifier) (f : Formula.t -> bool) : Formula.t =  
  let and_list = expand_andSeparately formula in 
  let (matching_list, non_matching_list) = 
    List.partition f and_list in 
  match matching_list with 
  | [] -> if ( 
      List.find_opt (function | True -> true | _ -> false ) non_matching_list |> 
      Option.is_some
    ) then formula else False
  | [_] -> 
    let t = compress_andSeparately non_matching_list in 
    let id_t = get_normal_form_disjoint_identifiers t in  (* identifiers in non-matching list (t) *)
    let fv_t = IdentifierSet.diff id_t vars in (* remove bound identifiers from id_t *)
    if (IdentifierSet.find_opt id fv_t |> Option.is_none) 
      then AndSeparately(EmptyHeap, t)
      else False
  | _ -> False

(* apply alloc semantics to single formula *)
let apply_alloc (vars : IdentifierSet.t) (id : identifier) (post : Formula.t) : Formula.t = 
  match post with
  | True -> True
  | Allocation(x, _) when x <> id -> False
  | Allocation(x, _) when x = id  -> 
    if (IdentifierSet.find_opt x vars |> Option.is_some) then False else EmptyHeap
  | AndSeparately(_, _) as formula -> 
    let f = function | Allocation(x,_) when x=id -> true | _ -> false in 
    heap_partition formula vars id f
  | _ -> False

(* apply free semantics to single formula *)
let apply_free (vars : IdentifierSet.t) (id : identifier) (post : Formula.t) : Formula.t = 
  match post with
  | True -> True
  | Allocation(x, _) when x <> id -> False
  | Allocation(x, _) when x = id  -> 
    if (IdentifierSet.find_opt x vars |> Option.is_some) then False else EmptyHeap
  | AndSeparately(_, _) as formula -> 
    let f = function | Allocation(x,_) when x=id -> true | _ -> false in 
    heap_partition formula vars id f
  | _ -> False