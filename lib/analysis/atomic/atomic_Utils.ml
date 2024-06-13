open Analysis_Prelude
open NormalForm

(* expand_andSeparately take a formula and returns a list of formulas,
   where each element is a separately-conjunct of the original formula
   i.e. AndSeparately(e1, AndSeparately(e2, e3)) -> [e1; e2; e3]
 *)
let rec expand_andSeparately (formula : Formula.t) : Formula.t list = 
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

(* given a heap partition t, this function checks mod(r) intersect fv(t) = empty 
   the function takes two possible results to return (one per branch)
 *)
let check_frame_rule_side_condition t vars mod_r res_then res_else =
  let id_t = get_normal_form_disjoint_identifiers t in  (* identifiers in non-matching list (t) *)
  let fv_t = IdentifierSet.diff id_t vars in (* remove bound identifiers from id_t *)
  if (IdentifierSet.find_opt mod_r fv_t |> Option.is_none) 
    then res_then
  else res_else

(* Check if x is not bound i.e. x is not in vars 
   if x is free, return res, otherwise return False 
 *)
let is_identifier_free (x : identifier) (vars : IdentifierSet.t) : bool =
  if (IdentifierSet.find_opt x vars |> Option.is_some) then false
  else true