open DataStructures
open DataStructures.Analysis
open DataStructures.Parser
open Analysis_Utils

let annot_cmd command =
  let annotation = Commands.make_annotation 0 0 None in
  annotate command annotation

let annot formula =
  let annotation = LogicFormulas.make_annotation 0 0 in
  annotate formula annotation

(* Check the number of expected bound variables *)
let test_expected_bound_variables (normalized: NormalForm.t) (num_bound_vars: int) =
  if IdentifierSet.cardinal normalized.variables = num_bound_vars then
    true
  else
    let _ = if IdentifierSet.is_empty normalized.variables then
      print_endline "actual identifiers: None"
    else
      print_endline "actual identifiers: ";
      IdentifierSet.iter print_endline normalized.variables
    in
    false

(* Check the list of actual disjoints is the same of expected disjoints *)
let test_expected_disjoints (normalized: NormalForm.t) (expected: NormalForm.Formula.t list) =
  let rec test_expected_disjoints list1 list2 =
    match list1, list2 with
    | [], [] -> true
    | [], _::_ -> false
    | [l], [r] -> equal_formulas l r
    | l::list1, list2 ->
        let compare_fn x = equal_formulas l x in
        let r, list2 = List.partition compare_fn list2 in
        match r with
        | [] -> false
        | [_] -> test_expected_disjoints list1 list2
        | _::xs -> test_expected_disjoints list1 (xs @ list2)
    in
  test_expected_disjoints normalized.disjoints expected