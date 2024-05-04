open Prelude.Ast.LogicFormulas
open NormalForm
open Utils

let annot formula =
  let annotation = make_annotation 0 0 in
  annotate formula annotation

let test_expected_free_variables (normalized: normal_form) (variables: identifier list) =
  match IdentifierSet.subset (IdentifierSet.of_list (variables)) normalized.variables with
  | true -> true
  | false ->
    let _ = if IdentifierSet.is_empty normalized.variables then
      print_endline "actual identifiers: None"
    else
      print_endline "actual identifiers: ";
      IdentifierSet.iter (fun x -> print_endline x) normalized.variables
    in
    false

let test_expected_disjoints (normalized: normal_form) (expected: Formula.t list) =
  match List.for_all (fun expected ->
    let compare_fn actual = equal_formulas expected actual in
    Option.is_some (List.find_opt compare_fn normalized.disjoints)
  ) expected with
  | true -> true
  | false ->
    print_endline "actual disjoints: ";
    List.iter (fun x -> print_endline (Formula.show x)) normalized.disjoints;
    false