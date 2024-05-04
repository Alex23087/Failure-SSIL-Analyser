open Prelude.Ast.LogicFormulas
open NormalForm

let annot formula =
  let annotation = make_annotation 0 0 in
  annotate formula annotation

let rec equal_formulas (lformula: Formula.t) (rformula: Formula.t) =
  let rec equal_expressions (lexpr: ArithmeticExpression.t) (rexpr: ArithmeticExpression.t) =
    match (lexpr.node, rexpr.node) with
    | (Literal(l), Literal(r)) -> l = r
    | (Variable(l), Variable(r)) -> l = r
    | (Operation(lop, ll, lr), Operation(rop, rl, rr)) ->
      lop = rop && (equal_expressions ll rl) && (equal_expressions lr rr)
    | _ -> false
  in
  match (lformula.node, rformula.node) with
  | True, True
  | False, False
  | EmptyHeap, EmptyHeap ->
    true
  | Allocation(lid, lexpr), Allocation(rid, rexpr) ->
    lid = rid && equal_expressions lexpr rexpr
  | NonAllocated(lid), NonAllocated(rid) ->
    lid = rid
  | Comparison(lop, ll, lr), Comparison(rop, rl, rr) ->
    lop = rop && (equal_expressions ll rl) && (equal_expressions lr rr)
  | And(ll, lr), And(rl, rr)
  | AndSeparately(ll, lr), AndSeparately(rl, rr) ->
    equal_formulas ll rl && equal_formulas lr rr
  | _, Or(_, _) | _, Exists(_, _) ->
    raise (Failure "expected formula which cannot appear in normalized form")
  | Or(_, _), _ | Exists(_, _), _ ->
    raise (Failure "disjunctions and existentials cannot appear in the normalized form")
  | _ ->
    false

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