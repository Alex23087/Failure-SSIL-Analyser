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
  (* | Or(ll, lr), Or(rl, rr) ->
    equal_formulas ll rl && equal_formulas lr rr
  | Exists(lid, lformula), Exists(rid, rformula) ->
    lid == rid && equal_formulas lformula rformula *)
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

let%test "existentialized non bound variable" =
  let formula = annot (
    Formula.Exists("x", annot (
      Formula.Exists("y", annot (
        Formula.NonAllocated("x")
      ))
    ))
  )
  in
  let normalized = existential_disjuntive_normal_form formula 0 in
  test_expected_free_variables normalized ("x"::[])

let%test "disjoint merging" =
  let formula = annot (
    Formula.Or(annot(
      Formula.Or(annot (
        Formula.NonAllocated("x")
      ), annot (
        Formula.NonAllocated("y")
      ))
    ), annot (Formula.Or(annot (
        Formula.NonAllocated("z")
      ), annot (
        Formula.NonAllocated("w")
      ))
    ))
  ) in
  let normalized = existential_disjuntive_normal_form formula 0 in
  let expected_disjoints = 
    annot (Formula.NonAllocated("x")) :: 
    annot (Formula.NonAllocated("y")) :: 
    annot (Formula.NonAllocated("z")) :: 
    annot (Formula.NonAllocated("w")) :: []
  in
  test_expected_disjoints normalized expected_disjoints

let%test "and distribution" =
  let formula = annot (
    Formula.And(annot(
      Formula.Or(annot (
        Formula.NonAllocated("x")
      ), annot (
        Formula.NonAllocated("y")
      ))
    ), annot (Formula.Or(annot (
        Formula.NonAllocated("z")
      ), annot (
        Formula.NonAllocated("w")
      ))
    ))
  ) in 
  let normalized = existential_disjuntive_normal_form formula 0 in
  let expected_disjoints = annot ( Formula.And(
    annot (Formula.NonAllocated("x")),
    annot (Formula.NonAllocated("z"))
  )) :: annot ( Formula.And(
    annot (Formula.NonAllocated("x")),
    annot (Formula.NonAllocated("w"))
  )) :: annot ( Formula.And(
    annot (Formula.NonAllocated("y")),
    annot (Formula.NonAllocated("z"))
  )) :: annot ( Formula.And(
    annot (Formula.NonAllocated("y")),
    annot (Formula.NonAllocated("w"))
  )) :: []
  in
  test_expected_disjoints normalized expected_disjoints

let%test "and separately distribution" =
  let formula = annot (
    Formula.AndSeparately(annot(
      Formula.Or(annot (
        Formula.NonAllocated("x")
      ), annot (
        Formula.NonAllocated("y")
      ))
    ), annot (Formula.Or(annot (
        Formula.NonAllocated("z")
      ), annot (
        Formula.NonAllocated("w")
      ))
    ))
  ) in 
  let normalized = existential_disjuntive_normal_form formula 0 in
  let expected_disjoints = annot ( Formula.AndSeparately(
    annot (Formula.NonAllocated("x")),
    annot (Formula.NonAllocated("z"))
  )) :: annot ( Formula.AndSeparately(
    annot (Formula.NonAllocated("x")),
    annot (Formula.NonAllocated("w"))
  )) :: annot ( Formula.AndSeparately(
    annot (Formula.NonAllocated("y")),
    annot (Formula.NonAllocated("z"))
  )) :: annot ( Formula.AndSeparately(
    annot (Formula.NonAllocated("y")),
    annot (Formula.NonAllocated("w"))
  )) :: []
  in
  test_expected_disjoints normalized expected_disjoints

let%test "variables renaming when merging normalized forms" =
  let formula = annot (
    Formula.AndSeparately(
      annot (Formula.Exists("y",
        annot (Formula.Exists("x", 
          annot (Formula.And(
            annot (Formula.NonAllocated("x")), 
            annot (Formula.NonAllocated("y"))
          ))
        ))
      )),
      annot (Formula.Or(
        annot (Formula.Exists("x",
          annot (Formula.NonAllocated("x"))
        )),
        annot (Formula.NonAllocated("y"))
      ))
    )
  ) in 
  let normalized = existential_disjuntive_normal_form formula 0 in
  let expected_identifiers = "x" :: "0$y" :: "1$x" :: [] in
  let expected_disjoints =
    annot (Formula.AndSeparately(
      annot (Formula.And(
        annot (Formula.NonAllocated("x")),
        annot (Formula.NonAllocated("0$y"))
      )),
      annot (Formula.NonAllocated("1$x"))
    )) ::
    annot (Formula.AndSeparately(
      annot (Formula.And(
        annot (Formula.NonAllocated("x")),
        annot (Formula.NonAllocated("0$y"))
      )),
      annot (Formula.NonAllocated("y"))
    )) :: []
  in
  test_expected_free_variables normalized expected_identifiers &&
  test_expected_disjoints normalized expected_disjoints