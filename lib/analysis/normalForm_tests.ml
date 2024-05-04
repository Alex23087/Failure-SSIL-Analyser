open Prelude.Ast.LogicFormulas
open NormalForm

let annot formula =
  let annotation = make_annotation 0 0 in
  annotate formula annotation

let rec equal_formulas (lformula: Formula.t) (rformula: Formula.t) =
  let rec equal_expressions (lexpr: ArithmeticExpression.t) (rexpr: ArithmeticExpression.t) =
    match (lexpr.node, rexpr.node) with
    | (Literal(l), Literal(r)) -> l == r
    | (Variable(l), Variable(r)) -> l == r
    | (Operation(lop, ll, lr), Operation(rop, rl, rr)) ->
      lop == rop && (equal_expressions ll rl) && (equal_expressions lr rr)
    | _ -> false
  in
  match (lformula.node, rformula.node) with
  | True, True
  | False, False
  | EmptyHeap, EmptyHeap ->
    true
  | Allocation(lid, lexpr), Allocation(rid, rexpr) ->
    lid == rid && equal_expressions lexpr rexpr
  | NonAllocated(lid), NonAllocated(rid) ->
    lid == rid
  | Comparison(lop, ll, lr), Comparison(rop, rl, rr) ->
    lop == rop && (equal_expressions ll rl) && (equal_expressions lr rr)
  | And(ll, lr), And(rl, rr)
  | Or(ll, lr), Or(rl, rr)
  | AndSeparately(ll, lr), AndSeparately(rl, rr) ->
    equal_formulas ll lr && equal_formulas rl rr
  | Exists(lid, lformula), Exists(rid, rformula) ->
    lid == rid && equal_formulas lformula rformula
  | _ -> false

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
  IdentifierSet.equal normalized.variables (IdentifierSet.of_list ("x"::[]))

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
  let disjoint = annot ( Formula.And(
    annot (Formula.NonAllocated("x")),
    annot (Formula.NonAllocated("z"))
  )) in
  Option.is_some (List.find_opt (fun x -> x == disjoint) normalized.disjoints)