open Ast.AnnotationLogic

let rec equal_formulas (lformula: 'a Formula.t) (rformula: 'a Formula.t) =
  let rec equal_expressions (lexpr: 'a ArithmeticExpression.t) (rexpr: 'a ArithmeticExpression.t) =
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