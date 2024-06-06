open DataStructures.Analysis.NormalForm

let rec equal_formulas (lformula: Formula.t) (rformula: Formula.t) =
  let rec equal_expressions (lexpr: ArithmeticExpression.t) (rexpr: ArithmeticExpression.t) =
    match (lexpr, rexpr) with
    | (Literal(l), Literal(r)) -> l = r
    | (Variable(l), Variable(r)) -> l = r
    | (Operation(lop, ll, lr), Operation(rop, rl, rr)) ->
      lop = rop && (equal_expressions ll rl) && (equal_expressions lr rr)
    | _ -> false
  in
  match (lformula, rformula) with
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
  | _ ->
    false