open DataStructures
open DataStructures.Analysis
open NormalForm

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
    
let rec get_normal_form_expr_identifiers (expr: ArithmeticExpression.t) =
  match expr with
  | Literal(_) -> (IdentifierSet.empty)
  | Variable(id) -> (IdentifierSet.singleton id)
  | Operation(_, lexpr, rexpr) ->
    let l_ids = get_normal_form_expr_identifiers lexpr in
    let r_ids = get_normal_form_expr_identifiers rexpr in
    IdentifierSet.union l_ids r_ids

let rec get_normal_form_disjoint_identifiers (formula: Formula.t) =
  match formula with
  | True | False | EmptyHeap ->
    IdentifierSet.empty
  | Allocation(id, _) | NonAllocated(id) ->
    IdentifierSet.singleton id
  | Comparison(_, lexpr, rexpr) ->
    IdentifierSet.union (get_normal_form_expr_identifiers lexpr) (get_normal_form_expr_identifiers rexpr)
    | And(lformula, rformula) | AndSeparately(lformula, rformula) ->
      IdentifierSet.union (get_normal_form_disjoint_identifiers lformula) (get_normal_form_disjoint_identifiers rformula)
      
let normal_form_free_variables (formula: NormalForm.t) =
  let ids = List.map get_normal_form_disjoint_identifiers formula.disjoints in
  let ids = List.fold_left (fun acc ids -> IdentifierSet.union acc ids) IdentifierSet.empty ids in
  IdentifierSet.diff ids formula.variables
