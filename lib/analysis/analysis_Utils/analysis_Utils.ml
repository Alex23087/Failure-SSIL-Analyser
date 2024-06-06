include RenameVariable
include NormalFormUtils
include CommandToLogicConversion

open DataStructures
open Parser

open Ast.AnnotationLogic

let annotate = DataStructures.AnnotatedNode.make
let remove_annotation (formula: 'a Ast.AnnotationLogic.t) : DataStructures.Analysis.NormalForm.Formulas.t =
  let rec remove_annotation_in_formula (formula: 'a Ast.AnnotationLogic.t) =
    let formula =
      match formula.node with
      | True -> Formula.True
      | False -> Formula.False
      | Exists(id, formula) -> Formula.Exists(id, (remove_annotation_in_formula formula))
      | And(left, right) -> 
        let left = remove_annotation_in_formula left in
        let right = remove_annotation_in_formula right in
        Formula.And(left, right)
      | AndSeparately(left, right) -> 
        let left = remove_annotation_in_formula left in
        let right = remove_annotation_in_formula right in
        Formula.AndSeparately(left, right)
      | Or(left, right) ->
        let left = remove_annotation_in_formula left in
        let right = remove_annotation_in_formula right in
        Formula.Or(left, right)
      | Comparison(op, lexpr, rexpr) ->
        let lexpr = remove_annotation_in_expr lexpr in
        let rexpr = remove_annotation_in_expr rexpr in
        Formula.Comparison(op, lexpr, rexpr)
      | EmptyHeap -> Formula.EmptyHeap
      | NonAllocated(id) -> Formula.NonAllocated(id)
      | Allocation(id, expr) -> Formula.Allocation(id, (remove_annotation_in_expr expr))
    in
    annotate formula ()
  and remove_annotation_in_expr expr =
    let expr =
      match expr.node with
      | Literal(value) -> ArithmeticExpression.Literal(value)
      | Variable(id) -> ArithmeticExpression.Variable(id)
      | Operation(op, lexpr, rexpr) ->
        let lexpr = remove_annotation_in_expr lexpr in
        let rexpr = remove_annotation_in_expr rexpr in
        ArithmeticExpression.Operation(op, lexpr, rexpr)
    in
    annotate expr ()
  in
  remove_annotation_in_formula formula

(* https://stackoverflow.com/a/10893700 *)
let list_cartesian l l' =
  List.concat (List.map (fun e -> List.map (fun e' -> (e,e')) l') l)
    
let get_variable_from_expression (expr: 'a ArithmeticExpression.t) =
  match expr.node with
  | Variable(id) -> Some(id)
  | _ -> None
    
let rec get_expr_identifiers (expr: 'a ArithmeticExpression.t) =
  match expr.node with
  | Literal(_) -> (IdentifierSet.empty)
  | Variable(id) -> (IdentifierSet.singleton id)
  | Operation(_, lexpr, rexpr) ->
    let l_ids = get_expr_identifiers lexpr in
    let r_ids = get_expr_identifiers rexpr in
    IdentifierSet.union l_ids r_ids

let rec get_formula_identifiers (formula: 'a Formula.t) =
  match formula.node with
  | True | False | EmptyHeap ->
    IdentifierSet.empty
  | Allocation(id, _) | NonAllocated(id) ->
    IdentifierSet.singleton id
  | Comparison(_, lexpr, rexpr) ->
    IdentifierSet.union (get_expr_identifiers lexpr) (get_expr_identifiers rexpr)
  | And(lformula, rformula) | AndSeparately(lformula, rformula) ->
    IdentifierSet.union (get_formula_identifiers lformula) (get_formula_identifiers rformula)
  | Exists(_, _) ->
    raise (Failure "Formulas of existential abstraction cannot be contained in normal form disjoints")
  | Or(_, _) ->
    raise (Failure "Disjunction of formulas cannot be contained in normal form disjoints")