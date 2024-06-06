include RenameVariable
include NormalFormUtils
include CommandToLogicConversion

open DataStructures
open Parser
open LogicFormulas

open Ast.AnnotationLogic

let annotate = DataStructures.AnnotatedNode.make
let remove_annotation (formula: 'a Ast.AnnotationLogic.t) : DataStructures.Analysis.NormalForm.Formulas.t =
  let rec remove_annotation_in_formula (formula: 'a Ast.AnnotationLogic.t) =
    let formula =
      match formula.node with
      | True -> Formula.True
      | False -> Formula.False
      | Exists(id, expr) -> raise (Failure "")
      | And(left, right) -> raise (Failure "")
      | AndSeparately(left, right) -> raise (Failure "")
      | Or(left, right) -> raise (Failure "")
      | Comparison(op, lexpr, rexpr) -> raise (Failure "")
      | EmptyHeap -> raise (Failure "")
      | NonAllocated(id) -> (Formula.NonAllocated id)
      | Allocation(id, expr) -> raise (Failure "")
    in
    annotate formula ()
  and remove_annotation_in_bexpr expr =
    raise (Failure "")
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