include RenameVariable
include NormalFormUtils
include CommandToLogicConversion

open DataStructures
open DataStructures.Analysis

let annotate = DataStructures.AnnotatedNode.make
let rec remove_annotation_in_formula (formula: 'a Ast.AnnotationLogic.t) : NormalForm.Formula.t =
  match formula.node with
  | True -> NormalForm.Formula.True
  | False -> NormalForm.Formula.False
  | And(left, right) -> 
    let left = remove_annotation_in_formula left in
    let right = remove_annotation_in_formula right in
    NormalForm.Formula.And(left, right)
  | AndSeparately(left, right) -> 
    let left = remove_annotation_in_formula left in
    let right = remove_annotation_in_formula right in
    NormalForm.Formula.AndSeparately(left, right)
  | Comparison(op, lexpr, rexpr) ->
    let lexpr = remove_annotation_in_expr lexpr in
    let rexpr = remove_annotation_in_expr rexpr in
    NormalForm.Formula.Comparison(op, lexpr, rexpr)
  | EmptyHeap -> NormalForm.Formula.EmptyHeap
  | NonAllocated(id) -> NormalForm.Formula.NonAllocated(id)
  | Allocation(id, expr) -> NormalForm.Formula.Allocation(id, (remove_annotation_in_expr expr))
  | Exists(_, _) -> raise (Failure "Existentialization of identifiers do not appear in normalized formulas")
  | Or(_, _) -> raise (Failure "Disunctions of formulas do not appear in normalized formulas")
and remove_annotation_in_expr expr =
  match expr.node with
  | Literal(value) -> NormalForm.ArithmeticExpression.Literal(value)
  | Variable(id) -> NormalForm.ArithmeticExpression.Variable(id)
  | Operation(op, lexpr, rexpr) ->
    let lexpr = remove_annotation_in_expr lexpr in
    let rexpr = remove_annotation_in_expr rexpr in
    NormalForm.ArithmeticExpression.Operation(op, lexpr, rexpr)

(* https://stackoverflow.com/a/10893700 *)
let list_cartesian l l' =
  List.concat (List.map (fun e -> List.map (fun e' -> (e,e')) l') l)
    
let get_variable_from_expression (expr: 'a Ast.AnnotationLogic.ArithmeticExpression.t) =
  match expr.node with
  | Variable(id) -> Some(id)
  | _ -> None
    
let rec get_expr_identifiers (expr: 'a Ast.AnnotationLogic.ArithmeticExpression.t) =
  match expr.node with
  | Literal(_) -> (IdentifierSet.empty)
  | Variable(id) -> (IdentifierSet.singleton id)
  | Operation(_, lexpr, rexpr) ->
    let l_ids = get_expr_identifiers lexpr in
    let r_ids = get_expr_identifiers rexpr in
    IdentifierSet.union l_ids r_ids

let rec get_formula_identifiers (formula: 'a Ast.AnnotationLogic.Formula.t) =
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

let get_variable_from_normal_form_expr (expr: NormalForm.ArithmeticExpression.t) =
  match expr with
  | Variable(id) -> Some(id)
  | _ -> None
    
let rec get_normal_form_expr_identifiers (expr: NormalForm.ArithmeticExpression.t) =
  match expr with
  | Literal(_) -> (IdentifierSet.empty)
  | Variable(id) -> (IdentifierSet.singleton id)
  | Operation(_, lexpr, rexpr) ->
    let l_ids = get_normal_form_expr_identifiers lexpr in
    let r_ids = get_normal_form_expr_identifiers rexpr in
    IdentifierSet.union l_ids r_ids

let rec get_normal_form_disjoint_identifiers (formula: NormalForm.Formula.t) =
  match formula with
  | True | False | EmptyHeap ->
    IdentifierSet.empty
  | Allocation(id, _) | NonAllocated(id) ->
    IdentifierSet.singleton id
  | Comparison(_, lexpr, rexpr) ->
    IdentifierSet.union (get_normal_form_expr_identifiers lexpr) (get_normal_form_expr_identifiers rexpr)
  | And(lformula, rformula) | AndSeparately(lformula, rformula) ->
    IdentifierSet.union (get_normal_form_disjoint_identifiers lformula) (get_normal_form_disjoint_identifiers rformula)