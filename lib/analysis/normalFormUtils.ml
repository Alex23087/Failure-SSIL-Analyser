open Prelude
open Prelude.Ast.LogicFormulas
open Utils

let first_annotation (annot1: annotation) (annot2: annotation) =
  if annot1.position.line < annot2.position.line then
    annot1
  else if annot1.position.line > annot2.position.line then
    annot2
  else if annot1.position.column <= annot2.position.column then
    annot1
  else
    annot2
    
let rec expr_identifiers (expr: ArithmeticExpression.t) =
  match expr.node with
  | Literal(_) -> (IdentifierSet.empty)
  | Variable(id) -> (IdentifierSet.singleton id)
  | Operation(_, lexpr, rexpr) ->
    let l_ids = expr_identifiers lexpr in
    let r_ids = expr_identifiers rexpr in
    IdentifierSet.union l_ids r_ids

let rec formula_identifiers (formula: Formula.t) =
  match formula.node with
  | True | False | EmptyHeap ->
    IdentifierSet.empty
  | Allocation(id, _) | NonAllocated(id) ->
    IdentifierSet.singleton id
  | Comparison(_, lexpr, rexpr) ->
    IdentifierSet.union (expr_identifiers lexpr) (expr_identifiers rexpr)
  | And(lformula, rformula) | AndSeparately(lformula, rformula) ->
    IdentifierSet.union (formula_identifiers lformula) (formula_identifiers rformula)
  | Exists(_, _) ->
    raise (Failure "Formulas of existential abstraction cannot be contained in normal form disjoints")
  | Or(_, _) ->
    raise (Failure "Disjunction of formulas cannot be contained in normal form disjoints")
    
let rename_variable_in_disjoints (var: identifier) (variables: IdentifierSet.t) (disjoints: Formula.t list) (phantom_id: int) =
  let (new_var, phantom_id) = new_variable_name var phantom_id in
  let variables = IdentifierSet.add new_var (IdentifierSet.remove var variables) in
  let disjoints = List.map (fun x -> rename_variable_in_formula x var new_var) disjoints in
  (variables, disjoints, phantom_id)