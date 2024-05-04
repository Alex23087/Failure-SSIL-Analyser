open Prelude
open Prelude.Ast.LogicFormulas

module IdentifierSet = struct include Ast.IdentifierSet end
type annotation = Ast.logic_formulas_annotation
type identifier = Ast.identifier

(* https://stackoverflow.com/a/10893700 *)
let list_cartesian l l' = 
  List.concat (List.map (fun e -> List.map (fun e' -> (e,e')) l') l)

let rename_variable_in_set (variables: IdentifierSet.t) (var: identifier) (new_name: identifier) =
  match IdentifierSet.find_opt var variables with
  | Some(_) -> IdentifierSet.add new_name (IdentifierSet.remove var variables)
  | None -> variables

let rec rename_variable_in_formula (disjoint: Formula.t) (var: identifier) (new_name: identifier) =
  let rename_variable_name (var: identifier) (old_name: identifier) (new_name: identifier) =
    if var = old_name then new_name else var
  in
  let rec rename_variable_in_expression (expr: ArithmeticExpression.t) (var: identifier) (new_name: identifier) =
    match expr.node with
    | Literal(_) -> expr
    | Variable(id) -> update_formula expr (ArithmeticExpression.Variable(rename_variable_name id var new_name))
    | Operation(op, lexpr, rexpr) -> 
      let lexpr = rename_variable_in_expression lexpr var new_name in
      let rexpr = rename_variable_in_expression rexpr var new_name in
      update_formula disjoint (ArithmeticExpression.Operation(op, lexpr, rexpr))
  in

  match disjoint.node with
  | True | False | EmptyHeap ->
    disjoint
  | NonAllocated(id) ->
    update_formula disjoint (Formula.NonAllocated(rename_variable_name id var new_name))
  | Exists(_, _) ->
    raise (Failure "Formulas of existential abstraction cannot be contained in normal form disjoints")
  | And(lformula, rformula) -> 
    let lformula = rename_variable_in_formula lformula var new_name in
    let rformula = rename_variable_in_formula rformula var new_name in
    update_formula disjoint (Formula.And(lformula, rformula))
  | Or(_, _) ->
    raise (Failure "Disjunction of formulas cannot be contained in normal form disjoints")
  | Comparison(op, lexpr, rexpr) ->
    let lexpr = rename_variable_in_expression lexpr var new_name in
    let rexpr = rename_variable_in_expression rexpr var new_name in
    update_formula disjoint (Formula.Comparison(op, lexpr, rexpr))
  | Allocation(id, expr) -> 
    let id = rename_variable_name id var new_name in
    let expr = rename_variable_in_expression expr var new_name in
    update_formula disjoint (Formula.Allocation(id, expr))
  | AndSeparately(lformula, rformula) ->
    let lformula = rename_variable_in_formula lformula var new_name in
    let rformula = rename_variable_in_formula rformula var new_name in
    update_formula disjoint (Formula.AndSeparately(lformula, rformula))