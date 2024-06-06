open DataStructures
open DataStructures.Parser
open Ast.AnnotationLogic

let update_formula = AnnotatedNode.update_node

let new_variable_name (old_var: identifier) (phantom_id: int) =
  let substr = String.split_on_char '$' old_var in
  if List.length substr > 2 then
    raise (Failure "Found more than two $ characters in a variable name")
  else if List.length substr = 2 then
    let var_name = List.nth substr 1 in
    ((string_of_int phantom_id) ^ "$" ^ var_name, phantom_id + 1)
  else
    let var_name = List.hd substr in
    ((string_of_int phantom_id) ^ "$" ^ var_name, phantom_id + 1)

let rename_variable_in_set (variables: IdentifierSet.t) (var: identifier) (new_name: identifier) =
  match IdentifierSet.find_opt var variables with
  | Some(_) -> IdentifierSet.add new_name (IdentifierSet.remove var variables)
  | None -> variables

let rec rename_variable_in_formula (disjoint: 'a Formula.t) (var: identifier) (new_name: identifier) =
  let rename_variable_name (var: identifier) (old_name: identifier) (new_name: identifier) =
    if var = old_name then new_name else var
  in
  let rec rename_variable_in_expression (expr: 'a ArithmeticExpression.t) (var: identifier) (new_name: identifier) =
    match expr.node with
    | Literal(_) -> expr
    | Variable(id) -> update_formula expr (ArithmeticExpression.Variable(rename_variable_name id var new_name))
    | Operation(op, lexpr, rexpr) ->
      let lexpr = rename_variable_in_expression lexpr var new_name in
      let rexpr = rename_variable_in_expression rexpr var new_name in
      update_formula expr (ArithmeticExpression.Operation(op, lexpr, rexpr))
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

let rename_variable_in_disjoints (var: identifier) (variables: IdentifierSet.t) (disjoints: 'a Formula.t list) (phantom_id: int) =
  let (new_var, phantom_id) = new_variable_name var phantom_id in
  let variables = IdentifierSet.add new_var (IdentifierSet.remove var variables) in
  let disjoints = List.map (fun x -> rename_variable_in_formula x var new_var) disjoints in
  (variables, disjoints, phantom_id)