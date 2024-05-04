open Prelude
open Prelude.Ast.LogicFormulas

module IdentifierSet = struct include Ast.IdentifierSet end
type annotation = Ast.logic_formulas_annotation
type identifier = Ast.identifier

(* https://stackoverflow.com/a/10893700 *)
let list_cartesian l l' =
  List.concat (List.map (fun e -> List.map (fun e' -> (e,e')) l') l)

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
    
let get_variable_from_expression (expr: ArithmeticExpression.t) =
  match expr.node with
  | Variable(id) -> Some(id)
  | _ -> None

let command_annotation_to_logic_annotation (annotation: Prelude.Ast.regular_formulas_annotation) : Prelude.Ast.logic_formulas_annotation =
  {position=annotation.position}

let rec command_expression_to_logic_expression (expr: Prelude.Ast.Commands.ArithmeticExpression.t) =
  let command_boperator_to_logic_boperator (op: Prelude.Ast.Commands.ArithmeticOperation.t) =
    match op with
    | Plus -> BinaryOperator.Plus
    | Minus -> BinaryOperator.Minus
    | Times -> BinaryOperator.Times
    | Division -> BinaryOperator.Division
    | Modulo -> BinaryOperator.Modulo
  in

  let annotation = command_annotation_to_logic_annotation expr.annotation in
  match expr.node with
  | Literal(value) ->
    annotate (ArithmeticExpression.Literal(value)) annotation
  | Variable(id) ->
    annotate (ArithmeticExpression.Variable(id)) annotation
  | BinaryOperation(op, lexpr, rexpr) ->
    let lexpr = command_expression_to_logic_expression lexpr in
    let rexpr = command_expression_to_logic_expression rexpr in
    let op = command_boperator_to_logic_boperator op in
    annotate (ArithmeticExpression.Operation(op, lexpr, rexpr)) annotation

let rec command_bexpression_to_logic_formula (expr: Prelude.Ast.Commands.BooleanExpression.t) =
  let command_bcomparison_to_logic_bcomparison (op: Prelude.Ast.Commands.BooleanComparison.t) =
    match op with
    | Equal -> BinaryComparison.Equals
    | NotEqual -> BinaryComparison.NotEquals
    | LessThan -> BinaryComparison.LessThan
    | LessOrEqual -> BinaryComparison.LessOrEqual
    | GreaterThan -> BinaryComparison.GreaterThan
    | GreaterOrEqual -> BinaryComparison.GreaterOrEqual
  in
  let command_bcomparison_to_negated_logic_bcomparison (op: Prelude.Ast.Commands.BooleanComparison.t) =
    match op with
    | Equal -> BinaryComparison.Equals
    | NotEqual -> BinaryComparison.NotEquals
    | LessThan -> BinaryComparison.GreaterOrEqual
    | LessOrEqual -> BinaryComparison.GreaterThan
    | GreaterThan -> BinaryComparison.LessOrEqual
    | GreaterOrEqual -> BinaryComparison.LessThan
  in
  let negated_command_bexpression (expr: Prelude.Ast.Commands.BooleanExpression.t) =
    Prelude.Ast.Commands.annotate (Prelude.Ast.Commands.BooleanExpression.Not(expr)) expr.annotation
  in

  let annotation = command_annotation_to_logic_annotation expr.annotation in
  match expr.node with
  | True ->
    annotate (Formula.True) annotation
  | False ->
    annotate (Formula.False) annotation
  (* Propagate the negation inwards *)
  | Not(subexpr) -> (
    match subexpr.node with
    | True ->
      annotate (Formula.False) annotation
    | False ->
      annotate (Formula.True) annotation
    | Not(_) ->
      command_bexpression_to_logic_formula expr
    | And(lexpr, rexpr) ->
      let lexpr = command_bexpression_to_logic_formula (negated_command_bexpression lexpr) in
      let rexpr = command_bexpression_to_logic_formula (negated_command_bexpression rexpr) in
      annotate (Formula.Or(lexpr, rexpr)) annotation
    | Or(lexpr, rexpr) ->
      let lexpr = command_bexpression_to_logic_formula (negated_command_bexpression lexpr) in
      let rexpr = command_bexpression_to_logic_formula (negated_command_bexpression rexpr) in
      annotate (Formula.And(lexpr, rexpr)) annotation
    | Comparison(op, lexpr, rexpr) ->
      let lexpr = command_expression_to_logic_expression lexpr in
      let rexpr = command_expression_to_logic_expression rexpr in
      let op = command_bcomparison_to_negated_logic_bcomparison op in
      annotate (Formula.Comparison(op, lexpr, rexpr)) annotation
  )
  | And(lexpr, rexpr) ->
    let lexpr = command_bexpression_to_logic_formula lexpr in
    let rexpr = command_bexpression_to_logic_formula rexpr in
    annotate (Formula.And(lexpr, rexpr)) annotation
  | Or(lexpr, rexpr) ->
    let lexpr = command_bexpression_to_logic_formula lexpr in
    let rexpr = command_bexpression_to_logic_formula rexpr in
    annotate (Formula.Or(lexpr, rexpr)) annotation
  | Comparison(op, lexpr, rexpr) ->
    let lexpr = command_expression_to_logic_expression lexpr in
    let rexpr = command_expression_to_logic_expression rexpr in
    let op = command_bcomparison_to_logic_bcomparison op in
    annotate (Formula.Comparison(op, lexpr, rexpr)) annotation