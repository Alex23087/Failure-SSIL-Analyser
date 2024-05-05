open Prelude.Ast
open LogicFormulas

let update_formula = AnnotatedNode.update_node
let annotate = AnnotatedNode.make

(* https://stackoverflow.com/a/10893700 *)
let list_cartesian l l' =
  List.concat (List.map (fun e -> List.map (fun e' -> (e,e')) l') l)

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
    
let get_variable_from_expression (expr: 'a ArithmeticExpression.t) =
  match expr.node with
  | Variable(id) -> Some(id)
  | _ -> None

let command_annotation_to_logic_annotation (annotation: Commands.annotation) : annotation =
  {position=annotation.position}

let rec command_expression_to_logic_expression (expr: 'a Commands.ArithmeticExpression.t) =
  let command_boperator_to_logic_boperator (op: Commands.ArithmeticOperation.t) =
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

let rec command_bexpression_to_logic_formula (expr: 'a Commands.BooleanExpression.t) =
  let command_bcomparison_to_logic_bcomparison (op: Commands.BooleanComparison.t) =
    match op with
    | Equal -> BinaryComparison.Equals
    | NotEqual -> BinaryComparison.NotEquals
    | LessThan -> BinaryComparison.LessThan
    | LessOrEqual -> BinaryComparison.LessOrEqual
    | GreaterThan -> BinaryComparison.GreaterThan
    | GreaterOrEqual -> BinaryComparison.GreaterOrEqual
  in
  let command_bcomparison_to_negated_logic_bcomparison (op: Commands.BooleanComparison.t) =
    match op with
    | Equal -> BinaryComparison.Equals
    | NotEqual -> BinaryComparison.NotEquals
    | LessThan -> BinaryComparison.GreaterOrEqual
    | LessOrEqual -> BinaryComparison.GreaterThan
    | GreaterThan -> BinaryComparison.LessOrEqual
    | GreaterOrEqual -> BinaryComparison.LessThan
  in
  let negated_command_bexpression (expr: 'a Commands.BooleanExpression.t) =
    annotate (Commands.BooleanExpression.Not(expr)) expr.annotation
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