open DataStructures
open DataStructures.Analysis
open NormalForm
open Analysis_Utils

let rec remove_annotation_in_formula (formula: 'a Ast.AnnotationLogic.t) : Formula.t =
  match formula.node with
  | True -> Formula.True
  | False -> Formula.False
  | And(left, right) -> 
    let left = remove_annotation_in_formula left in
    let right = remove_annotation_in_formula right in
    Formula.And(left, right)
  | AndSeparately(left, right) -> 
    let left = remove_annotation_in_formula left in
    let right = remove_annotation_in_formula right in
    Formula.AndSeparately(left, right)
  | Comparison(op, lexpr, rexpr) ->
    let lexpr = remove_annotation_in_expr lexpr in
    let rexpr = remove_annotation_in_expr rexpr in
    Formula.Comparison(op, lexpr, rexpr)
  | EmptyHeap -> Formula.EmptyHeap
  | NonAllocated(id) -> Formula.NonAllocated(id)
  | Allocation(id, expr) -> Formula.Allocation(id, (remove_annotation_in_expr expr))
  | Exists(_, _) -> raise (Failure "Existentialization of identifiers do not appear in normalized formulas")
  | Or(_, _) -> raise (Failure "Disunctions of formulas do not appear in normalized formulas")
and remove_annotation_in_expr expr =
  match expr.node with
  | Literal(value) -> ArithmeticExpression.Literal(value)
  | Variable(id) -> ArithmeticExpression.Variable(id)
  | Operation(op, lexpr, rexpr) ->
    let lexpr = remove_annotation_in_expr lexpr in
    let rexpr = remove_annotation_in_expr rexpr in
    ArithmeticExpression.Operation(op, lexpr, rexpr)
    
let rename_common_free_variables (lformula: NormalForm.t) (rformula: NormalForm.t) (last_phantom_id: int) =
  let lformula_free, lformula_bound = normal_form_free_variables lformula, lformula.variables in
  let rformula_free, rformula_bound = normal_form_free_variables rformula, rformula.variables in

  (* rename the bound variables in lformula that are free in rformula *)
  let lformula_vars_to_rename = IdentifierSet.inter lformula_bound rformula_free in
  let (variables, disjoints, last_phantom_id) =
    IdentifierSet.fold (fun elem (variables, disjoints, phantom_id) -> rename_variable_in_disjoints elem variables disjoints phantom_id)
    lformula_vars_to_rename (lformula.variables, lformula.disjoints, last_phantom_id)
  in
  let lformula = NormalForm.make variables disjoints last_phantom_id in

  (* rename the bound variables in rformula that are free in lformula *)
  let rformula_vars_to_rename = IdentifierSet.inter rformula_bound lformula_free in
  let (variables, disjoints, last_phantom_id) =
    IdentifierSet.fold (fun elem (variables, disjoints, phantom_id) -> rename_variable_in_disjoints elem variables disjoints phantom_id)
    rformula_vars_to_rename (rformula.variables, rformula.disjoints, last_phantom_id)
  in
  let rformula = NormalForm.make variables disjoints last_phantom_id in

  (* rename the common bound variables only in the rformulas (it would have been indifferent if were renamed them in lformulas) *)
  let common_vars_to_rename = IdentifierSet.inter lformula_bound rformula_bound in
  let (variables, disjoints, last_phantom_id) =
    IdentifierSet.fold (fun elem (variables, disjoints, phantom_id) -> rename_variable_in_disjoints elem variables disjoints phantom_id)
    common_vars_to_rename (rformula.variables, rformula.disjoints, last_phantom_id)
  in
  let rformula = NormalForm.make variables disjoints last_phantom_id in
  (lformula, rformula, last_phantom_id)

let compute_last_phantom_id (lformula: NormalForm.t) (rformula: NormalForm.t): int =
  if lformula.last_phantom_id > rformula.last_phantom_id then
    lformula.last_phantom_id
  else
    rformula.last_phantom_id

let merge_two_formulas (lformula: NormalForm.t) (rformula: NormalForm.t) (last_phantom_id: int) make_disjoints =
  let (lformula, rformula, last_phantom_id) = rename_common_free_variables lformula rformula last_phantom_id in
  let bound_variables = IdentifierSet.union lformula.variables rformula.variables in
  let disjoints = make_disjoints lformula rformula in
  NormalForm.make bound_variables disjoints last_phantom_id