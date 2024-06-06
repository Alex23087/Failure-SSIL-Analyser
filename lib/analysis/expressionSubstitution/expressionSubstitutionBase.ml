open DataStructures
open DataStructures.Analysis
open NormalForm
open Analysis_Utils

let substitute_expression_in_normalized_formula (formula: NormalForm.t) (changing_expr: ArithmeticExpression.t) (changed_id: identifier) =
  let rec substitute_expression_in_expression (expr: ArithmeticExpression.t) (changing_expr: ArithmeticExpression.t) (changed_id: identifier) =
    match expr with
    | Literal(_) -> expr
    | Variable(id) ->
      if id = changed_id then
        changing_expr
      else
        expr
    | Operation(op, lexpr, rexpr) ->
      let lexpr = substitute_expression_in_expression lexpr changing_expr changed_id in
      let rexpr = substitute_expression_in_expression rexpr changing_expr changed_id in
      ArithmeticExpression.Operation(op, lexpr, rexpr)
  in
  let rec substitute_expression_in_formula (formula: Formula.t) (changing_expr: ArithmeticExpression.t) (changed_id: identifier) (renamed_id: identifier) =
    match formula with
    | True | False | EmptyHeap ->
      formula, false
    | Allocation(id, expr) ->
      let expr = substitute_expression_in_expression expr changing_expr changed_id in
      if id = changed_id then
        match get_variable_from_normal_form_expr changing_expr with
        | Some(changing_id) -> Formula.Allocation(changing_id, expr), false
        | None  -> Formula.Allocation(renamed_id, expr), true
      else
        Formula.Allocation(id, expr), false
    | NonAllocated(id) ->
      if id = changed_id then
        match get_variable_from_normal_form_expr changing_expr with
        | Some(changing_id) -> Formula.NonAllocated(changing_id), false
        | None  -> Formula.NonAllocated(renamed_id), true
      else
        formula, false
    | Comparison(op, lexpr, rexpr) ->
      let lexpr = substitute_expression_in_expression lexpr changing_expr changed_id in
      let rexpr = substitute_expression_in_expression rexpr changing_expr changed_id in
      Formula.Comparison(op, lexpr, rexpr), false
    | And(lformula, rformula) ->
      let lformula, lrenamed = substitute_expression_in_formula lformula changing_expr changed_id renamed_id in
      let rformula, rrenamed = substitute_expression_in_formula rformula changing_expr changed_id renamed_id in
      Formula.And(lformula, rformula), lrenamed || rrenamed
    | AndSeparately(lformula, rformula) ->
      let lformula, lrenamed = substitute_expression_in_formula lformula changing_expr changed_id renamed_id in
      let rformula, rrenamed = substitute_expression_in_formula rformula changing_expr changed_id renamed_id in
      Formula.AndSeparately(lformula, rformula), lrenamed || rrenamed
  in
  let original_phantom_id = formula.last_phantom_id in
  let renamed_var, renamed_phantom_id = new_variable_name changed_id original_phantom_id in
  let id_expr = ArithmeticExpression.Variable(renamed_var) in
  let renamed_equal_formula = Formula.Comparison(BinaryComparison.Equals, id_expr, changing_expr) in
  let substitute_in_disjoint (formula: Formula.t) (disjoints: Formula.t list) (accum_renamed_used: bool) =
    let formula, renamed_used = substitute_expression_in_formula formula changing_expr changed_id renamed_var in
    let formula = if renamed_used then
      Formula.And(formula, renamed_equal_formula)
    else
      formula
    in
    (formula :: disjoints), renamed_used || accum_renamed_used
  in
  let disjoints, renamed_used = List.fold_left (fun (disjoints, renamed_used) x -> substitute_in_disjoint x disjoints renamed_used) ([], false) formula.disjoints in
  let phantom_id = if renamed_used then renamed_phantom_id else original_phantom_id in
  NormalForm.make formula.variables disjoints phantom_id