open DataStructures.Analysis
open NormalForm
open Submodules

let simplify_arithmetic_expressions (formula: NormalForm.t) =
  let to_symalg_operator (op: BinaryOperator.t) =
    match op with
    | Plus -> SymAlg.Plus
    | Minus -> SymAlg.Minus
    | Times -> SymAlg.Times
    | Division -> SymAlg.Div
    | Modulo -> raise (Failure "SymAlg does not support the modulo operator")
  in
  let rec to_symalg_expression (expr: ArithmeticExpression.t) =
    match expr with
    | Literal(value) ->
      SymAlg.Leaf(SymAlg.Poly([ (float_of_int value, []) ]))
    | Variable(id) ->
      SymAlg.Leaf(SymAlg.Poly([ (1., [(id, 1.)]) ]))
    | Operation(op, lexpr, rexpr) -> (
      let op = to_symalg_operator op in
      let lexpr = to_symalg_expression lexpr in
      let rexpr = to_symalg_expression rexpr in
      SymAlg.Node(op, [lexpr; rexpr])
    )
  in

  let from_symalg_operator (op: SymAlg.operator) =
    match op with
    | Plus -> BinaryOperator.Plus
    | Minus -> BinaryOperator.Minus
    | Times -> BinaryOperator.Times
    | Div -> BinaryOperator.Division
    | Power -> raise (Failure "Our logic does not support the power operator")
  in
  let rec from_symalg_power ((var, power): SymAlg.power) =
    let power = int_of_float power in
    if power = 0 then
      ArithmeticExpression.Literal(1)
    else if power = 1 then
      ArithmeticExpression.Variable(var)
    else if power > 1 then
      ArithmeticExpression.Operation(
        BinaryOperator.Times,
        ArithmeticExpression.Variable(var),
        from_symalg_power (var, float_of_int (power - 1))
      )
    else (* power < 0 *)
      ArithmeticExpression.Operation(
        BinaryOperator.Division,
        ArithmeticExpression.Literal(1),
        from_symalg_power (var, float_of_int (-power))
      )
  in
  let rec from_symalg_power_list (power_list: SymAlg.power list) =
    match power_list with
    | [] -> ArithmeticExpression.Literal(1)
    | [x] -> from_symalg_power x
    | x::xs -> 
      ArithmeticExpression.Operation(
        BinaryOperator.Times,
        from_symalg_power x,
        from_symalg_power_list xs
      )
  in
  let from_symalg_mono ((mult, power_list): SymAlg.monomial) =
    let mult = int_of_float mult in
    let mult_literal = ArithmeticExpression.Literal(mult) in
    if mult = 0 then
      mult_literal
    else if mult = 1 then
      from_symalg_power_list power_list
    else if List.length power_list = 0 then
      mult_literal
    else
      ArithmeticExpression.Operation(
        BinaryOperator.Times,
        mult_literal,
        from_symalg_power_list power_list
      )
  in
  let rec from_symalg_poly (poly: SymAlg.polynomial) =
    match poly with
    | [] -> ArithmeticExpression.Literal(0)
    | [x] -> from_symalg_mono x
    | x::xs -> 
      ArithmeticExpression.Operation(
        BinaryOperator.Plus,
        from_symalg_mono x,
        from_symalg_poly xs
      )
  in
  let from_symalg_frac ((numerator, denominator): SymAlg.frac) =
    ArithmeticExpression.Operation(
      BinaryOperator.Division,
      from_symalg_poly numerator,
      from_symalg_poly denominator
    )
  in
  let from_symalg_term (term: SymAlg.term) =
    match term with
    | Poly(poly) -> from_symalg_poly poly
    | Frac(frac) -> from_symalg_frac frac
    | Exp(_) -> raise (Failure "Our logic does not support exponentiation")
  in
  let rec from_symalg_expression (expr: SymAlg.expr) =
    match expr with
    | Node(op, exprs) -> (
      match exprs with
      | [] -> (
        match op with
        | Plus | Minus -> ArithmeticExpression.Literal(0)
        | Times | Div -> ArithmeticExpression.Literal(1)
        | Power -> raise (Failure "Our logic does not support exponentiation")
      )
      | [x] -> from_symalg_expression x
      | x::xs -> 
        ArithmeticExpression.Operation(
          from_symalg_operator op,
          from_symalg_expression x,
          from_symalg_expression (SymAlg.Node(op, xs))
        )
    )
    | Leaf(term) -> from_symalg_term term
  in

  let simplify_expression (expr: ArithmeticExpression.t) =
    expr |> to_symalg_expression
         |> SymAlg.simplify_expr
         |> from_symalg_expression
  in
  let rec simplify_formula (conjunct: Formula.t) =
    match conjunct with
    | Comparison(op, lexpr, rexpr) ->
      Formula.Comparison(op, simplify_expression lexpr, simplify_expression rexpr)
    | Allocation(id, expr) ->
      Formula.Allocation(id, simplify_expression expr)
    | And(lformula, rformula) ->
      Formula.And(simplify_formula lformula, simplify_formula rformula)
    | AndSeparately(lformula, rformula) ->
      Formula.AndSeparately(simplify_formula lformula, simplify_formula rformula)
    | _ -> conjunct
  in

  let disjoints = List.map simplify_formula formula.disjoints in
  NormalForm.make formula.variables disjoints formula.id_generator

let f = simplify_arithmetic_expressions