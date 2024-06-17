module Parser = struct
  type atomic_list = DataStructures.Parser.Commands.atomic_t list [@@deriving show]
end

module Analysis = struct
  type commands_list = DataStructures.Analysis.Commands.t list [@@deriving show]

  open DataStructures
  open DataStructures.Analysis
  open NormalForm

  let pretty_print_normal_form (formula: NormalForm.t) =
    let join_list (formulas: Formula.t list) (f: Formula.t -> string) (sep: string) =
      let rec join_list (formulas: Formula.t list) (f: Formula.t -> string) (sep: string) =
        match formulas with
        | [] -> ""
        | [x] -> f x
        | x::xs -> f x ^ sep ^ join_list xs f sep
      in
      let sep = " " ^ sep ^ " " in
      join_list formulas f sep
    in

    let bound_identifier_to_string(var: identifier) =
      "exists " ^ var ^ "."
    in
    let bound_identifiers_to_string (vars: IdentifierSet.t) = 
      IdentifierSet.fold (fun acc x -> acc ^ bound_identifier_to_string x) vars ""
    in

    let rec expand_conjuncts (formula: Formula.t) =
      match formula with
      | And(lformula, rformula) -> expand_conjuncts lformula @ expand_conjuncts rformula
      | _ -> [formula]
    in
    let expand_separate_conjuncts (formula: Formula.t) =
      match formula with
      | AndSeparately(lformula, rformula) -> expand_conjuncts lformula @ expand_conjuncts rformula
      | _ -> [formula]
    in
    let comparison_op_to_string (op: BinaryComparison.t) =
      match op with
      | Equals -> "="
      | NotEquals -> "!="
      | LessThan -> "<"
      | LessOrEqual -> "<="
      | GreaterThan -> ">"
      | GreaterOrEqual -> ">="
    in
    let binary_op_to_string (op: BinaryOperator.t) =
      match op with
      | Plus -> "+"
      | Minus -> "-"
      | Times -> "*"
      | Division -> "/"
      | Modulo -> "%"
    in
    let rec expression_to_string (expr: ArithmeticExpression.t) =
      match expr with
      | Literal(value) -> string_of_int value
      | Variable(id) -> id
      | Operation(op, lexpr, rexpr) -> 
        let lexpr = match lexpr with
          | Operation(_) -> "(" ^ expression_to_string lexpr ^ ")"
          | _ -> expression_to_string lexpr
        in
        let rexpr = match rexpr with
          | Operation(_) -> "(" ^ expression_to_string rexpr ^ ")"
          | _ -> expression_to_string rexpr
        in
        lexpr ^ " " ^ binary_op_to_string op ^ " " ^ rexpr
    in
    let rec disjoint_to_string (formula: Formula.t) =
      match formula with
      | True -> "true"
      | False -> "false"
      | EmptyHeap -> "emp"
      | Allocation(id, expr) -> id ^ " -> " ^ expression_to_string expr
      | NonAllocated(id) -> id ^ " -/>"
      | Comparison(op, lexpr, rexpr) ->
        let lexpr = match lexpr with
          | Operation(_) -> "(" ^ expression_to_string lexpr ^ ")"
          | _ -> expression_to_string lexpr
        in
        let rexpr = match rexpr with
          | Operation(_) -> "(" ^ expression_to_string rexpr ^ ")"
          | _ -> expression_to_string rexpr
        in
        lexpr ^ " " ^ comparison_op_to_string op ^ " " ^ rexpr
      | And(_) -> expand_conjuncts formula |> (fun x -> join_list x conjunct_to_string "&&")
      | AndSeparately(_) -> expand_separate_conjuncts formula |> (fun x -> join_list x conjunct_to_string "*")
    and conjunct_to_string (conjunct: Formula.t) =
      match conjunct with
      | AndSeparately(_)
      | And(_) -> "( " ^ disjoint_to_string conjunct ^ ")"
      | _ -> disjoint_to_string conjunct
    in

    let disjoints_print = join_list formula.disjoints (fun x -> "(" ^ disjoint_to_string x ^ ")") "||" in
    bound_identifiers_to_string formula.variables ^ disjoints_print
end