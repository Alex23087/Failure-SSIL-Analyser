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
    let bound_identifiers_to_string (vars: IdentifierSet.t) = 
      IdentifierSet.fold (fun x acc -> acc ^ "exists " ^ x ^ ".") vars ""
    in
    let bound_identifiers_better_names (formula: NormalForm.t) =
      let new_id_name last_name =
        if last_name.[0] = 'z' then
          "a" ^ last_name
        else
          let last_part = String.sub last_name 1 (String.length last_name - 1) in
          let ch = last_name.[0] |> Char.code |> ((+) 1) |> Char.chr |> (String.make 1) in
          ch ^ last_part
      in
      let rec new_id_name_in_vars old_id last_name (vars: IdentifierSet.t) =
        let new_name = new_id_name last_name in
        if IdentifierSet.find_opt last_name vars |> Option.is_none then
          new_name, vars |> IdentifierSet.remove old_id |> IdentifierSet.add last_name
        else
          new_id_name_in_vars old_id new_name vars
      in

      IdentifierSet.fold (fun id (name, formula) ->
        let next_name, variables = new_id_name_in_vars id name formula.variables in
        let disjoints = List.map (fun x -> RenameVariable.rename_variable_in_formula x id name) formula.disjoints in
        next_name, NormalForm.make variables disjoints formula.id_generator
      ) formula.variables ("a", formula) |> snd
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

    let formula = bound_identifiers_better_names formula in

    let disjoints_print = join_list formula.disjoints (fun x -> "(" ^ disjoint_to_string x ^ ")") "||" in
    bound_identifiers_to_string formula.variables ^ disjoints_print
end