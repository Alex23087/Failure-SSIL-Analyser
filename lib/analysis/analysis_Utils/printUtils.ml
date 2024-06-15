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
      let rec join_list (acc: string) (formulas: Formula.t list) (f: Formula.t -> string) (sep: string) =
        match formulas with
        | [] -> ""
        | [x] -> f x
        | x::xs ->
          let new_acc = acc ^ f x ^ sep in
          join_list new_acc xs f sep
      in
      join_list "" formulas f (" " ^ sep ^ " ")
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
    let rec expand_separate_conjuncts (formula: Formula.t) =
      match formula with
      | AndSeparately(lformula, rformula) -> expand_conjuncts lformula @ expand_conjuncts rformula
      | _ -> [formula]
    in
    let rec expression_to_string (expr: ArithmeticExpression.t) =
      ""
    in
    let rec disjoint_to_string (formula: Formula.t) =
      match formula with
      | True -> "true"
      | False -> "false"
      | EmptyHeap -> "emp"
      | Allocation(id, expr) -> id ^ " -> " ^ expression_to_string expr
      | NonAllocated(id) -> id ^ " -/>"
      | Comparison(op, lexpr, rexpr) -> ""
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