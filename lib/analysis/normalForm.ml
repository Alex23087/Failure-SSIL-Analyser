open Prelude
open Prelude.Ast.LogicFormulas
open Utils

module IdentifierSet = struct include Ast.IdentifierSet end
type annotation = Ast.logic_formulas_annotation
type identifier = Ast.identifier

type normal_form = {
  variables: IdentifierSet.t;
  disjoints: Formula.t list;
  annotation: annotation;
  last_phantom_id: int;
}

let make_normal_form variables disjoints annotation phantom_id =
  {variables; disjoints; annotation; last_phantom_id = phantom_id}

let rename_variable_in_normal_formula (formula: normal_form) (var: identifier) (new_name: identifier) =
  let variables = rename_variable_in_set formula.variables var new_name in
  let disjoints = List.map (function x -> rename_variable_in_formula x var new_name) formula.disjoints in
  make_normal_form variables disjoints formula.annotation formula.last_phantom_id

let rec existential_disjuntive_normal_form (formula: Formula.t) (last_phantom_id: int) =
  let new_variable_name (old_var: identifier) (phantom_id: int) =
    let substr = String.split_on_char '$' old_var in
    if List.length substr > 2 then
      raise (Failure "Found more than two $ characters in a variable name")
    else if List.length substr == 2 then
      let var_name = List.nth substr 1 in
      ((string_of_int phantom_id) ^ "$" ^ var_name, phantom_id + 1)
    else
      let var_name = List.hd substr in
      ((string_of_int phantom_id) ^ "$" ^ var_name, phantom_id + 1)
  in
  let first_annotation (annot1: annotation) (annot2: annotation) =
    if annot1.position.line < annot2.position.line then
      annot1
    else if annot1.position.line > annot2.position.line then
      annot2
    else if annot1.position.column <= annot2.position.column then
      annot1
    else
      annot2
  in
  let rec expr_identifiers (expr: ArithmeticExpression.t) =
    match expr.node with
    | Literal(_) -> (IdentifierSet.empty)
    | Variable(id) -> (IdentifierSet.singleton id)
    | Operation(_, lexpr, rexpr) -> 
      let l_ids = expr_identifiers lexpr in
      let r_ids = expr_identifiers rexpr in
      IdentifierSet.union l_ids r_ids
  in
  let rec formula_identifiers (formula: Formula.t) =
    match formula.node with
    | True | False | EmptyHeap ->
      IdentifierSet.empty
    | Allocation(id, _) | NonAllocated(id) ->
      IdentifierSet.singleton id
    | Comparison(_, lexpr, rexpr) -> 
      IdentifierSet.union (expr_identifiers lexpr) (expr_identifiers rexpr)
    | And(lformula, rformula) | AndSeparately(lformula, rformula) ->
      IdentifierSet.union (formula_identifiers lformula) (formula_identifiers rformula)      
    | Exists(_, _) ->
      raise (Failure "Formulas of existential abstraction cannot be contained in normal form disjoints")
    | Or(_, _) ->
      raise (Failure "Disjunction of formulas cannot be contained in normal form disjoints")
  in
  let normal_form_free_variables (formula: normal_form) =
    let ids = List.map formula_identifiers formula.disjoints in
    let ids = List.fold_left (fun acc ids -> IdentifierSet.union acc ids) IdentifierSet.empty ids in
    IdentifierSet.diff ids formula.variables
  in
  let rename_variable_in_disjoints (var: identifier) (variables: IdentifierSet.t) (disjoints: Formula.t list) (phantom_id: int) =
    let (new_var, phantom_id) = new_variable_name var phantom_id in
    let variables = IdentifierSet.add new_var (IdentifierSet.remove var variables) in
    let disjoints = List.map (fun x -> rename_variable_in_formula x var new_var) disjoints in
    (variables, disjoints, phantom_id)
  in
  let common_free_variables (lformula: normal_form) (rformula: normal_form) =
    let lformula_free = normal_form_free_variables lformula in
    let rformula_free = normal_form_free_variables rformula in
    IdentifierSet.inter lformula_free rformula_free
  in
  let rename_common_free_variables (lformula: normal_form) (rformula: normal_form) (last_phantom_id: int) =
    let lformula_free, lformula_bound = normal_form_free_variables lformula, lformula.variables in
    let rformula_free, rformula_bound = normal_form_free_variables rformula, rformula.variables in

    (* rename the free variables in lformula that are bound in rformula *)
    let lformula_vars_to_rename = IdentifierSet.inter lformula_free rformula_bound in
    let (variables, disjoints, last_phantom_id) =
      IdentifierSet.fold (fun elem (variables, disjoints, phantom_id) -> rename_variable_in_disjoints elem variables disjoints phantom_id)
      lformula_vars_to_rename (lformula.variables, lformula.disjoints, last_phantom_id)
    in
    let lformula = make_normal_form variables disjoints lformula.annotation last_phantom_id in
    
    (* rename the free variables in rformula that are bound in lformula *)
    let rformula_vars_to_rename = IdentifierSet.inter rformula_free lformula_bound in
    let (variables, disjoints, last_phantom_id) =
      IdentifierSet.fold (fun elem (variables, disjoints, phantom_id) -> rename_variable_in_disjoints elem variables disjoints phantom_id)
      rformula_vars_to_rename (rformula.variables, rformula.disjoints, last_phantom_id)
    in
    let rformula = make_normal_form variables disjoints rformula.annotation last_phantom_id in

    (* rename the common free variables only in the rformulas (it would have been indifferent if were renamed them in lformulas) *)
    let common_vars_to_rename = IdentifierSet.inter lformula_free rformula_free in 
    let (variables, disjoints, last_phantom_id) =
      IdentifierSet.fold (fun elem (variables, disjoints, phantom_id) -> rename_variable_in_disjoints elem variables disjoints phantom_id)
      common_vars_to_rename (rformula.variables, rformula.disjoints, last_phantom_id)
    in
    let rformula = make_normal_form variables disjoints rformula.annotation last_phantom_id in
    (lformula, rformula, last_phantom_id)
  in

  let handle_and (lformula: normal_form) (rformula: normal_form) (last_phantom_id: int) =
    let (lformula, rformula, last_phantom_id) = rename_common_free_variables lformula rformula last_phantom_id in
    let free_variables = common_free_variables lformula rformula in
    let annotation = first_annotation lformula.annotation rformula.annotation in
    let cartesian = list_cartesian lformula.disjoints rformula.disjoints in
    let disjoints = List.map (fun (l, r) -> annotate (Formula.And(l, r)) annotation) cartesian in
    make_normal_form free_variables disjoints annotation last_phantom_id
  in
  let handle_and_separately (lformula: normal_form) (rformula: normal_form) (last_phantom_id: int) =
    let (lformula, rformula, last_phantom_id) = rename_common_free_variables lformula rformula last_phantom_id in
    let free_variables = common_free_variables lformula rformula in
    let annotation = first_annotation lformula.annotation rformula.annotation in
    let cartesian = list_cartesian lformula.disjoints rformula.disjoints in
    let disjoints = List.map (fun (l, r) -> annotate (Formula.AndSeparately(l, r)) annotation) cartesian in
    make_normal_form free_variables disjoints annotation last_phantom_id
  in
  let handle_exist (exist_id: identifier) (subformula: normal_form) (exist_annot: annotation) =
    let variables = subformula.variables in
    let disjoints = subformula.disjoints in
    let annotation = first_annotation exist_annot subformula.annotation in
    let phantom_id = subformula.last_phantom_id in
    
    match IdentifierSet.find_opt exist_id subformula.variables with
    | Some(_) ->
      (* if the given identifier has already been existentialized, we don't add
         it to the free variables as it cannot occur in the subformulas *)
      make_normal_form variables disjoints annotation phantom_id
    | None ->
      (* if the existentialized variable does not occur in the subformula disjoints, we don't need to add it *)
      let free_variables = normal_form_free_variables subformula in 
      match IdentifierSet.find_opt exist_id free_variables with
      | Some(_) -> make_normal_form (IdentifierSet.add exist_id variables) disjoints annotation phantom_id
      | None -> make_normal_form variables disjoints annotation phantom_id
  in
  let handle_or (lformula: normal_form) (rformula: normal_form) (last_phantom_id: int) =
    let (lformula, rformula, last_phantom_id) = rename_common_free_variables lformula rformula last_phantom_id in
    let free_variables = common_free_variables lformula rformula in
    let annotation = first_annotation lformula.annotation rformula.annotation in
    make_normal_form free_variables (lformula.disjoints @ rformula.disjoints) annotation last_phantom_id
  in

  match formula.node with
  | True | False | EmptyHeap | NonAllocated(_)
  | Comparison(_, _, _) | Allocation(_, _) ->
    make_normal_form IdentifierSet.empty [formula] formula.annotation last_phantom_id
  | And(lformula, rformula) ->
    (* normalize recursively *)
    let lformula = existential_disjuntive_normal_form lformula last_phantom_id in
    let rformula = existential_disjuntive_normal_form rformula lformula.last_phantom_id in
    handle_and lformula rformula rformula.last_phantom_id
  | AndSeparately(lformula, rformula) ->
    (* normalize recursively *)
    let lformula = existential_disjuntive_normal_form lformula last_phantom_id in
    let rformula = existential_disjuntive_normal_form rformula lformula.last_phantom_id in
    handle_and_separately lformula rformula rformula.last_phantom_id
  | Exists(id, subformula) ->
    (* normalize recursively *)
    let subformula = existential_disjuntive_normal_form subformula last_phantom_id in
    handle_exist id subformula formula.annotation
  | Or(lformula, rformula) ->
    (* normalize recursively *)
    let lformula = existential_disjuntive_normal_form lformula last_phantom_id in
    let rformula = existential_disjuntive_normal_form rformula lformula.last_phantom_id in
    handle_or lformula rformula rformula.last_phantom_id