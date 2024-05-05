open Prelude.Ast
open Utils

let first_annotation (annot1: LogicFormulas.annotation) (annot2: LogicFormulas.annotation) =
  if annot1.position.line < annot2.position.line then
    annot1
  else if annot1.position.line > annot2.position.line then
    annot2
  else if annot1.position.column <= annot2.position.column then
    annot1
  else
    annot2
    
let rename_variable_in_disjoints (var: identifier) (variables: IdentifierSet.t) (disjoints: LogicFormulas.t list) (phantom_id: int) =
  let (new_var, phantom_id) = new_variable_name var phantom_id in
  let variables = IdentifierSet.add new_var (IdentifierSet.remove var variables) in
  let disjoints = List.map (fun x -> rename_variable_in_formula x var new_var) disjoints in
  (variables, disjoints, phantom_id)