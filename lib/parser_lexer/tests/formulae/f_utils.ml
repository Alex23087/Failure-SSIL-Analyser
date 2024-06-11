open Prelude.Ast.LogicFormulas
open Prelude.Ast.LogicFormulas.AnnotatedNode
open Utils

let test_node (formula) =
  {node = formula; annotation = {position = dummy_position}}