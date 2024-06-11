open Prelude.Ast.LogicFormulas
open Prelude.Ast.LogicFormulas.AnnotatedNode

let dummy_position = { Prelude.Ast.line = -1; column = -1 }

let test_node (formula) =
  {node = formula; annotation = {position = dummy_position}}