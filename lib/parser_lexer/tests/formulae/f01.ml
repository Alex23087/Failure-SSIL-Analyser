open Prelude.Ast.LogicFormulas
open Prelude.Ast.LogicFormulas.AnnotatedNode
open Utils

let source = {|<< (y -> v) * (v = x) >>|}

let expected: Formula.t = {
  node = (AndSeparately (
    {
      node = (Allocation (
        "y",
        {
          node = (Variable "v");
          annotation = { position = dummy_position }
        }
      ));
      annotation = { position = dummy_position }
    }, 
    {
      node = (Comparison (
        Equals,
        {
          node = (Variable "v");
          annotation = { position = dummy_position }
        },
        {
          node = (Variable "x");
          annotation = { position = dummy_position }
        }
      ));
      annotation = { position = dummy_position }
    }
  ));
  annotation = {position = dummy_position}
}
;;

let%test_unit "test formulae n. 01" =
  [%test_eq: Formula.t] (parse_formula source) expected