open Prelude.Ast.LogicFormulas
open Prelude.Ast.LogicFormulas.AnnotatedNode
open Utils

let source = {|<< ((y -> v) * (x = y)) && (x = v) >>|}

let expected: Formula.t = {
  node = (And (
    {
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
              node = (Variable "x");
              annotation = { position = dummy_position }
            },
            {
              node = (Variable "y");
              annotation = { position = dummy_position }
            }
          ));
          annotation = { position = dummy_position }
        }
      ));
      annotation = {position = dummy_position}
    },
    {
    node = (Comparison (
          Equals,
          {
            node = (Variable "x");
            annotation = { position = dummy_position }
          },
          {
            node = (Variable "v");
            annotation = { position = dummy_position }
          }
        ));
        annotation = { position = dummy_position }
    }
  ));
  annotation = { position = dummy_position }
}
;;

let%test_unit "test formulae n. 02" =
  [%test_eq: Formula.t] (parse_formula source) expected