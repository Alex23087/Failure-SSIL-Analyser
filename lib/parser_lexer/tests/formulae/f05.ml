open Prelude.Ast.LogicFormulas
open Prelude.Ast.LogicFormulas.AnnotatedNode
open Utils

let source = {|<< exists y . ( (y < 0) && (x = (0 - y)) ) >>|}

let expected: Formula.t = {
  node = (Exists(
    "y",
    {
      node = (And(
        {
          node = (Comparison(
            LessThan,
            {
              node = Variable "y";
              annotation = { position = dummy_position}
            },
            {
              node = Literal 0;
              annotation = { position = dummy_position }
            }
          ));
          annotation = { position = dummy_position }
        },
        {
          node = (Comparison(
            Equals,
            {
              node = Variable "x";
              annotation = { position = dummy_position }
            },
            { 
              node = (Operation(
                Minus,
                {
                  node = Literal 0;
                  annotation = { position = dummy_position }
                },
                {
                  node = Variable "y";
                  annotation = { position = dummy_position }
                }
              ));
              annotation = { position = dummy_position }
            }
          ));
          annotation = { position = dummy_position }
        }
      ));
      annotation = { position = dummy_position }
    }
  ));
  annotation = {position = dummy_position}
}
;;

let%test_unit "test formulae n. 05" =
  [%test_eq: Formula.t] (parse_formula source) expected