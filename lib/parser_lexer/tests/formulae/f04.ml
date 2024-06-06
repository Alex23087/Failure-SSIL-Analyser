open Prelude.Ast.LogicFormulas
open Prelude.Ast.LogicFormulas.AnnotatedNode
open Utils

let source = {|<< exists n . ((0 + n >= 20000) && (n > 0))>>|}

let expected: Formula.t = {
  node = (Exists(
    "n",
    {
      node = (And(
        {
          node = (Comparison(
            GreaterOrEqual,
            {
              node = (Operation(
                Plus,
                {
                  node = Literal 0;
                  annotation = { position = dummy_position }
                },
                {
                  node = Variable "n";
                  annotation = { position = dummy_position }
                }
              ));
              annotation = { position = dummy_position }
            },
            {
              node = Literal 20000;
              annotation = { position = dummy_position }
            }
          ));
          annotation = { position = dummy_position }
        },
        {
          node = (Comparison(
            GreaterThan,
            {
              node = Variable "n";
              annotation = { position = dummy_position }
            },
            {
              node = Literal 0;
              annotation = { position = dummy_position }
            }
      ));
          annotation = { position = dummy_position }
        }
      ));
      annotation = {position = dummy_position}
    }
  ));
  annotation = {position = dummy_position}
}
;;

let%test_unit "test formulae n. 04" =
  [%test_eq: Formula.t] (parse_formula source) expected