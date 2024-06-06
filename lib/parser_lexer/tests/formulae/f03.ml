open Prelude.Ast.LogicFormulas
open Prelude.Ast.LogicFormulas.AnnotatedNode
open Utils

let source = {|<< (exists a . v -> z) * ( (z = a) * ( true * ( (exists b . a -> b) * ( (x = a) || (x -/>) ) ) ) ) >>|}

let expected: Formula.t = {
  node = ( AndSeparately (
    {
      node = ( Exists (
        "a",
        {
          node = (Allocation (
            "v",
            {
              node = (Variable "z");
              annotation = { position = dummy_position }
            }
          ));
          annotation = { position = dummy_position }
        }
      ));
      annotation = { position = dummy_position }
    },
    {
      node = ( AndSeparately (
        {
          node = (Comparison (
            Equals,
            {
              node = (Variable "z");
              annotation = { position = dummy_position }
            },
            {
              node = (Variable "a");
              annotation = { position = dummy_position }
            }
          ));
          annotation = { position = dummy_position }
        },
        {
          node = (AndSeparately(
            {
              node = True;
              annotation = { position = dummy_position }
            },
            {
              node = (AndSeparately(
                {
                  node = (Exists (
                    "b",
                    {
                      node = (Allocation(
                        "a",
                        {
                          node = (Variable "b");
                          annotation = { position = dummy_position }
                        }
                      ));
                      annotation = { position = dummy_position }
                    }
                  ));
                  annotation = { position = dummy_position }
                },
                {
                  node = (Or(
                    {
                      node = (Comparison(
                        Equals,
                        {
                          node = (Variable "x");
                          annotation = { position = dummy_position}
                        },
                        {
                          node = (Variable "a");
                          annotation = { position = dummy_position }
                        }
                      ));
                      annotation = { position = dummy_position }
                    },
                    {
                      node = (NonAllocated "x");
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
      annotation = { position = dummy_position }
    }
  ));
  annotation = {position = dummy_position}
}
;;

let%test_unit "test formulae n. 03" =
  [%test_eq: Formula.t] (parse_formula source) expected