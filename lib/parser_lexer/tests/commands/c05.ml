open Prelude.Ast.Commands
open Prelude.Ast.LogicFormulas
open Prelude.Ast.Commands.AnnotatedNode
open Prelude.Ast.LogicFormulas.AnnotatedNode
open Utils

let source = {|
  if (x == 0) {
    x = 1;
  } else {
    skip;
  }
|}

let expected: HeapRegularCommand.t = {
  node = (HeapRegularCommand.NondeterministicChoice (
    {
      node = (HeapRegularCommand.Sequence (
        {
          node = (HeapRegularCommand.Command {
            node = (HeapAtomicCommand.Guard {
              node = (BinaryOperation (
                ComparisonOperation.Equal,
                {
                  node = (Variable "x");
                  annotation = {
                    position = dummy_position;
                    logic_formula = None
                  }
                },
                {
                  node = (Literal 0);
                  annotation = {
                    position = dummy_position;
                    logic_formula = None
                  }
                }
              ));
              annotation = {
                position = dummy_position;
                logic_formula = None
              }
            });
            annotation = {
              position = dummy_position;
              logic_formula = None
            }
          });
          annotation = {
            position = dummy_position;
            logic_formula = None
          }
        },
        {
          node = (HeapRegularCommand.Command {
            node = (HeapAtomicCommand.WriteHeap (
              "x",
              {
                node = (Literal 1);
                annotation = {
                  position = dummy_position;
                  logic_formula = None
                }
              }
            ));
            annotation = {
              position = dummy_position;
              logic_formula = None
            }
          });
          annotation = {
            position = dummy_position;
            logic_formula = None
          }
        }
      ));
      annotation = {
        position = dummy_position;
        logic_formula = None
      }
    },
    {
      node = (HeapRegularCommand.Command {
        node = (HeapAtomicCommand.Skip);
        annotation = {
          position = dummy_position;
          logic_formula = None
        }
      });
      annotation = {
        position = dummy_position;
        logic_formula = None
      }
    }
  );
  annotation = {
    position = dummy_position;
    logic_formula = None
  }
  )
}
;;

let%test_unit "test commands n. 05" =
  [%test_eq: HeapRegularCommand.t] (parse_command source) expected