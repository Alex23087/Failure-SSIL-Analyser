open Prelude.Ast.Commands
open Prelude.Ast.LogicFormulas
open Prelude.Ast.Commands.AnnotatedNode
open Prelude.Ast.LogicFormulas.AnnotatedNode
open Utils

let source = {|y = alloc();
[y] = 2 * 300;
if (y == 600) {
  y = nondet()
} else {
  skip
};
free(y)
|}

let expected: HeapRegularCommand.t = {
  node = (HeapRegularCommand.Sequence (
    {
      node = (HeapRegularCommand.Sequence (
        {
          node = (HeapRegularCommand.Command {
            node = (HeapAtomicCommand.Allocation "y");
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
              "y",
              {
                node = (BinaryOperation (
                  ArithmeticOperation.Multiplication,
                  {
                    node = (Literal 2);
                    annotation = {
                      position = dummy_position;
                      logic_formula = None
                    }
                  },
                  {
                    node = (Literal 300);
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
      node = (HeapRegularCommand.Sequence (
        {
          node = (HeapRegularCommand.Conditional (
            {
              node = (BinaryOperation (
                ArithmeticOperation.Equal,
                {
                  node = (Variable "y");
                  annotation = {
                    position = dummy_position;
                    logic_formula = None
                  }
                },
                {
                  node = (Literal 600);
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
                node = (HeapAtomicCommand.Assignment (
                  "y",
                  {
                    node = HeapAtomicCommand.NonDet;
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
            },
            {
              node = (HeapRegularCommand.Command {
                node = HeapAtomicCommand.Skip;
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
            node = (HeapAtomicCommand.Free "y");
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
    }
  ));
  annotation = {
    position = dummy_position;
    logic_formula = None
  }
}
;;

let%test_unit "test commands n. 02" =
  [%test_eq: HeapRegularCommand.t] (parse_command source) expected
