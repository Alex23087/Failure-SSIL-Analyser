open Prelude.Ast.Commands
open Prelude.Ast.LogicFormulas
open Prelude.Ast.Commands.AnnotatedNode
open Prelude.Ast.LogicFormulas.AnnotatedNode
open Utils

let source = {|z = alloc();
[z] = 5 * (10 + 2);
while (z != 0) {
  z = z - 1
};
free(z)
|}

let expected: HeapRegularCommand.t = {
  node = (HeapRegularCommand.Sequence (
    {
      node = (HeapRegularCommand.Sequence (
        {
          node = (HeapRegularCommand.Command {
            node = (HeapAtomicCommand.Allocation "z");
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
              "z",
              {
                node = (BinaryOperation (
                  ArithmeticOperation.Times,
                  {
                    node = (Literal 5);
                    annotation = {
                      position = dummy_position;
                      logic_formula = None
                    }
                  },
                  {
                    node = (BinaryOperation (
                      ArithmeticOperation.Plus,
                      {
                        node = (Literal 10);
                        annotation = {
                          position = dummy_position;
                          logic_formula = None
                        }
                      },
                      {
                        node = (Literal 2);
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
          node = (HeapRegularCommand.While (
            {
              node = (BinaryOperation (
                ArithmeticOperation.NotEqual,
                {
                  node = (Variable "z");
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
            },
            {
              node = (HeapRegularCommand.Command {
                node = (HeapAtomicCommand.Assignment (
                  "z",
                  {
                    node = (BinaryOperation (
                      ArithmeticOperation.Minus,
                      {
                        node = (Variable "z");
                        annotation = {
                          position = dummy_position;
                          logic_formula = None
                        }
                      },
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
            node = (HeapAtomicCommand.Free "z");
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

let%test_unit "test commands n. 03" =
  [%test_eq: HeapRegularCommand.t] (parse_command source) expected
