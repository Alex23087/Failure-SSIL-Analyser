open Prelude.Ast.Commands
open Prelude.Ast.LogicFormulas
open Prelude.Ast.Commands.AnnotatedNode
open Prelude.Ast.LogicFormulas.AnnotatedNode
open Utils

let source = {|x = alloc();
y = alloc();
if (x == y || x != y && true) {
  [x] = 42;
  free(x)
} else {
  [y] = 0;
  free(y)
};
skip
|}

let expected: HeapRegularCommand.t = {
  node = (HeapRegularCommand.Sequence (
    {
      node = (HeapRegularCommand.Sequence (
        {
          node = (HeapRegularCommand.Command {
            node = (HeapAtomicCommand.Allocation "x");
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
          node = (HeapRegularCommand.Condition (
            {
              node = (BinaryOperation (
                ArithmeticOperation.Or,
                {
                  node = (BinaryOperation (
                    ArithmeticOperation.Equal,
                    {
                      node = (Variable "x");
                      annotation = {
                        position = dummy_position;
                        logic_formula = None
                      }
                    },
                    {
                      node = (Variable "y");
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
                  node = (BinaryOperation (
                    ArithmeticOperation.And,
                    {
                      node = (BinaryOperation (
                        ArithmeticOperation.NotEqual,
                        {
                          node = (Variable "x");
                          annotation = {
                            position = dummy_position;
                            logic_formula = None
                          }
                        },
                        {
                          node = (Variable "y");
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
            },
            {
              node = (HeapRegularCommand.Sequence (
                {
                  node = (HeapRegularCommand.Command {
                    node = (HeapAtomicCommand.WriteHeap (
                      "x",
                      {
                        node = (Literal 42);
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
                    node = (HeapAtomicCommand.Free "x");
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
                  node = (HeapRegularCommand.Command {
                    node = (HeapAtomicCommand.WriteHeap (
                      "y",
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
    }
  ));
  annotation = {
    position = dummy_position;
    logic_formula = None
  }
}
;;

let%test_unit "test commands n. 04" =
  [%test_eq: HeapRegularCommand.t] (parse_command source) expected
