open Prelude.Ast.Commands
open Prelude.Ast.LogicFormulas
open Prelude.Ast.Commands.AnnotatedNode
open Prelude.Ast.LogicFormulas.AnnotatedNode
open Utils

let source = {|
  x = alloc();
  while (x < 10) {
    x = x + 1
  }
|}

let expected: HeapRegularCommand.t = {
  node = (HeapRegularCommand.Star (
    {
      node = (HeapRegularCommand.Sequence (
        {
          node = (HeapRegularCommand.Command {
            node = (HeapAtomicCommand.Guard {
              node = (BooleanExpression.LessThan {
                node = (Variable "x");
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
          });
          annotation = {
            position = dummy_position;
            logic_formula = None
          }
        },
        {
          node = (HeapRegularCommand.Sequence (
            {
              node = (HeapRegularCommand.Command {
                node = (HeapAtomicCommand.Assignment {
                  node = (Assignment {
                    node = (Variable "x");
                    value = {
                      node = (BinaryOperation (
                        ArithmeticOperation.Plus,
                        {
                          node = (Variable "x");
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
                    };
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
              });
              annotation = {
                position = dummy_position;
                logic_formula = None
              }
            },
            {
              node = (HeapRegularCommand.Command {
                node = (HeapAtomicCommand.Guard {
                  node = (BooleanExpression.LessThan {
                    node = (Variable "x");
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
  ));
  annotation = {
    position = dummy_position;
    logic_formula = None
  }
}
  ;;
