open Prelude.Ast.Commands
open Prelude.Ast.LogicFormulas
open Prelude.Ast.Commands.AnnotatedNode
open Prelude.Ast.LogicFormulas.AnnotatedNode
open Utils

let source = {|x = alloc();
[x] = 1 + 400 << (exists y . x -> y) * emp >>;
free(x) << x -/> * emp >>
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
            node = (HeapAtomicCommand.WriteHeap (
              "x",
              {
                node = (BinaryOperation (
                  ArithmeticOperation.Plus,
                  {
                    node = (Literal 1);
                    annotation = {
                      position = dummy_position;
                      logic_formula = None
                    }
                  },
                  {
                    node = (Literal 400);
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
            logic_formula = (Some {
              node = (Formula.AndSeparately (
                {
                  node = (Formula.Exists (
                    "y",
                    {
                      node = (Formula.Allocation (
                        "x",
                        {
                          node = (Variable "y");
                          annotation = {
                            position = dummy_position
                          }
                        }
                      ));
                      annotation = {
                        position = dummy_position
                      }
                    }
                  ));
                  annotation = {
                    position = dummy_position
                  }
                },
                {
                  node = Formula.EmptyHeap;
                  annotation = {
                    position = dummy_position
                  }
                }
              ));
              annotation = {
                position = dummy_position
              }
            })
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
        node = (HeapAtomicCommand.Free "x");
        annotation = {
          position = dummy_position;
          logic_formula = None
        }
      });
      annotation = {
        position = dummy_position;
        logic_formula = (Some {
          node = (Formula.AndSeparately (
            {
              node = (Formula.NonAllocated "x");
              annotation = {
                position = dummy_position
              }
            },
            {
              node = Formula.EmptyHeap;
              annotation = {
                position = dummy_position
              }
            }
          ));
          annotation = {
            position = dummy_position
          }
        })
      }
    }
  ));
  annotation = {
    position = dummy_position;
    logic_formula = None
  }
}
;;

let%test_unit "test commands n. 01" =
  [%test_eq: HeapRegularCommand.t] (parse_command source) expected