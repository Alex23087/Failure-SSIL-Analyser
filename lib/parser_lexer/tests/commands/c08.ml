open Prelude.Ast.Commands
open Prelude.Ast.LogicFormulas
open Prelude.Ast.Commands.AnnotatedNode
open Prelude.Ast.LogicFormulas.AnnotatedNode
open Utils

let emptyAnnotation = {Prelude.Ast.position = dummy_position; logic_formula = None}
let annotateCommand formula =
  Prelude.Ast.Commands.annotate formula emptyAnnotation

(* annotation goes on if *)
let source = {|
  if (x < -(1 + 3))
    then x = (x*(-1))-4
    else skip
  << x -> 1 >>
|}

let expected: HeapRegularCommand.t = { node =
  (HeapRegularCommand.NondeterministicChoice (
     { node =
       (HeapRegularCommand.Sequence (
          { node =
            (HeapRegularCommand.Command
               { node =
                 (HeapAtomicCommand.Guard
                    { node =
                      (BooleanExpression.Comparison (
                         BooleanComparison.LessThan,
                         { node =
                           (Prelude.Ast.Commands.ArithmeticExpression.Variable
                              "x");
                           annotation =
                           { position =
                             dummy_position;
                             logic_formula = None }
                           },
                         { node =
                           (Prelude.Ast.Commands.ArithmeticExpression.BinaryOperation (
                              ArithmeticOperation.Minus,
                              { node =
                                (Prelude.Ast.Commands.ArithmeticExpression.Literal
                                   0);
                                annotation =
                                { position =
                                  dummy_position;
                                  logic_formula = None }
                                },
                              { node =
                                (Prelude.Ast.Commands.ArithmeticExpression.BinaryOperation (
                                   ArithmeticOperation.Plus,
                                   { node =
                                     (Prelude.Ast.Commands.ArithmeticExpression.Literal
                                        1);
                                     annotation =
                                     { position =
                                       dummy_position;
                                       logic_formula = None }
                                     },
                                   { node =
                                     (Prelude.Ast.Commands.ArithmeticExpression.Literal
                                        3);
                                     annotation =
                                     { position =
                                       dummy_position;
                                       logic_formula = None }
                                     }
                                   ));
                                annotation =
                                { position =
                                  dummy_position;
                                  logic_formula = None }
                                }
                              ));
                           annotation =
                           { position =
                             dummy_position;
                             logic_formula = None }
                           }
                         ));
                      annotation =
                      { position =
                        dummy_position;
                        logic_formula = None }
                      });
                 annotation =
                 { position =
                   dummy_position; logic_formula = None
                   }
                 });
            annotation =
            { position = dummy_position;
              logic_formula = None }
            },
          { node =
            (HeapRegularCommand.Command
               { node =
                 (HeapAtomicCommand.Assignment (
                    "x",
                    { node =
                      (Prelude.Ast.Commands.ArithmeticExpression.BinaryOperation (
                         ArithmeticOperation.Minus,
                         { node =
                           (Prelude.Ast.Commands.ArithmeticExpression.BinaryOperation (
                              ArithmeticOperation.Times,
                              { node =
                                (Prelude.Ast.Commands.ArithmeticExpression.Variable
                                   "x");
                                annotation =
                                { position =
                                  dummy_position;
                                  logic_formula = None }
                                },
                              { node =
                                (Prelude.Ast.Commands.ArithmeticExpression.BinaryOperation (
                                   ArithmeticOperation.Minus,
                                   { node =
                                     (Prelude.Ast.Commands.ArithmeticExpression.Literal
                                        0);
                                     annotation =
                                     { position =
                                       dummy_position;
                                       logic_formula = None }
                                     },
                                   { node =
                                     (Prelude.Ast.Commands.ArithmeticExpression.Literal
                                        1);
                                     annotation =
                                     { position =
                                       dummy_position;
                                       logic_formula = None }
                                     }
                                   ));
                                annotation =
                                { position =
                                  dummy_position;
                                  logic_formula = None }
                                }
                              ));
                           annotation =
                           { position =
                             dummy_position;
                             logic_formula = None }
                           },
                         { node =
                           (Prelude.Ast.Commands.ArithmeticExpression.Literal
                              4);
                           annotation =
                           { position =
                             dummy_position;
                             logic_formula = None }
                           }
                         ));
                      annotation =
                      { position =
                        dummy_position;
                        logic_formula = None }
                      }
                    ));
                 annotation =
                 { position =
                   dummy_position;
                   logic_formula = None }
                 });
            annotation =
            { position = dummy_position;
              logic_formula = None }
            }
          ));
       annotation =
       { position = dummy_position;
         logic_formula = None }
       },
     { node =
       (HeapRegularCommand.Sequence (
          { node =
            (HeapRegularCommand.Command
               { node =
                 (HeapAtomicCommand.Guard
                    { node =
                      (BooleanExpression.Not
                         { node =
                           (BooleanExpression.Comparison (
                              BooleanComparison.LessThan,
                              { node =
                                (Prelude.Ast.Commands.ArithmeticExpression.Variable
                                   "x");
                                annotation =
                                { position =
                                  dummy_position;
                                  logic_formula = None }
                                },
                              { node =
                                (Prelude.Ast.Commands.ArithmeticExpression.BinaryOperation (
                                   ArithmeticOperation.Minus,
                                   { node =
                                     (Prelude.Ast.Commands.ArithmeticExpression.Literal
                                        0);
                                     annotation =
                                     { position =
                                       dummy_position;
                                       logic_formula = None }
                                     },
                                   { node =
                                     (Prelude.Ast.Commands.ArithmeticExpression.BinaryOperation (
                                        ArithmeticOperation.Plus,
                                        { node =
                                          (Prelude.Ast.Commands.ArithmeticExpression.Literal
                                             1);
                                          annotation =
                                          { position =
                                           dummy_position;
                                            logic_formula = None }
                                          },
                                        { node =
                                          (Prelude.Ast.Commands.ArithmeticExpression.Literal
                                             3);
                                          annotation =
                                          { position =
                                            dummy_position;
                                            logic_formula = None }
                                          }
                                        ));
                                     annotation =
                                     { position =
                                       dummy_position;
                                       logic_formula = None }
                                     }
                                   ));
                                annotation =
                                { position =
                                  dummy_position;
                                  logic_formula = None }
                                }
                              ));
                           annotation =
                           { position =
                             dummy_position;
                             logic_formula = None }
                           });
                      annotation =
                      { position =
                        dummy_position;
                        logic_formula = None }
                      });
                 annotation =
                 { position =
                   dummy_position; logic_formula = None
                   }
                 });
            annotation =
            { position = dummy_position;
              logic_formula = None }
            },
          { node =
            (HeapRegularCommand.Command
               { node =
                 HeapAtomicCommand.Skip;
                 annotation =
                 { position =
                   dummy_position;
                   logic_formula = None }
                 });
            annotation =
            { position = dummy_position;
              logic_formula = None }
            }
          ));
       annotation =
       { position = dummy_position;
         logic_formula = None }
       }
     ));
  annotation =
  { position = dummy_position;
    logic_formula =
    (Some { node =
            (Formula.Allocation ("x",
               { node =
                 (ArithmeticExpression.Literal
                    1);
                 annotation =
                 { position =
                   dummy_position}
                 }
               ));
            annotation =
            { position = dummy_position}
            })
    }
  }

let%test_unit "test commands n. 08 - 01" =
  [%test_eq: HeapRegularCommand.t] (parse_command source) expected


(* annotation goes on skip *)
let source_2 = {|
  if (x < -(1 + 3))
    then x = (x*(-1))-4
    else { skip << x -> 1 >> }
|}

let expected_2: HeapRegularCommand.t = { node =
  (HeapRegularCommand.NondeterministicChoice (
     { node =
       (HeapRegularCommand.Sequence (
          { node =
            (HeapRegularCommand.Command
               { node =
                 (HeapAtomicCommand.Guard
                    { node =
                      (BooleanExpression.Comparison (
                         BooleanComparison.LessThan,
                         { node =
                           (Prelude.Ast.Commands.ArithmeticExpression.Variable
                              "x");
                           annotation =
                           { Prelude.Ast.position =
                             dummy_position;                             logic_formula = None }
                           },
                         { node =
                           (Prelude.Ast.Commands.ArithmeticExpression.BinaryOperation (
                              ArithmeticOperation.Minus,
                              { node =
                                (Prelude.Ast.Commands.ArithmeticExpression.Literal
                                   0);
                                annotation =
                                { Prelude.Ast.position =
                                  dummy_position;                                  logic_formula = None }
                                },
                              { node =
                                (Prelude.Ast.Commands.ArithmeticExpression.BinaryOperation (
                                   ArithmeticOperation.Plus,
                                   { node =
                                     (Prelude.Ast.Commands.ArithmeticExpression.Literal
                                        1);
                                     annotation =
                                     { Prelude.Ast.position =
                                       dummy_position;                                       logic_formula = None }
                                     },
                                   { node =
                                     (Prelude.Ast.Commands.ArithmeticExpression.Literal
                                        3);
                                     annotation =
                                     { Prelude.Ast.position =
                                       dummy_position;                                       logic_formula = None }
                                     }
                                   ));
                                annotation =
                                { Prelude.Ast.position =
                                  dummy_position;                                  logic_formula = None }
                                }
                              ));
                           annotation =
                           { Prelude.Ast.position =
                             dummy_position;                             logic_formula = None }
                           }
                         ));
                      annotation =
                      { Prelude.Ast.position =
                        dummy_position;                        logic_formula = None }
                      });
                 annotation =
                 { Prelude.Ast.position =
                   dummy_position; logic_formula = None
                   }
                 });
            annotation =
            { Prelude.Ast.position = dummy_position;              logic_formula = None }
            },
          { node =
            (HeapRegularCommand.Command
               { node =
                 (HeapAtomicCommand.Assignment (
                    "x",
                    { node =
                      (Prelude.Ast.Commands.ArithmeticExpression.BinaryOperation (
                         ArithmeticOperation.Minus,
                         { node =
                           (Prelude.Ast.Commands.ArithmeticExpression.BinaryOperation (
                              ArithmeticOperation.Times,
                              { node =
                                (Prelude.Ast.Commands.ArithmeticExpression.Variable
                                   "x");
                                annotation =
                                { Prelude.Ast.position =
                                  dummy_position;                                  logic_formula = None }
                                },
                              { node =
                                (Prelude.Ast.Commands.ArithmeticExpression.BinaryOperation (
                                   ArithmeticOperation.Minus,
                                   { node =
                                     (Prelude.Ast.Commands.ArithmeticExpression.Literal
                                        0);
                                     annotation =
                                     { Prelude.Ast.position =
                                       dummy_position;                                       logic_formula = None }
                                     },
                                   { node =
                                     (Prelude.Ast.Commands.ArithmeticExpression.Literal
                                        1);
                                     annotation =
                                     { Prelude.Ast.position =
                                       dummy_position;                                       logic_formula = None }
                                     }
                                   ));
                                annotation =
                                { Prelude.Ast.position =
                                  dummy_position;                                  logic_formula = None }
                                }
                              ));
                           annotation =
                           { Prelude.Ast.position =
                             dummy_position;                             logic_formula = None }
                           },
                         { node =
                           (Prelude.Ast.Commands.ArithmeticExpression.Literal
                              4);
                           annotation =
                           { Prelude.Ast.position =
                             dummy_position;                             logic_formula = None }
                           }
                         ));
                      annotation =
                      { Prelude.Ast.position =
                        dummy_position;                        logic_formula = None }
                      }
                    ));
                 annotation =
                 { Prelude.Ast.position =
                   dummy_position;
                   logic_formula = None }
                 });
            annotation =
            { Prelude.Ast.position = dummy_position;
              logic_formula = None }
            }
          ));
       annotation =
       { Prelude.Ast.position = dummy_position;
         logic_formula = None }
       },
     { node =
       (HeapRegularCommand.Sequence (
          { node =
            (HeapRegularCommand.Command
               { node =
                 (HeapAtomicCommand.Guard
                    { node =
                      (BooleanExpression.Not
                         { node =
                           (BooleanExpression.Comparison (
                              BooleanComparison.LessThan,
                              { node =
                                (Prelude.Ast.Commands.ArithmeticExpression.Variable
                                   "x");
                                annotation =
                                { Prelude.Ast.position =
                                  dummy_position;
                                  logic_formula = None }
                                },
                              { node =
                                (Prelude.Ast.Commands.ArithmeticExpression.BinaryOperation (
                                   ArithmeticOperation.Minus,
                                   { node =
                                     (Prelude.Ast.Commands.ArithmeticExpression.Literal
                                        0);
                                     annotation =
                                     { Prelude.Ast.position =
                                       dummy_position;
                                       logic_formula = None }
                                     },
                                   { node =
                                     (Prelude.Ast.Commands.ArithmeticExpression.BinaryOperation (
                                        ArithmeticOperation.Plus,
                                        { node =
                                          (Prelude.Ast.Commands.ArithmeticExpression.Literal
                                             1);
                                          annotation =
                                          { position = dummy_position;
                                            logic_formula = None }
                                          },
                                        { node =
                                          (Prelude.Ast.Commands.ArithmeticExpression.Literal
                                             3);
                                          annotation =
                                          { position = dummy_position;
                                            logic_formula = None }
                                          }
                                        ));
                                     annotation =
                                     { Prelude.Ast.position =
                                       dummy_position;
                                       logic_formula = None }
                                     }
                                   ));
                                annotation =
                                { Prelude.Ast.position =
                                  dummy_position;
                                  logic_formula = None }
                                }
                              ));
                           annotation =
                           { Prelude.Ast.position =
                             dummy_position;
                             logic_formula = None }
                           });
                      annotation =
                      { Prelude.Ast.position =
                        dummy_position;
                        logic_formula = None }
                      });
                 annotation =
                 { Prelude.Ast.position =
                   dummy_position; logic_formula = None
                   }
                 });
            annotation =
            { Prelude.Ast.position = dummy_position;
              logic_formula = None }
            },
          { node =
            (HeapRegularCommand.Command
               { node =
                 HeapAtomicCommand.Skip;
                 annotation =
                 { Prelude.Ast.position =
                   dummy_position;
                   logic_formula = None }
                 });
            annotation =
            { Prelude.Ast.position = dummy_position;
              logic_formula =
              (Some { node =
                      (Prelude.Ast.LogicFormulas.Formula.Allocation (
                         "x",
                         { node =
                           (ArithmeticExpression.Literal
                              1);
                           annotation =
                           { Prelude.Ast.position =
                             dummy_position }
                           }
                         ));
                      annotation =
                      { Prelude.Ast.position =
                        dummy_position }
                      })
              }
            }
          ));
       annotation =
       { Prelude.Ast.position = dummy_position;         logic_formula = None }
       }
     ));
  annotation =
  { Prelude.Ast.position = dummy_position;    logic_formula = None }
  }

let%test_unit "test commands n. 08 - 02" =
  [%test_eq: HeapRegularCommand.t] (parse_command source_2) expected_2