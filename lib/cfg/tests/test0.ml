open Ast
open Base
open Ast2cfg
open Cfg_Tests_Utils
open HeapRegularCommands

(* TODO: make the test not depend on each other, on the order of operations
         or on the actual value of the id
         (also equality checks every loop so it doesn't stop) *)

(* -------------------------------------------------------------------------- *)
(*                   test ast to cfg Command&Allocation                       *)
let source =
  annotate ( HeapRegularCommand.Command (
      annotate ( HeapAtomicCommand.Allocation "x")
  ))

let expected: ((unit HeapAtomicCommand.t) list Node.t) =
  Node.makeWithId 1 [ annotate (HeapAtomicCommand.Allocation "x") ] [] []

let%test_unit "test ast to cfg Command&Allocation" =
  [%test_eq: _ Node.t] (Ast2cfgConverter.convert source) expected


(* -------------------------------------------------------------------------- *)
(*                      test ast to cfg Sequence C&C                          *)
let source =
  annotate ( HeapRegularCommand.Sequence (
    annotate ( HeapRegularCommand.Command (
        annotate ( HeapAtomicCommand.Allocation "x")
    )),
    annotate ( HeapRegularCommand.Command (
        annotate ( HeapAtomicCommand.Allocation "x")
    ))
  ))

let expected: ((unit HeapAtomicCommand.t) list Node.t) =
  Node.makeWithId 3 [ annotate (HeapAtomicCommand.Allocation "x");
                      annotate (HeapAtomicCommand.Allocation "x") ] [] []

let%test_unit "test ast to cfg Sequence C&C" =
  [%test_eq: _ Node.t] (Ast2cfgConverter.convert source) expected


(* -------------------------------------------------------------------------- *)
(*                          test ast to cfg Star                              *)
let source =
  annotate ( HeapRegularCommand.Star (
    annotate ( HeapRegularCommand.Command (
        annotate ( HeapAtomicCommand.Allocation "x")
    ))
  ))

let expected: ((unit HeapAtomicCommand.t) list Node.t) =
  Node.makeWithId 4 [ annotate (HeapAtomicCommand.Allocation "x"); ] [] [4]

let%test_unit "test ast to cfg Star" =
  [%test_eq: _ Node.t] (convert_for_star source) expected


(* -------------------------------------------------------------------------- *)
(*                          test ast to cfg Star                              *)
let source =
  annotate ( HeapRegularCommand.NondeterministicChoice (
    annotate ( HeapRegularCommand.Command (
        annotate ( HeapAtomicCommand.Allocation "x")
    )),
    annotate ( HeapRegularCommand.Command (
        annotate ( HeapAtomicCommand.Allocation "x")
    ))
  ))

let expected: ((unit HeapAtomicCommand.t) list Node.t) =
  Node.makeWithId 7 [] [
      Node.makeWithId 5 [ annotate ( HeapAtomicCommand.Allocation "x") ] [
          Node.makeWithId 8 [] [] [5; 6]
        ] [7];
      Node.makeWithId 6 [ annotate ( HeapAtomicCommand.Allocation "x") ] [
          Node.makeWithId 8 [] [] [5; 6]
        ] [7]
    ] []

let%test_unit "test ast to cfg Star" =
  [%test_eq: _ Node.t] (Ast2cfgConverter.convert source) expected


(* -------------------------------------------------------------------------- *)
(*                        Testing everything works                            *)
let source = annotate (HeapRegularCommand.Sequence(
      annotate (HeapRegularCommand.Command(
        annotate (HeapAtomicCommand.Assignment("x",
          annotate (ArithmeticExpression.Literal 1))))),
      annotate (HeapRegularCommand.Sequence(
      annotate (HeapRegularCommand.Star(
        annotate (HeapRegularCommand.Sequence(
          annotate (HeapRegularCommand.Command(
            annotate (HeapAtomicCommand.Guard(
              annotate (BooleanExpression.Comparison(
                BooleanComparison.LessThan,
                annotate (ArithmeticExpression.Variable "x"),
                annotate (ArithmeticExpression.Literal 10)
              ))
            ))
          )),
          annotate (HeapRegularCommand.Command(
            annotate (HeapAtomicCommand.Assignment(
              "x",
              annotate (ArithmeticExpression.BinaryOperation(
                ArithmeticOperation.Plus,
                annotate (ArithmeticExpression.Variable "x"),
                annotate (ArithmeticExpression.Literal 1)
              ))
            ))
          ))
        ))
      )),

      annotate (HeapRegularCommand.Command(
        annotate (HeapAtomicCommand.Guard(
          annotate (BooleanExpression.Not(
            annotate (BooleanExpression.Comparison(
              BooleanComparison.LessThan,
              annotate (ArithmeticExpression.Variable "x"),
              annotate (ArithmeticExpression.Literal 10)
            ))
          ))
        ))
      ))
    ))
  ))

let%test_unit "test ast to cfg Sequence C&C" =
  [%test_eq: _] (let _ = Ast2cfgConverter.convert source in ()) ()
