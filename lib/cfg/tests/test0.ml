open Ast
open Base
open Ast2cfg
open Cfg_Tests_Utils
open HeapRegularCommands

(* -------------------------------------------------------------------------- *)
(*                   test ast to cfg Command&Allocation                       *)
let source =
  annotate ( HeapRegularCommand.Command (
      annotate ( HeapAtomicCommand.Allocation "x")
  ))

let expected: ((unit HeapAtomicCommand.t) list Node.t) =
  Node.make [ annotate (HeapAtomicCommand.Allocation "x") ] [] []

let%test "test ast to cfg Command&Allocation" =
  Node.compare (Ast2cfgConverter.convert source) expected


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
  Node.make [ annotate (HeapAtomicCommand.Allocation "x");
              annotate (HeapAtomicCommand.Allocation "x") ] [] []

let%test "test ast to cfg Sequence C&C" =
  Node.compare (Ast2cfgConverter.convert source) expected


(* -------------------------------------------------------------------------- *)
(*                          test ast to cfg Star                              *)
let source =
  annotate ( HeapRegularCommand.Star (
    annotate ( HeapRegularCommand.Command (
        annotate ( HeapAtomicCommand.Allocation "x")
    ))
  ))

let expected: ((unit HeapAtomicCommand.t) list Node.t) =
  Node.make [ annotate (HeapAtomicCommand.Allocation "x") ] [] []
let () = Node.addsucc expected expected


let%test "test ast to cfg Star" =
  Node.compare (Ast2cfgConverter.convert source) expected


(* -------------------------------------------------------------------------- *)
(*                         test ast to cfg NonDet                             *)
let source =
  annotate ( HeapRegularCommand.NondeterministicChoice (
    annotate ( HeapRegularCommand.Command (
        annotate ( HeapAtomicCommand.Allocation "x")
    )),
    annotate ( HeapRegularCommand.Command (
        annotate ( HeapAtomicCommand.Allocation "y")
    ))
  ))

let expected: ((unit HeapAtomicCommand.t) list Node.t) =
  Node.makeWithId 1 [] [
      Node.makeWithId 2 [ annotate ( HeapAtomicCommand.Allocation "x") ] [
          Node.makeWithId 4 [] [] [2; 3]
        ] [1];
      Node.makeWithId 3 [ annotate ( HeapAtomicCommand.Allocation "y") ] [
          Node.makeWithId 4 [] [] [2; 3]
        ] [1]
    ] []

let%test "test ast to cfg NonDet" =
  Node.compare (Ast2cfgConverter.convert source) expected


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

let%test_unit "test ast to cfg Everything" =
  let _ = Ast2cfgConverter.convert source in
  ()
