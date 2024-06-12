open Ast
open Cfg_Node
open Cfg_Converter
open Cfg_Tests_Utils
open HeapRegularCommands

(* -------------------------------------------------------------------------- *)
(*                   test ast to cfg Command&Allocation                       *)

let%test "test ast to cfg Command&Allocation" =
  let source =
    annotate ( HeapRegularCommand.Command (
      annotate ( HeapAtomicCommand.Allocation "x")
    ))
  in
  let expected: ((unit HeapAtomicCommand.t) list Node.t) =
    Node.make [ annotate (HeapAtomicCommand.Allocation "x") ] [] []
  in
  Node.compare (Converter.convert source) expected


(* -------------------------------------------------------------------------- *)
(*                      test ast to cfg Sequence C&C                          *)

let%test "test ast to cfg Sequence C&C" =
  let source =
    annotate ( HeapRegularCommand.Sequence (
      annotate ( HeapRegularCommand.Command (
        annotate ( HeapAtomicCommand.Allocation "x")
      )),
      annotate ( HeapRegularCommand.Command (
        annotate ( HeapAtomicCommand.Allocation "x")
      ))
    ))
  in

  let expected: ((unit HeapAtomicCommand.t) list Node.t) =
    Node.make [ annotate (HeapAtomicCommand.Allocation "x");
                annotate (HeapAtomicCommand.Allocation "x") ] [] []
  in
  Node.compare (Converter.convert source) expected


(* -------------------------------------------------------------------------- *)
(*                          test ast to cfg Star                              *)



let%test "test ast to cfg Star" =
  let source =
    annotate ( HeapRegularCommand.Star (
      annotate ( HeapRegularCommand.Command (
        annotate ( HeapAtomicCommand.Allocation "x")
      ))
    ))
  in
  let expected: ((unit HeapAtomicCommand.t) list Node.t) =
    Node.make [ annotate (HeapAtomicCommand.Allocation "x") ] [] []
  in
  Node.add_succ expected expected;
  Node.compare (Converter.convert source) expected


(* -------------------------------------------------------------------------- *)
(*                         test ast to cfg NonDet                             *)

let%test "test ast to cfg NonDet" =
  let source =
    annotate ( HeapRegularCommand.NondeterministicChoice (
      annotate ( HeapRegularCommand.Command (
        annotate ( HeapAtomicCommand.Allocation "x")
      )),
      annotate ( HeapRegularCommand.Command (
        annotate ( HeapAtomicCommand.Allocation "y")
      ))
    ))
  in

  let expected: ((unit HeapAtomicCommand.t) list Node.t) =
    Node.make_with_id 1 [] [
        Node.make_with_id 2 [ annotate ( HeapAtomicCommand.Allocation "x") ] [
            Node.make_with_id 4 [] [] [2; 3]
          ] [1];
        Node.make_with_id 3 [ annotate ( HeapAtomicCommand.Allocation "y") ] [
            Node.make_with_id 4 [] [] [2; 3]
          ] [1]
      ] []
  in
  Node.compare (Converter.convert source) expected


(* -------------------------------------------------------------------------- *)
(*                        Testing everything works                            *)

let%test_unit "test ast to cfg Everything" =
  let source =
    annotate (HeapRegularCommand.Sequence(
      annotate (HeapRegularCommand.Command(
        annotate (HeapAtomicCommand.Assignment("x",
          annotate (ArithmeticExpression.Literal 1)
        ))
      )),
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
  in
  let _ = Converter.convert source in
  ()
