open Lisproject.Ast
open HeapRegularCommands
open Lisproject.Cfg

let counter = ref 0

type t = int HeapRegularCommands.t
[@@deriving show]

(* Annotate a node with a unique integer *)
let annotate node =
  let out = AnnotatedNode.make node !counter in
  counter := !counter + 1;
  out

let () =
  (* Create an AST corresponding to the RegCmd:  x = 1; (x < 10?; x = x + 1)*; !(x < 10)?
     This encodes the command:  x = 1; while(x<10){x = x + 1}  *)
  let root = annotate (HeapRegularCommand.Sequence(
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
  )) in
  (* Print it with show_rcmd *)
  (* print_endline (show root); *)
  let convertedroot = convert root in
  let _print_command = (HeapAtomicCommand.pp (fun _ _ -> ())) in

  (* print_endline (Node.show (fun _ _ -> ()) convertedroot); *)
  print_endline (string_of_int (Node.getnodeid convertedroot))



  (* let root = annotate (HeapRegularCommand.Sequence( *)
  (*   (annotate (HeapRegularCommand.Command(annotate (HeapAtomicCommand.Allocation("x"))))), *)
  (*   (annotate (HeapRegularCommand.Sequence ( *)
  (*   (annotate (HeapRegularCommand.Command (annotate(HeapAtomicCommand.WriteHeap("x", annotate (ArithmeticExpression.Literal 1)))))), *)
  (*   (annotate (HeapRegularCommand.Command (annotate(HeapAtomicCommand.ReadHeap("y", "x"))))) *)
  (*   )) *)
  (*   ) *)
  (* )) in *)
  (* let convertedroot = convert root in *)
  (* print_endline (Node.show (fun _ _ -> ()) convertedroot); *)
