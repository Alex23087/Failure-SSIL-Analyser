open Lisproject.Ast
open Sexplib.Std

(* Instantiate the AST with the annotation type *)
module ASTHRC = HeapRegularCommands(struct
  type t = int (* int annotations *) [@@deriving show, sexp]
end)

let counter = ref 0

(* Annotate a node with a unique integer *)
let annotate (node: 'a): 'a ASTHRC.AnnotatedNode.t =
  let out = ASTHRC.AnnotatedNode.make node !counter in
  counter := !counter + 1;
  out

open ASTHRC

let () =
  (* Create an AST corresponding to the RegCmd:  x = 1; (x < 10?; x = x + 1)*; !(x < 10)?
     This encodes the command:  x = 1; while(x<10){x = x + 1}  *)
  let root = annotate (HeapRegularCommand.Sequence(
    annotate (HeapRegularCommand.Command(annotate (HeapAtomicCommand.Assignment("x", annotate (ArithmeticExpression.Literal 1))))),

    annotate (HeapRegularCommand.Sequence(
      annotate (HeapRegularCommand.Star(
        annotate (HeapRegularCommand.Sequence(
          annotate (HeapRegularCommand.Command(annotate (HeapAtomicCommand.Guard(annotate (BooleanExpression.Comparison(
            BooleanComparison.LessThan,
            annotate (ArithmeticExpression.Variable "x"),
            annotate (ArithmeticExpression.Literal 10)
          )))))),
          annotate (HeapRegularCommand.Command(annotate (HeapAtomicCommand.Assignment("x", annotate (
            ArithmeticExpression.BinaryOperation(
              ArithmeticOperation.Plus,
              annotate (ArithmeticExpression.Variable "x"),
              annotate (ArithmeticExpression.Literal 1)))))))
        ))
      )),

      annotate (HeapRegularCommand.Command(annotate (HeapAtomicCommand.Guard(annotate (BooleanExpression.Not(annotate (BooleanExpression.Comparison(
        BooleanComparison.LessThan,
        annotate (ArithmeticExpression.Variable "x"),
        annotate (ArithmeticExpression.Literal 10)
      ))))))))
    ))
  )) in
  match root.node with
    | Command(a) -> (match a.node with
      | Guard(a) -> (match a.node with
        | Comparison(a, b, c) -> (match (a, b.node, c.node) with
          | (Equal, Variable(x), Literal(y)) -> print_endline ("Equal: " ^ x ^ " = " ^ (string_of_int y))
          | _ -> print_endline "Not Equal")
        | _ -> print_endline "Not Comp")
      | _ -> print_endline "Not Seq")
    | _ -> print_endline "Not Star";

  (* Print it with show_rcmd *)
  print_endline (show root);

  let root = annotate (HeapRegularCommand.Sequence(
    (annotate (HeapRegularCommand.Command(annotate (HeapAtomicCommand.Allocation("x"))))),
    (annotate (HeapRegularCommand.Sequence (
    (annotate (HeapRegularCommand.Command (annotate(HeapAtomicCommand.WriteHeap("x", annotate (ArithmeticExpression.Literal 1)))))),
    (annotate (HeapRegularCommand.Command (annotate(HeapAtomicCommand.ReadHeap("y", "x")))))
    ))
    )
  )) in
  print_endline (show root);
  print_endline (modifiedVariables root |> IdentifierSet.elements |> String.concat ", ");