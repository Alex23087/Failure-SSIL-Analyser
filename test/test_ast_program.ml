open Lisproject.Ast_program

(* Instantiate the AST with the annotation type *)
module ASTHRC = ASTHeapRegularCommands(struct
  type t = int (* int annotations *)
end)

let counter = ref 0

(* Annotate a node with a unique integer *)
let annotate (node: 'a): 'a ASTHRC.annotated_node =
  let out: 'a ASTHRC.annotated_node = ASTHRC.addAnnotation node !counter in
  counter := !counter + 1;
  out

let () =
  (* Create an AST corresponding to the RegCmd:  x = 1; (x < 10?; x = x + 1)*; !(x < 10)?
     This encodes the command:  x = 1; while(x<10){x = x + 1}  *)
  let root = annotate (ASTHRC.HeapRegularCommand.Sequence(
    annotate (ASTHRC.HeapRegularCommand.Command(annotate (ASTHRC.HeapAtomicCommand.Assignment("x", annotate (ASTHRC.ArithmeticExpression.Literal 1))))),

    annotate (ASTHRC.HeapRegularCommand.Sequence(
      annotate (ASTHRC.HeapRegularCommand.Star(
        annotate (ASTHRC.HeapRegularCommand.Sequence(
          annotate (ASTHRC.HeapRegularCommand.Command(annotate (ASTHRC.HeapAtomicCommand.Guard(annotate (ASTHRC.BooleanExpression.Comparison(
            ASTHRC.BooleanComparison.LessThan,
            annotate (ASTHRC.ArithmeticExpression.Variable "x"),
            annotate (ASTHRC.ArithmeticExpression.Literal 10)
          )))))),
          annotate (ASTHRC.HeapRegularCommand.Command(annotate (ASTHRC.HeapAtomicCommand.Assignment("x", annotate (
            ASTHRC.ArithmeticExpression.BinaryOperation(
              ASTHRC.ArithmeticOperation.Plus,
              annotate (ASTHRC.ArithmeticExpression.Variable "x"),
              annotate (ASTHRC.ArithmeticExpression.Literal 1)))))))
        ))
      )),

      annotate (ASTHRC.HeapRegularCommand.Command(annotate (ASTHRC.HeapAtomicCommand.Guard(annotate (ASTHRC.BooleanExpression.Not(annotate (ASTHRC.BooleanExpression.Comparison(
        ASTHRC.BooleanComparison.LessThan,
        annotate (ASTHRC.ArithmeticExpression.Variable "x"),
        annotate (ASTHRC.ArithmeticExpression.Literal 10)
      ))))))))
    ))
  )) in
  match ASTHRC.removeAnnotation root with
    | Command(a) -> (match ASTHRC.removeAnnotation a with
      | Guard(a) -> (match ASTHRC.removeAnnotation a with
        | Comparison(a, b, c) -> (match (a, b.node, c.node) with
          | (Equal, Variable(x), Literal(y)) -> print_endline ("Equal: " ^ x ^ " = " ^ (string_of_int y))
          | _ -> print_endline "Not Equal")
        | _ -> print_endline "Not Comp")
      | _ -> print_endline "Not Seq")
    | _ -> print_endline "Not Star";

  (* Print it with ASTHRC.show_rcmd *)
  print_endline (ASTHRC.show root);

  let root = annotate (ASTHRC.HeapRegularCommand.Sequence(
    (annotate (ASTHRC.HeapRegularCommand.Command(annotate (ASTHRC.HeapAtomicCommand.Allocation("x"))))),
    (annotate (ASTHRC.HeapRegularCommand.Sequence (
    (annotate (ASTHRC.HeapRegularCommand.Command (annotate(ASTHRC.HeapAtomicCommand.WriteHeap("x", annotate (ASTHRC.ArithmeticExpression.Literal 1)))))),
    (annotate (ASTHRC.HeapRegularCommand.Command (annotate(ASTHRC.HeapAtomicCommand.ReadHeap("y", "x")))))
    ))
    )
  )) in
  print_endline (ASTHRC.show root);
  print_endline (ASTHRC.modifiedVariables root |> ASTHRC.IdentifierSet.elements |> String.concat ", ");