open Lisproject.Ast.Prelude

(* Instantiate the AST with the annotation type *)
module ASTRC = RegularCommands(struct
  type t = int (* int annotations *)
end)

open ASTRC

let counter = ref 0

(* Annotate a node with a unique integer *)
let annotate (node: 'a): 'a AnnotatedNode.t =
  let out = AnnotatedNode.make node !counter in
  counter := !counter + 1;
  out

let () =
  (* Create an AST corresponding to the RegCmd:  x = 1; (x < 10?; x = x + 1)*; !(x < 10)?
     This encodes the command:  x = 1; while(x<10){x = x + 1}  *)
  let root = annotate (RegularCommand.Sequence(
    annotate (RegularCommand.Command(annotate (AtomicCommand.Assignment("x", annotate (ArithmeticExpression.Literal 1))))),

    annotate (RegularCommand.Sequence(
      annotate (RegularCommand.Star(
        annotate (RegularCommand.Sequence(
          annotate (RegularCommand.Command(annotate (AtomicCommand.Guard(annotate (BooleanExpression.Comparison(
            BooleanComparison.LessThan,
            annotate (ArithmeticExpression.Variable "x"),
            annotate (ArithmeticExpression.Literal 10)
          )))))),
          annotate (RegularCommand.Command(annotate (AtomicCommand.Assignment("x", annotate (
            ArithmeticExpression.BinaryOperation(
              ArithmeticOperation.Plus,
              annotate (ArithmeticExpression.Variable "x"),
              annotate (ArithmeticExpression.Literal 1)))))))
        ))
      )),

      annotate (RegularCommand.Command(annotate (AtomicCommand.Guard(annotate (BooleanExpression.Not(annotate (BooleanExpression.Comparison(
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
  print_endline (show root)