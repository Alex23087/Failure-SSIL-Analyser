open Lisproject.Ast_program

(* Instantiate the AST with the annotation type *)
module ASTRC = ASTRegularCommands(struct
  type t = int (* int annotations *)
end)

let counter = ref 0

(* Annotate a node with a unique integer *)
let annotate (node: 'a): 'a ASTRC.annotated_node =
  let out: 'a ASTRC.annotated_node = {node; annotation = !counter} in
  counter := !counter + 1;
  out

let () =
  (* Create an AST corresponding to the RegCmd:  x = 1; (x < 10?; x = x + 1)*; !(x < 10)?    
     This encodes the command:  x = 1; while(x<10){x = x + 1}  *)
  let root = annotate (ASTRC.RegularCommand.Sequence(
    annotate (ASTRC.RegularCommand.Command(annotate (ASTRC.BasicCommand.Assignment("x", annotate (ASTRC.ArithmeticExpression.Literal 1))))),

    annotate (ASTRC.RegularCommand.Sequence(
      annotate (ASTRC.RegularCommand.Star(
        annotate (ASTRC.RegularCommand.Sequence(
          annotate (ASTRC.RegularCommand.Command(annotate (ASTRC.BasicCommand.Guard(annotate (ASTRC.BooleanExpression.Comparison(
            ASTRC.BooleanComparison.LessThan,
            annotate (ASTRC.ArithmeticExpression.Variable "x"),
            annotate (ASTRC.ArithmeticExpression.Literal 10)
          )))))),
          annotate (ASTRC.RegularCommand.Command(annotate (ASTRC.BasicCommand.Assignment("x", annotate (
            ASTRC.ArithmeticExpression.BinaryOperation(
              ASTRC.ArithmeticOperation.Plus,
              annotate (ASTRC.ArithmeticExpression.Variable "x"),
              annotate (ASTRC.ArithmeticExpression.Literal 1)))))))
        ))
      )),

      annotate (ASTRC.RegularCommand.Command(annotate (ASTRC.BasicCommand.Guard(annotate (ASTRC.BooleanExpression.Not(annotate (ASTRC.BooleanExpression.Comparison(
        ASTRC.BooleanComparison.LessThan,
        annotate (ASTRC.ArithmeticExpression.Variable "x"),
        annotate (ASTRC.ArithmeticExpression.Literal 10)
      ))))))))
    ))
  )) in
  match ASTRC.removeAnnotation root with
    | Command(a) -> (match ASTRC.removeAnnotation a with
      | Guard(a) -> (match ASTRC.removeAnnotation a with
        | Comparison(a, b, c) -> (match (a, b.node, c.node) with
          | (Equal, Variable(x), Literal(y)) -> print_endline ("Equal: " ^ x ^ " = " ^ (string_of_int y))
          | _ -> print_endline "Not Equal")
        | _ -> print_endline "Not Comp")
      | _ -> print_endline "Not Seq")
    | _ -> print_endline "Not Star";

  (* Print it with ASTRC.show_rcmd *)
  print_endline (ASTRC.show root)