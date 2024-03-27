open Lisproject.Ast_coherent_logic

(* Instantiate the AST with the annotation type *)
module ASTCC = ASTCoherentCommands(struct
  type t = int (* int annotations *)
end)

let counter = ref 0

(* Annotate a node with a unique integer *)
let annotate (node: 'a): 'a ASTCC.annotated_node =
  let out: 'a ASTCC.annotated_node = ASTCC.addAnnotation node !counter in
  counter := !counter + 1;
  out


let () =
  (* Create an AST corresponding to:  ((x<5) and (x+1==y%2)) or (exists p.(p<=x or p>=y*2)) *)
  let root = annotate (ASTCC.CoherentCommand.Or(
                 annotate (ASTCC.CoherentCommand.And(
                     annotate (ASTCC.CoherentCommand.Comparison(
                         ASTCC.BinaryComparison.LessThan,
                         annotate (ASTCC.ArithmeticExpression.Variable "x"),
                         annotate (ASTCC.ArithmeticExpression.Literal 5)
                       )),
                     annotate (ASTCC.CoherentCommand.Comparison(
                         ASTCC.BinaryComparison.Equals,
                         annotate (ASTCC.ArithmeticExpression.Operation(
                             ASTCC.BinaryOperator.Plus,
                             annotate (ASTCC.ArithmeticExpression.Variable "x"),
                             annotate (ASTCC.ArithmeticExpression.Literal 1)
                           )),
                         annotate (ASTCC.ArithmeticExpression.Operation(
                             ASTCC.BinaryOperator.Modulo,
                             annotate (ASTCC.ArithmeticExpression.Variable "y"),
                             annotate (ASTCC.ArithmeticExpression.Literal 2)
                           ))
                       ))
                   )),
                 annotate (ASTCC.CoherentCommand.Exists(
                     "p",
                     annotate (ASTCC.CoherentCommand.Or(
                         annotate (ASTCC.CoherentCommand.Comparison(
                             ASTCC.BinaryComparison.LessOrEqual,
                             annotate (ASTCC.ArithmeticExpression.Variable "p"),
                             annotate (ASTCC.ArithmeticExpression.Variable "x")
                           )),
                         annotate (ASTCC.CoherentCommand.Comparison(
                             ASTCC.BinaryComparison.GreaterOrEqual,
                             annotate (ASTCC.ArithmeticExpression.Variable "p"),
                             annotate (ASTCC.ArithmeticExpression.Operation(
                                 ASTCC.BinaryOperator.Times,
                                 annotate (ASTCC.ArithmeticExpression.Variable "y"),
                                 annotate (ASTCC.ArithmeticExpression.Literal 2)
                               ))
                           ))
                       ))
                   ))
  )) in
  match ASTCC.removeAnnotation root with
    | Or(a, _) -> (match ASTCC.removeAnnotation a with
      | And(a, _) -> (match ASTCC.removeAnnotation a with
        | Comparison(a, b, c) -> (match (a, b.node, c.node) with
          | (LessThan, Variable(x), Literal(y)) -> print_endline ("Less: " ^ x ^ " < " ^ (string_of_int y))
          | _ -> print_endline "Not Less")
        | _ -> print_endline "Not Comp")
      | _ -> print_endline "Not And")
    | _ -> print_endline "Not Or";

  (* Print it with ASTCC.show_rcmd *)
  print_endline (ASTCC.show root)
