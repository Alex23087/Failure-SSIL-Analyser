open Lisproject.Ast.Prelude

(* Instantiate the AST with the annotation type *)
module ASTCC = CoherentFormulas(struct
  type t = int (* int annotations *)
end)

open ASTCC

let counter = ref 0

(* Annotate a node with a unique integer *)
let annotate (node: 'a): 'a AnnotatedNode.t =
  let out = AnnotatedNode.make node !counter in
  counter := !counter + 1;
  out


let () =
  (* Create an AST corresponding to:  ((x<5) and (x+1==y%2)) or (exists p.(p<=x or p>=y*2)) *)
  let root =  annotate (CoherentFormula.Or(
                annotate (CoherentFormula.And(
                  annotate (CoherentFormula.Comparison(
                    BinaryComparison.LessThan,
                    annotate (ArithmeticExpression.Variable "x"),
                    annotate (ArithmeticExpression.Literal 5)
                  )),
                  annotate (CoherentFormula.Comparison(
                    BinaryComparison.Equals,
                    annotate (ArithmeticExpression.Operation(
                      BinaryOperator.Plus,
                      annotate (ArithmeticExpression.Variable "x"),
                      annotate (ArithmeticExpression.Literal 1)
                    )),
                    annotate (ArithmeticExpression.Operation(
                      BinaryOperator.Modulo,
                      annotate (ArithmeticExpression.Variable "y"),
                      annotate (ArithmeticExpression.Literal 2)
                    ))
                  ))
                )),
                annotate (CoherentFormula.Exists(
                  "p",
                  annotate (CoherentFormula.Or(
                    annotate (CoherentFormula.Comparison(
                      BinaryComparison.LessOrEqual,
                      annotate (ArithmeticExpression.Variable "p"),
                      annotate (ArithmeticExpression.Variable "x")
                    )),
                    annotate (CoherentFormula.Comparison(
                      BinaryComparison.GreaterOrEqual,
                      annotate (ArithmeticExpression.Variable "p"),
                      annotate (ArithmeticExpression.Operation(
                        BinaryOperator.Times,
                        annotate (ArithmeticExpression.Variable "y"),
                        annotate (ArithmeticExpression.Literal 2)
                      ))
                    ))
                  ))
                ))
              )) in
  match root.node with
    | Or(a, _) -> (match a.node with
      | And(a, _) -> (match a.node with
        | Comparison(a, b, c) -> (match (a, b.node, c.node) with
          | (LessThan, Variable(x), Literal(y)) -> print_endline ("Less: " ^ x ^ " < " ^ (string_of_int y))
          | _ -> print_endline "Not Less")
        | _ -> print_endline "Not Comp")
      | _ -> print_endline "Not And")
    | _ -> print_endline "Not Or";

  (* Print it with show_rcmd *)
  print_endline (show root)
