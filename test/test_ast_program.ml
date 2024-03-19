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
  let root = annotate (ASTRC.Seq(
    annotate (ASTRC.Command(annotate (ASTRC.Assign("x", annotate (ASTRC.Literal 1))))),

    annotate (ASTRC.Seq(
      annotate (ASTRC.Star(
        annotate (ASTRC.Seq(
          annotate (ASTRC.Command(annotate (ASTRC.Guard(annotate (ASTRC.Comp(
            ASTRC.Lt,
            annotate (ASTRC.Var "x"),
            annotate (ASTRC.Literal 10)
          )))))),
          annotate (ASTRC.Command(annotate (ASTRC.Assign("x", annotate (ASTRC.Binop(ASTRC.Plus, annotate (ASTRC.Var "x"), annotate (ASTRC.Literal 1)))))))
        ))
      )),

      annotate (ASTRC.Command(annotate (ASTRC.Guard(annotate (ASTRC.Not(annotate (ASTRC.Comp(
        ASTRC.Lt,
        annotate (ASTRC.Var "x"),
        annotate (ASTRC.Literal 10)
      ))))))))
    ))
  )) in
  (* Print it with ASTRC.show_rcmd *)
  print_endline (ASTRC.show_rcmd root)