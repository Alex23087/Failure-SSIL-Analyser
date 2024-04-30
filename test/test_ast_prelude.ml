open Lisproject.Prelude.Ast

let pos = make_position 0 1

let () =
  let formula =
    LogicFormulas.annotate (LogicFormulas.Formula.NonAllocated("x")) pos
  in
  let free_x =
    Commands.annotate (Commands.HeapRegularCommand.Command(
      Commands.annotate (Commands.HeapAtomicCommand.Free "x") pos (Option.Some(formula))
    )) pos Option.None
  in
  let assing_y =
    Commands.annotate (Commands.HeapRegularCommand.Command(
      Commands.annotate (Commands.HeapAtomicCommand.Assignment("x",
        Commands.annotate (Commands.ArithmeticExpression.Literal(50)) pos Option.None
      )) pos Option.None
    )) pos Option.None
  in
  let test_ast = Commands.annotate (Commands.HeapRegularCommand.Sequence(free_x, assing_y)) pos Option.None in
  print_endline (Commands.show test_ast)