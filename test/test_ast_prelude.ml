open Lisproject.Prelude.Ast

let () =
  let formula =
    LogicFormulas.annotate (LogicFormulas.Formula.NonAllocated("x")) 0 1
  in
  let free_x =
    Commands.annotate (Commands.HeapRegularCommand.Command(
      Commands.annotate (Commands.HeapAtomicCommand.Free "x") 0 1 (Option.Some(formula))
    )) 0 1 Option.None
  in
  let assing_y =
    Commands.annotate (Commands.HeapRegularCommand.Command(
      Commands.annotate (Commands.HeapAtomicCommand.Assignment("x",
        Commands.annotate (Commands.ArithmeticExpression.Literal(50)) 0 1 Option.None
      )) 0 1 Option.None
    )) 0 1 Option.None
  in
  let test_ast = Commands.annotate (Commands.HeapRegularCommand.Sequence(free_x, assing_y)) 0 1 Option.None in
  print_endline (Commands.show test_ast)