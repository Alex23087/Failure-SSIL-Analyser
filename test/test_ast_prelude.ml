open Lisproject.Prelude.Ast

let () =
  let formula =
    LogicFormulas.annotate_parser (LogicFormulas.Formula.NonAllocated("x")) 0 1
  in
  let free_x =
    Commands.annotate_parser (Commands.HeapRegularCommand.Command(
      Commands.annotate_parser (Commands.HeapAtomicCommand.Free "x") 0 1 (Option.Some(formula))
    )) 0 1 Option.None
  in
  let assing_y =
    Commands.annotate_parser (Commands.HeapRegularCommand.Command(
      Commands.annotate_parser (Commands.HeapAtomicCommand.Assignment("x",
        Commands.annotate_parser (Commands.ArithmeticExpression.Literal(50)) 0 1 Option.None
      )) 0 1 Option.None
    )) 0 1 Option.None
  in
  let test_ast = Commands.annotate_parser (Commands.HeapRegularCommand.Sequence(free_x, assing_y)) 0 1 Option.None in
  print_endline (Commands.show test_ast)