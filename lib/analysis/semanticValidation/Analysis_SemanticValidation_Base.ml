open DataStructures
open Parser

let validate_ast (ast: Commands.t) = 
  let rec validate_regular_command (cmd: Commands.t) =
    match cmd.node with
    | Command(cmd) -> validate_atomic_command cmd
    | Sequence(left, right) -> validate_regular_command left && validate_regular_command right
    | NondeterministicChoice(left, right) -> validate_regular_command left && validate_regular_command right
    | Star(cmd) -> validate_regular_command cmd
  and validate_atomic_command (cmd: Commands.atomic_t) =
    match cmd.node with
    | Skip -> true
    | Assignment(_, _) -> true
    | NonDet(_) -> true
    | Guard(_) -> true
    | Allocation(_) -> true
    | Free(_) -> true
    | ReadHeap(var, heap_read) -> not (String.equal var heap_read)
    | WriteHeap(heap, expr) ->
      let expr = Analysis_Utils.command_expression_to_logic_expression expr (fun _ -> ()) in
      not (Analysis_Utils.identifier_in_expr expr heap)
  in
  validate_regular_command ast