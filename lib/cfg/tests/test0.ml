open Ast
open Base
open Cfg_Tests_Utils
open HeapRegularCommands

let source = annotate (HeapRegularCommand.Command(
  annotate (HeapAtomicCommand.Allocation "x")
))

let (expected: ((unit HeapAtomicCommand.t) list Node.t)) =
  Node.make [ annotate (HeapAtomicCommand.Allocation "x") ] [] []

let%test_unit "test ast to cfg example" =
  [%test_eq: Node.t] (Node.structure_without_loops_destructive source) expected