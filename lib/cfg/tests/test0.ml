open Ast
open Base
open Ast2cfg
open Cfg_Tests_Utils
open HeapRegularCommands

let source =
  annotate ( HeapRegularCommand.Command (
      annotate ( HeapAtomicCommand.Allocation "x")
  ))

let (expected: ((unit HeapAtomicCommand.t) list Node.t)) =
  Node.make [ annotate (HeapAtomicCommand.Allocation "x") ] [] []

let%test_unit "test ast to cfg example" =
  [%test_eq: _ Node.t] (Ast2cfgConverter.convert source) expected
