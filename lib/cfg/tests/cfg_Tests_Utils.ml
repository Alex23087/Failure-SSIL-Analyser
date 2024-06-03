open Ast
open Sexplib.Std
open Ppx_compare_lib.Builtin

type t = unit HeapRegularCommands.t
[@@deriving show, sexp, compare]

let annotate node =
  AnnotatedNode.make node ()