open Ast
open Base
open Ast2cfg
open Sexplib.Std
open Ppx_compare_lib.Builtin

type t = unit HeapRegularCommands.t
[@@deriving show, sexp, compare]

let annotate node =
  AnnotatedNode.make node ()

let convert_for_star node =
  let out = Ast2cfgConverter.convert node in
  Node.structure_without_loops_destructive out;
  out
