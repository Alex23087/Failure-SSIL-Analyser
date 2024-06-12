open Sexplib.Std
open Base

type identifier = Ast.identifier [@@deriving show, sexp, compare]
module AnnotatedNode = struct include Ast.AnnotatedNode end [@@deriving show, sexp, compare]
module IdentifierSet = struct include Ast.IdentifierSet end [@@deriving show, sexp, compare]

(** Position record, which holds where the given command or annotation is in the source files.*)
type position = {line: int; column: int} [@@deriving show, sexp, compare]

let make_position (line: int) (column: int) = {line; column}