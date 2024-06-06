type identifier = Ast.identifier
module AnnotatedNode = struct include Ast.AnnotatedNode end [@@deriving show]
module IdentifierSet = struct include Ast.IdentifierSet end [@@deriving show]

(** Position record, which holds where the given command or annotation is in the source files.*)
type position = {line: int; column: int} [@@deriving show]

let make_position (line: int) (column: int) = {line; column}