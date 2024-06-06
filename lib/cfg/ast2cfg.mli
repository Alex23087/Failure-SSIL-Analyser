open Ast.HeapRegularCommands
open Base

module Ast2cfgConverter : sig
  val convert : 'a HeapRegularCommand.t -> 'a HeapAtomicCommand.t list Node.t
end
