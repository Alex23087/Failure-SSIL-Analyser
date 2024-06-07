open Ast.HeapRegularCommands
open Cfg__node

module Converter : sig
  val convert : 'a HeapRegularCommand.t -> 'a HeapAtomicCommand.t list Node.t
end
