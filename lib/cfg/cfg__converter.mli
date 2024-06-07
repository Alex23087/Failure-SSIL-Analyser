open Ast.HeapRegularCommands
open Cfg__node

(** The module Cfg__converter provides the module
 *  Converter: that provides a method convert that given an AST converts it
 *  to a CFG composed by nodes from the Cfg__node module.
 *  See {!module:Cfg__cfg.CFG.make} to convert the resulting structure to a
 *  CFG.
 *)
module Converter : sig
  val convert : 'a HeapRegularCommand.t -> 'a HeapAtomicCommand.t list Node.t
end
