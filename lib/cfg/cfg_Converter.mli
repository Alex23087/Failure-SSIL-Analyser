open Ast.HeapRegularCommands
open Cfg_Node

(** The module Cfg__converter provides the module
 *  Converter: that provides a method convert that given an AST converts it
 *  to a CFG composed by nodes from the Cfg__node module.
 *  See {{! Cfg.CFG.make}make} to convert the resulting structure to a
 *  CFG.
 *)
module Converter : sig
  val convert : ?keep_structure:bool -> 'a HeapRegularCommand.t -> 'a HeapAtomicCommand.t list Node.t
end
