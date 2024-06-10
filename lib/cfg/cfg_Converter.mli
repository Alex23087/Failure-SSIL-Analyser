open Ast.HeapRegularCommands
open Cfg_Node

(** The module Converter provides a method [convert], that given an AST, it converts it
   into a CFG composed by nodes from the {{! Cfg.Node}Node} module.
   See {{! Cfg.CFG.make}make} to convert the resulting structure to a CFG.
 *)
module Converter : sig
  val convert : ?keep_structure:bool -> 'a HeapRegularCommand.t -> 'a HeapAtomicCommand.t list Node.t
end
