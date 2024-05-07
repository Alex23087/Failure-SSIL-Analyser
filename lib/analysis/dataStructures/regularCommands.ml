(** Concrete implementation of the Regular Commands, with source-position and logic formula in the annotation.
    
    Note that the logic formula annotates after the command, not before. *)

open Analysis_DataStructures_Base

include Ast.HeapRegularCommands

type annotation = {
  position: position;
  logic_formula: LogicFormulas.t option
}
[@@deriving show]

type t = annotation Ast.HeapRegularCommands.t
[@@deriving show]

let make_annotation line column formula : annotation =
  let position = make_position line column in
  {position; logic_formula = formula}