(** This module contains the concrete implementation of the annotations for the Regular Commands
    and the Logic Formulas
    
    Important definitions:
    - {{! Ast.type-position}position} - position record to represent locations in source files.
    - {{! Ast.LogicFormulas}LogicFormulas} - Logic Formulas concrete implementation, with {{! Ast.logic_formulas_annotation} annotations}.
    - {{! Ast.Commands}Commands} - Regular Commands concrete implementation, with {{! Ast.regular_formulas_annotation} annotations}.
    *)
module Ast = struct
  type identifier = Ast.identifier
  module IdentifierSet = struct include Ast.IdentifierSet end

  (** Position record, which holds where the given command or annotation is in the source files.*)
  type position = {line: int; column: int} [@@deriving show]

  let make_position (line: int) (column: int) = {line; column}

  type logic_formulas_annotation = {
    position: position;
  }
  [@@deriving show]

  (** Concrete implementation of the Logic Formulas, with source-position annotations*)
  module LogicFormulas = struct
    include Ast.AnnotationLogic(struct
      type t = logic_formulas_annotation
    end)

    let make_annotation line column : AnnotatedNode.annotation =
      let position = make_position line column in {position}

    (** Utility functions to build Logic Formulas' annotated nodes*)
    let annotate formula annotation =
      AnnotatedNode.make formula annotation
      
    let annotate_parser formula line column =
      AnnotatedNode.make formula (make_annotation line column)

    (** Utility function to update a logic formula*)
    let update_formula annotated_node new_formula =
      let _, annotation = AnnotatedNode.unpack annotated_node in 
      AnnotatedNode.make new_formula annotation
  end

  type regular_formulas_annotation = {
    position: position;
    logic_formula: LogicFormulas.t option
  }
  [@@deriving show]

  (** Concrete implementation of the Regular Commands, with source-position and logic formula in the annotation.

      Note that the logic formula annotates after the command, not before. *)
  module Commands = struct
    include Ast.HeapRegularCommands(struct
      type t = regular_formulas_annotation
    end)

    let make_annotation line column formula : AnnotatedNode.annotation =
      let position = make_position line column in
      {position; logic_formula = formula}

    (** Utility functions to build Commands' annotated nodes*)
    let annotate formula annotation =
      AnnotatedNode.make formula annotation

    let annotate_parser command line column formula =
      AnnotatedNode.make command (make_annotation line column formula)

    (** Utility function to update a Command's logic formula*)
    let update_formula annotated_node new_formula =
      let node, annotation = AnnotatedNode.unpack annotated_node in 
      let position = annotation.position in
      AnnotatedNode.make node (make_annotation position.line position.column new_formula)
  end
end

(** This module contains the concrete implementation of the Control Flow Graph data structures.

    Important definitions:
    - {{! Cfg.cfg_block}block} - CFG block record to represent sequences of atomic commands in source.
    - {{! Cfg.cfg}cfg} - Control Flow Graph, instanced on the blocks' record.
    - {{! Cfg.cfg_item}item} - Control Flow Graph's node, which represent a block of commands, with their predecessor and successor blocks.
    *)
module Cfg = struct
  (** Control Flow Graph nodes' content. *)
  type cfg_block = {
    visit_count: int;
    precondition: Ast.LogicFormulas.t option;
    statements: Ast.Commands.HeapAtomicCommand.t list;
  }

  include Cfg.CFG

  (** Control Flow Graph. *)
  type cfg = cfg_block Cfg.CFG.t
  
  (** Control Flow Graphs' node item. *)
  type cfg_item = cfg_block Cfg.CFG.item
end