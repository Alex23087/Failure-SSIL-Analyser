(** This module contains the concrete implementation of the annotations for the Regular Commands
    and the Logic Formulas
    
    Important definitions:
    - {{! Ast.type-position}position} - position record to represent locations in source
     files
    - {{! Ast.LogicFormulas}LogicFormulas} - Logic Formulas concrete implementation, with {{! Ast.logic_formulas_annotation} annotations}
    - {{! Ast.Commands}Commands} - Regular Commands concrete implementation, with {{! Ast.regular_formulas_annotation} annotations}
    *)
module Ast = struct
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

    (** Utility function to build Logic Formulas' annotated nodes*)
    let annotate formula line column =
      let make_annotation line column : AnnotatedNode.annotation =
        let position = make_position line column in
        {position}
      in
      AnnotatedNode.make formula (make_annotation line column)
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

    (** Utility function to build Commands' annotated nodes*)
    let annotate command line column formula =
      let make_annotation line column formula : AnnotatedNode.annotation =
        let position = make_position line column in
        {position; logic_formula = formula}
      in
      AnnotatedNode.make command (make_annotation line column formula)
  end
end