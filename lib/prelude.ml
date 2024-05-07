(** This module contains the concrete implementation of the annotations for the Regular Commands
    and the Logic Formulas
    
    Important definitions:
    - {{! Ast.type-position}position} - position record to represent locations in source files.
    - {{! Ast.LogicFormulas}LogicFormulas} - Logic Formulas concrete implementation, with {{! Ast.logic_formulas_annotation} annotations}.
    - {{! Ast.Commands}Commands} - Regular Commands concrete implementation, with {{! Ast.regular_formulas_annotation} annotations}.
    *)
module Ast = struct
  type identifier = Ast.identifier
  module AnnotatedNode = struct include Ast.AnnotatedNode end [@@deriving show]
  module IdentifierSet = struct include Ast.IdentifierSet end [@@deriving show]

  (** Position record, which holds where the given command or annotation is in the source files.*)
  type position = {line: int; column: int} [@@deriving show]

  let make_position (line: int) (column: int) = {line; column}

  (** Concrete implementation of the Logic Formulas, with source-position annotations*)
  module LogicFormulas = struct
    include Ast.AnnotationLogic

    type annotation = {
      position: position;
    }
    [@@deriving show]

    type t = annotation Ast.AnnotationLogic.t
    [@@deriving show]

    let make_annotation line column : annotation =
      let position = make_position line column in {position}
  end

  (** Concrete implementation of the Regular Commands, with source-position and logic formula in the annotation.
      
      Note that the logic formula annotates after the command, not before. *)
  module Commands = struct
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
  end

  module NormalForm = struct
    type t = {
      variables: IdentifierSet.t; [@opaque]
      disjoints: LogicFormulas.t list;
      annotation: LogicFormulas.annotation;
      last_phantom_id: int;
    }
    [@@deriving show]

    let make variables disjoints annotation phantom_id =
      {variables; disjoints; annotation; last_phantom_id = phantom_id}
  end

  module AnalysisCommands = struct
    type annotation = {
      position: position;
      postcondition: NormalForm.t option;
    }
    [@@deriving show]

    type t = annotation Ast.HeapRegularCommands.HeapAtomicCommand.t
    [@@deriving show]

    let get_postcondition (command: ('a, annotation) AnnotatedNode.t) =
      command.annotation.postcondition

    let update_postcondition (command: ('a, annotation) AnnotatedNode.t) postcondition =
      let annotation = {
        position = command.annotation.position;
        postcondition
      } in
      AnnotatedNode.update_annotation command annotation
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
  type block = {
    visit_count: int;
    precondition: Ast.NormalForm.t option;
    statements: Ast.AnalysisCommands.t list;
  }

  include Cfg.CFG

  (** Control Flow Graph. *)
  type t = block Cfg.CFG.t
  
  (** Control Flow Graphs' node item. *)
  type item = block Cfg.CFG.item

  let update_precondition (block: block) (formula: Ast.NormalForm.t option) = 
    {
      visit_count = block.visit_count;
      precondition = formula;
      statements = block.statements
    }

  let update_statements (block: block) (statements: Ast.AnalysisCommands.t list) =
    {
      visit_count = block.visit_count;
      precondition = block.precondition;
      statements = statements
    }

  let increase_visit_count (block: block) = 
    {
      visit_count = block.visit_count + 1;
      precondition = block.precondition;
      statements = block.statements
    }

  let update_formula_at (block: block) (update_idx: int) (formula: Ast.NormalForm.t option) =
    if update_idx = 0 then
      update_precondition block formula
    else (
      let map_fun idx (statement: Ast.AnalysisCommands.t) = 
        if (idx + 1) = update_idx then
          Ast.AnalysisCommands.update_postcondition statement formula
        else
          statement
      in
      let statements = List.mapi map_fun block.statements in
      update_statements block statements
    )

  let update_formula_at_last (block: block) (formula: Ast.NormalForm.t option) =
    match List.rev block.statements with
    | [] ->
      update_precondition block formula
    | statement::tail ->
      let statement = Ast.AnalysisCommands.update_postcondition statement formula in
      let statements = List.rev (statement :: tail) in
      update_statements block statements
    
end