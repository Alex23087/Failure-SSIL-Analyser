(** This module contains the concrete implementation of the Control Flow Graph data structures,
with the definition of the structures contained inside each block.

Important definitions:
- {{! NormalForm.t}Normal Form} - Logic formulas represented in Existential Disjunctive Normal Form.
- {{! Commands.t}Command} - Regular Commands with the updated annotation that contains formulas in normal form.
- {{! Cfg.t}CFG} - Control Flow Graph, instanced on the blocks' record.
- {{! Cfg.block}CFG Block} - CFG block record to represent sequences of atomic commands in source.
- {{! Cfg.item}CFG Item} - Control Flow Graph's node, which represent a block of commands, with their predecessor and successor blocks.
*)

open Analysis_DataStructures_Base

module NormalForm = struct
  type t = {
    variables: IdentifierSet.t; [@opaque]
    disjoints: LogicFormulas.t list;
    annotation: annotation;
    last_phantom_id: int;
  }
  and annotation = LogicFormulas.annotation
  [@@deriving show]

  let make variables disjoints annotation phantom_id =
    {variables; disjoints; annotation; last_phantom_id = phantom_id}
end

module Commands = struct
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

module Cfg = struct
  (** Control Flow Graph nodes' content. *)
  type block = {
    visit_count: int;
    precondition: NormalForm.t option;
    statements: Commands.t list;
  }

  include Cfg.CFG

  (** Control Flow Graph. *)
  type t = block Cfg.CFG.t

  (** Control Flow Graphs' node item. *)
  type item = block Cfg.CFG.item

  let update_precondition (block: block) (formula: NormalForm.t option) = 
    {
      visit_count = block.visit_count;
      precondition = formula;
      statements = block.statements
    }

  let update_statements (block: block) (statements: Commands.t list) =
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

  let update_formula_at (block: block) (update_idx: int) (formula: NormalForm.t option) =
    if update_idx = 0 then
      update_precondition block formula
    else (
      let map_fun idx (statement: Commands.t) = 
        if (idx + 1) = update_idx then
          Commands.update_postcondition statement formula
        else
          statement
      in
      let statements = List.mapi map_fun block.statements in
      update_statements block statements
    )

  let update_formula_at_last (block: block) (formula: NormalForm.t option) =
    match List.rev block.statements with
    | [] ->
      update_precondition block formula
    | statement::tail ->
      let statement = Commands.update_postcondition statement formula in
      let statements = List.rev (statement :: tail) in
      update_statements block statements
end