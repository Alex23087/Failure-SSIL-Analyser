module Ast = struct
  type position = {line: int; column: int} [@@deriving show]

  let make_position (line: int) (column: int) = {line; column}

  type logic_formulas_annotation = {
    position: position;
  }
  [@@deriving show]

  module LogicFormulas = struct
    include Ast.AnnotationLogic(struct
      type t = logic_formulas_annotation
    end)

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

  module Commands = struct
    include Ast.HeapRegularCommands(struct
      type t = regular_formulas_annotation
    end)

    let annotate command line column formula =
      let make_annotation line column formula : AnnotatedNode.annotation =
        let position = make_position line column in
        {position; logic_formula = formula}
      in
      AnnotatedNode.make command (make_annotation line column formula)
  end
end