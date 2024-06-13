(** Concrete implementation of the Logic Formulas, with source-position annotations *)

open Analysis_DataStructures_Base

include Ast.AnnotationLogic

type annotation = {
  position: position;
}
[@@deriving show]

type t = annotation Ast.AnnotationLogic.Formula.t
[@@deriving show]

type arithmetic_t = annotation Ast.AnnotationLogic.ArithmeticExpression.t
[@@deriving show]

let make_annotation line column : annotation =
  let position = make_position line column in {position}

let make_annotation_position position = { position}