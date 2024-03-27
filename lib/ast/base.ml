(** Identifier type *)
type identifier = string [@@deriving show]

(** AnnotationType signature which is used to define ASTs' annotations *)
module type AnnotationType = sig
  type t
end

(** AnnotatedNode struct which represents ASTs' nodes and their annotations *)
module AnnotatedNode(Annotation: AnnotationType) = struct
  type annotation = Annotation.t
  type 'a t = {node: 'a; annotation: annotation [@opaque]} [@@deriving show]

  let make (node: 'a) (annotation: annotation) = {node; annotation}
end