type identifier = string [@@deriving show]

module type AnnotationType = sig
  type t
end

module AnnotatedNode(Annotation: AnnotationType) = struct
  type annotation = Annotation.t
  type 'a t = {node: 'a; annotation: annotation [@opaque]} [@@deriving show]

  let make (node: 'a) (annotation: annotation) = {node; annotation}
  let annotation (node: 'a t) = node.annotation
  let node (node: 'a t) = node.node
end