open Sexplib.Std
open Ppx_compare_lib.Builtin

(**{1 Support data structures}*)

(**Identifier type*)
type identifier = string [@@deriving show, sexp, compare]

(**AnnotationType signature which is used to define ASTs' annotations *)
module type AnnotationType = sig
  type t [@@deriving show, sexp, compare]
end

(**AnnotatedNode struct which represents ASTs' nodes and their annotations*)
module AnnotatedNode(Annotation: AnnotationType) = struct
  type annotation = Annotation.t [@@deriving show, sexp, compare]
  type 'a t = {node: 'a; annotation: annotation } [@@deriving show, sexp, compare]

  let make (node: 'a) (annotation: annotation) = {node; annotation}
  let unpack (annotated_node: 'a t) = (annotated_node.node, annotated_node.annotation)
  let annotation (annotated_node: 'a t) = annotated_node.annotation
  let node (annotated_node: 'a t) = annotated_node.node
end

(**IdentifierSet*)
module IdentifierSet = Set.Make(struct
  type t = identifier
  let compare = compare
end)