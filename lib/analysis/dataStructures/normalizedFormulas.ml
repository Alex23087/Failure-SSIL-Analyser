open Analysis_DataStructures_Base

(** Normalized Logic Formulas

Logic formulas are normalized in Existential Disjunctive Normal Form, which easens the implementation of the analysis.
A normalized formula consists in:
- A set of existentialized identifiers.
- A list of disjoint formulas. Each disjoint is a formula composed only of atomic propositions, conjunctions and separate conjunctions.
- An annotation, the same type as of the non normalized formulas coming from the AST.

Additionally, a number of support information is kept:
- A so called phantom identifier, which is used to generate fresh names without having to rescan the names in the formulas.
*)
module NormalForm = struct
  module Formulas = struct
    include Ast.AnnotationLogic
    
    type t = unit Ast.AnnotationLogic.Formula.t
    [@@deriving show]

    type arithmetic_t = unit Ast.AnnotationLogic.ArithmeticExpression.t
    [@@deriving show]
  end

  type t = {
    variables: IdentifierSet.t; [@opaque]
    disjoints: Formulas.t list;
    last_phantom_id: int;
  }
  [@@deriving show]

  let make variables disjoints phantom_id =
    {variables; disjoints; last_phantom_id = phantom_id}
end