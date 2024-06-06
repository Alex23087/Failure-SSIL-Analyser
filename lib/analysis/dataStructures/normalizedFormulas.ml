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