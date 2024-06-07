module Prelude = Analysis_Prelude

(** {1 Data Structures}

Data structures used throughout the analysis. The submodules {{! DataStructures.Analysis}Analysis} and {{! DataStructures.Parser}Parser} specify
data structures used in those specific points of the software.
*)
module DataStructures = DataStructures

(** {1 Formula Normalization}

This module implements functions to transform generic formulas into Existential Disjunctive Normal Form,
plus other utility functions to compute operations on normalized formulas.
*)
module Normalization = Normalization
module ExpressionSubstitution = ExpressionSubstitution
module Atomic = Atomic