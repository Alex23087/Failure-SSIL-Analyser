(**{1 Control Flow Graph}
*)

(** Definition of a Node data structures to represent the CFG *)
module Node = Cfg__node.Node

(** Definition of the CFG data structure *)
module CFG = Cfg__cfg.CFG

(** Definition of a converter from AST to Node data structure for CFG *)
module Converter = Cfg__converter.Converter
