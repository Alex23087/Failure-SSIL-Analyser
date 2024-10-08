(** The module provides the abstraction of a CFG node. *)
module Node : sig
  type 'a t [@@deriving show, sexp]

  (** Checks if two nodes and their children are the same recursively.

      Notes: Should always work for the structure generated by {{! Cfg.Converter}Converter}.
      Severely limited, only checks isomorphism for two children for each node,
      also doesn't return true for every isomophic graphs that have two
      children for each node, only for some *)
  val compare : 'a t -> 'a t -> bool

  (** [length node] computes the number of nodes in the whole nodes' tree *)
  val length : 'a t -> int

  (** [make data succ prec] creates a new unique node using [data] as its content,
    [succ] the list of successors and [prec] the list of precedessors identifiers. *)
  val make : 'a -> 'a t list -> int list -> 'a t

  (** [make id data succ prec] creates a new node using [id] as its identifiers, [data] as its content,
    [succ] the list of successors and [prec] the list of precedessors identifiers.
    
    *Warning* if the same id as another node is chosen, other methods might return wrong results.
   *)
  val make_with_id : int -> 'a -> 'a t list -> int list -> 'a t

  (** Get a node's unique identifier *)
  val get_id : 'a t -> int

  (** Get a node's associated data *)
  val get_exp : 'a t -> 'a

  (** Get a node's successors list *)
  val get_succ : 'a t -> 'a t list

  (** Get a node's predecessors' identifier list *)
  val get_pred : 'a t -> int list

  (** [add_succ node succ] adds the node [succ] to the list of successors of [node] *)
  val add_succ : 'a t -> 'a t -> unit

  (** Replace the metadata of the node *)
  val set_exp : 'a t -> 'a -> unit

  (** Replace the succ list of the node *)
  val set_succ : 'a t -> 'a t list -> unit

  (** Remove a node from the predecessors *)
  val remove_pred : 'a t -> int -> unit

  (** Modifies the succ lists such that no loops are present, destroyes
      the original topology, pred lists are not modified *)
  val structure_without_loops_destructive : 'a t -> unit

  (** Given a nodes' tree only composed of valid successor lists, computes the precedessors identifiers. *)
  val compute_pred : 'a t -> unit
  
  val to_string : 'a t -> ('a -> string) -> string
end
