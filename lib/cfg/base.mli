module Node : sig
  type 'a t [@@deriving show]
  val make : 'a -> 'a t list -> int list -> 'a t

  val getnodeid: 'a t -> int

  (** given a node with its successors, returns the number of nodes *)
  val length : 'a t -> int

  (** given a node and an id, adds the latter to the predecessor list of the former *)
  val addsucc : 'a t -> 'a t -> unit
  val concat : 'a t -> 'a t -> unit

  val structure_without_loops_destructive : 'a t -> unit
end

module Hashtbl : sig
  val pp : (Format.formatter -> 'a -> unit) ->
    (Format.formatter -> 'b -> unit) ->
    Format.formatter -> ('a, 'b) Hashtbl.t -> unit
end

module CFG : sig
  type 'a t [@@deriving show]
  type 'a item [@@deriving show]
  val make : 'a Node.t -> 'a t
  
  val root : 'a t -> 'a item

  val idx : 'a t -> 'a item -> int

  val fold : 'a t -> ('a t -> 'a item -> 'b -> 'b) -> 'b -> 'b

  (** returns the current binding of id in cfg, or raises Not_found if no such binding exists *)
  val get : 'a t -> int -> 'a item

  (** returns the successors identifiers of id in cfg, or raises Not_found if id no exists in cfg *)
  val succ_of : 'a t -> int -> int list

  (** returns the predecessors identifiers of id in cfg, or raises Not_found if id no exists in cfg *)
  val pred_of : 'a t -> int -> int list

  (** returns the expression binded with id in cfg, or raises Not_found if id no exists in cfg *)
  val get_exp : 'a t -> int -> 'a

  (** updates the expression bound with id in cfg, or raises Not_found if id no exists in cfg *)
  val set_exp : 'a t -> int -> 'a -> 'a t
end
