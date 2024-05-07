module Node : sig
  type 'a t [@@deriving show]
  val make : 'a -> 'a t list -> 'a t

  (** given a node with its successors, returns the number of nodes *)
  val length : 'a t -> int
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
  val clone : 'a t -> 'a t
  
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

  val set_exp : 'a t -> int -> 'a -> 'a t

  (** updates the expression binded with id in cfg, or raises Not_found if id no exists in cfg *)
  val update : 'a t -> int -> 'a -> unit
end