module Node : sig
  type 'a t [@@deriving show, sexp]
  val compare : 'a t -> 'a t -> bool

  val make : 'a -> 'a t list -> int list -> 'a t

  val makeWithId : int -> 'a -> 'a t list -> int list -> 'a t

  val getnodeid : 'a t -> int

  val getexp : 'a t -> 'a

  (** given a node with its successors, returns the number of nodes *)
  val length : 'a t -> int

  val addsucc : 'a t -> 'a t -> unit

  val setsucc : 'a t -> 'a t list -> unit

  val replaceexp : 'a t -> 'a -> unit

  val structure_without_loops_destructive : 'a t -> unit

  val succ : 'a t -> 'a t list

  val prev : 'a t -> int list

  val compute_pred : 'a t -> unit
end
