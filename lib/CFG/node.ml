(** Recursive definition of CFG node, made by
 *  id,
 *  exp,
 *  successors,
 *  predecessors
 *)
module Node = struct
  type 'a t = { 
    id            : int;
    exp           : 'a;
    succ          : 'a t list;
    mutable pred  : int list;
  } [@@deriving show]

  (* Static counter for generating incremental IDs *)
  let counter = ref 0

  let next_id () =
    counter := !counter + 1;
    !counter

  let make (exp : 'a) (succ : 'a t list) : 'a t =
    {id = next_id(); exp = exp; succ = succ; pred = []}

  (** given a node, compute and set its predecessors and the predecessors of its successors *)
  let compute_pred (node : 'a t) : unit = 
    let rec aux = function
      | { id; exp=_; succ; _ } ->
        List.iter (fun x -> x.pred <- id::x.pred; aux x) succ;
    in aux node

  (** given a node with its successors, returns the number of nodes *)
  let rec length (node : 'a t) : int = match node.succ with
    | [] -> 1
    | [x] -> (length x)+1
    | ls -> (
      List.map (fun x -> length x) ls |>
      List.fold_left (+) 1
    )

end

(** Module for pretty printing ht *)
module Hashtbl = struct
  include Hashtbl

  let pp pp_key pp_value ppf values =
    Hashtbl.iter (fun key data ->
      Format.fprintf ppf "@[<1>%a: %a@]@." pp_key key pp_value data) values

end

(** The CFG is implemented as an Hashtable <id, (exp, predecessors, successors)> *)
module CFG = struct

  type 'a ht_item = {
    exp   : 'a;
    pred  : int list;
    succ  : int list;
  } [@@deriving show]

  type 'a t = Cfg of ((int, 'a ht_item) Hashtbl.t)
  [@@deriving show]

  let make_ht_item e ps ss = {exp = e; pred = ps; succ = ss}

  (** starting from a node (the root), builds a CFG *)
  let make (initial_node : string Node.t) : 'a t = 
    let h = Hashtbl.create (Node.length initial_node) in

    let rec aux (h : (int, 'a ht_item) Hashtbl.t) (node : string Node.t) : unit = 
      match node.succ with
      | [] -> (* the node has not successors, insert it *)
        Hashtbl.add h (node.id) (make_ht_item (node.exp) (node.pred) [])

      | x::xs ->  (* insert node' successors into the HT *)
        let id_succ = List.map (fun x -> x.Node.id) (x.succ) in
        Hashtbl.add h (x.id) (make_ht_item (x.exp) (x.pred) id_succ);
        List.iter (aux h) (x.succ);  
        List.iter (aux h) xs;

        (* insert node itself *)
        let id_node_succ = List.map (fun x -> x.Node.id) (node.succ) in
        Hashtbl.add h (node.id) (make_ht_item (node.exp) (node.pred) id_node_succ)
    in
    Node.compute_pred initial_node;
    aux h initial_node;
    Cfg(h)

  (** returns the current binding of id in cfg, or raises Not_found if no such binding exists *)
  let get (cfg : 'a t) (id : int) = match cfg with
    | Cfg(ht) -> Hashtbl.find ht id
  
  (** returns the successors identifiers of id in cfg, or raises Not_found if id no exists in cfg *)
  let succ_of (cfg : 'a t) (id : int) = (get cfg id).succ

  (** returns the predecessors identifiers of id in cfg, or raises Not_found if id no exists in cfg *)
  let pred_of (cfg : 'a t) (id : int) = (get cfg id).pred

  (** returns the expression binded with id in cfg, or raises Not_found if id no exists in cfg *)
  let get_exp (cfg : 'a t) (id : int) = (get cfg id).exp

end