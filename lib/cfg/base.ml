(** The module CFG_Node includes the modules
 *  Node: that provides the abstraction of a CFG node
 *  CFG: that provides the abstraction of a CFG
 *  The module CFG_Node also provides some internal functions 
 *    for working with these two modules without exposing their internals
 *)

(* Internal static counter for generating incremental IDs *)
let counter = ref 0

let next_id () =
  counter := !counter + 1;
  !counter

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

  let make (exp : 'a) (succ : 'a t list) : 'a t =
    {id = next_id(); exp = exp; succ = succ; pred = []}

  (** given a node with its successors, returns the number of nodes *)
  let rec length (node : 'a t) : int = match node.succ with
    | [] -> 1
    | [x] -> (length x)+1
    | ls -> (
      List.map (fun x -> length x) ls |>
      List.fold_left (+) 1
    )

end

(** Auxiliary module for providing Hashtbl of a pretty printing function *)
module Hashtbl = struct
  include Hashtbl

  let pp pp_key pp_value ppf values =
    Hashtbl.iter (fun key data ->
      Format.fprintf ppf "@[<1>%a: %a@]@." pp_key key pp_value data) values

end

(** given a node and an id, adds the latter to the predecessor list of the former *)
let add_pred (node : 'a Node.t) (pred : int) : unit = node.pred <- pred::(node.pred)

(** Auxiliary, internal function for computing and setting node predecessors *)
let compute_pred (node : 'a Node.t) : unit = 
  let rec aux = function
    | { Node.id; exp=_; succ; _ } ->
      List.iter (fun x -> (add_pred x id); aux x) succ;
  in aux node

(** The CFG is implemented as an Hashtable <id, (exp, predecessors, successors)> *)
module CFG = struct

  type 'a item = {
    exp   : 'a;
    pred  : int list;
    succ  : int list;
  } [@@deriving show]

  type 'a t = Cfg of ((int, 'a item) Hashtbl.t)
  [@@deriving show]

  let make_item exp pred succ = {exp = exp; pred = pred; succ = succ}

  (** starting from a node (the root), builds a CFG *)
  let make (initial_node : 'a Node.t) : 'a t = 
    let h = Hashtbl.create (Node.length initial_node) in

    let rec aux (h : (int, 'a item) Hashtbl.t) (node : 'a Node.t) : unit = 
      match node.succ with
      | [] -> (* the node has not successors, insert it *)
        Hashtbl.add h (node.id) (make_item (node.exp) (node.pred) [])

      | x::xs ->  (* insert node' successors into the HT *)
        let id_succ = List.map (fun (x : 'a Node.t) -> x.id) (x.succ) in
        Hashtbl.add h (x.id) (make_item (x.exp) (x.pred) id_succ);
        List.iter (aux h) (x.succ);  
        List.iter (aux h) xs;

        (* insert node itself *)
        let id_node_succ = List.map (fun (x : 'a Node.t) -> x.id) (node.succ) in
        Hashtbl.add h (node.id) (make_item (node.exp) (node.pred) id_node_succ)
    in
    compute_pred initial_node;
    aux h initial_node;
    Cfg(h)

  let clone (cfg : 'a t) = match cfg with
    | Cfg(ht) -> Cfg(Hashtbl.copy ht)

  let root (cfg : 'a t) =
    raise (Failure "not implemented")

  let idx (cfg : 'a t) (item: 'a item) =
    raise (Failure "not implemented")
    
  let fold (cfg : 'a t) (fn: 'a t -> 'a item -> 'b -> 'b) (acc: 'b) =
    raise (Failure "not implemented")
  
  (** returns the current binding of id in cfg, or raises Not_found if no such binding exists *)
  let get (cfg : 'a t) (id : int) = match cfg with
    | Cfg(ht) -> Hashtbl.find ht id
  
  (** returns the successors identifiers of id in cfg, or raises Not_found if id no exists in cfg *)
  let succ_of (cfg : 'a t) (id : int) = (get cfg id).succ

  (** returns the predecessors identifiers of id in cfg, or raises Not_found if id no exists in cfg *)
  let pred_of (cfg : 'a t) (id : int) = (get cfg id).pred

  (** returns the expression binded with id in cfg, or raises Not_found if id no exists in cfg *)
  let get_exp (cfg : 'a t) (id : int) = (get cfg id).exp

  (** updates the expression binded with id in cfg, or raises Not_found if id no exists in cfg *)
  let update (cfg : 'a t) (id : int) (expr: 'a) = 
    let item = get cfg id in
    match cfg with
    | Cfg(ht) -> Hashtbl.replace ht id (make_item expr item.pred item.succ)

end