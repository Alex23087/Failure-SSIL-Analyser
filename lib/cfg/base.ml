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
  open Sexplib.Std
  open Ppx_compare_lib.Builtin

  type 'a t = {
    id            : int;
    mutable exp   : 'a;
    mutable succ  : 'a t list;
    mutable pred  : int list;
  } [@@deriving show, sexp, compare]

  let make (exp: 'a) (succ: 'a t list) (pred: int list) : 'a t =
    {id = next_id(); exp = exp; succ = succ; pred = pred}

  let getnodeid (node: 'a t): int = node.id

  (** given a node with its successors, returns the number of nodes *)
  let rec length (node : 'a t) : int = match node.succ with
    | [] -> 1
    | [x] -> (length x)+1
    | ls -> (
      List.map (fun x -> length x) ls |>
        List.fold_left (+) 1
    )

  let addsucc (node : 'a t) (succ : 'a t) : unit =
    node.succ <- succ    :: node.succ;
    succ.pred <- node.id :: succ.pred

  let getexp (node : 'a t) : 'a =
    node.exp

  let setsucc (node : 'a t) (succ : 'a t list) : unit =
    List.iter (fun x -> (* remove all backwards links*)
        x.pred <- List.filter (fun x -> x != node.id) x.pred
      ) node.succ;
    List.iter (fun x -> (* add new backwards links *)
        x.pred <- node.id :: x.pred
      ) succ;
    node.succ <- succ

  let replaceexp (node : 'a t) (newexp : 'a) : unit =
    node.exp <- newexp

  let rec structure_without_loops_destructive (node : 'a t) : unit =
    (* recursive protection *)
    let alreadyvisited = ref [] in
    let rec helper (node : 'a t) : unit =
      if List.mem node.id !alreadyvisited then
        ();
      alreadyvisited := node.id :: !alreadyvisited;
      node.succ <- List.filter (fun x -> not (List.mem x.id !alreadyvisited)) node.succ;
      List.iter helper node.succ
    in
    helper node

  let succ (node : 'a t) : 'a t list =
    node.succ

  let prev (node : 'a t) : int list =
    node.pred
end

(** given a node and an id, adds the latter to the predecessor list of the former
 *)
let add_pred (node : 'a Node.t) (pred : int) : unit =
  node.pred <- pred::(node.pred)

(** Auxiliary, internal function for computing and setting node predecessors *)
let compute_pred (node : 'a Node.t) : unit =
  let rec aux = function
    | { Node.id; exp=_; succ; _ } ->
       List.iter (fun x -> (add_pred x id); aux x) succ;
  in aux node

(** The CFG is implemented as an Hashtable <id, (exp, predecessors, successors)> *)
module CFG = struct
  (* Auxiliary module for providing Hashtbl of a pretty printing function *)
  module Hashtbl = struct
    include Hashtbl

    let pp pp_key pp_value ppf values =
      Hashtbl.iter (fun key data ->
        Format.fprintf ppf "@[<1>%a: %a@]@." pp_key key pp_value data) values

  end

  type 'a item = {
      idx   : int;
      exp   : 'a;
      pred  : int list;
      succ  : int list;
    } [@@deriving show]

  (* The CFG is implemented as an Hashtable <id, (exp, predecessors, successors)> *)
  type 'a t = {
    cfg: (int, 'a item) Hashtbl.t;
    root_id: int
  }
  [@@deriving show]

  let make_item idx exp pred succ =
    {idx = idx; exp = exp; pred = pred; succ = succ}

  (** starting from a node (the root), builds a CFG *)
  let make (initial_node : 'a Node.t) : 'a t =
    let h = Hashtbl.create (Node.length initial_node) in

    let rec aux (h : (int, 'a item) Hashtbl.t) (node : 'a Node.t) : unit =
      match node.succ with
      | [] -> (* the node has not successors, insert it *)
         Hashtbl.add h (node.id) (make_item (node.id) (node.exp) (node.pred) [])

      | x::xs ->  (* insert node' successors into the HT *)
         let id_succ = List.map (fun (x : 'a Node.t) -> x.id) (x.succ) in
         Hashtbl.add h (x.id) (make_item (x.id) (x.exp) (x.pred) id_succ);
         List.iter (aux h) (x.succ);
         List.iter (aux h) xs;

         (* insert node itself *)
         let id_node_succ = List.map (fun (x : 'a Node.t) -> x.id) (node.succ) in
         Hashtbl.add h (node.id) (make_item (node.id) (node.exp) (node.pred) id_node_succ)
    in
    compute_pred initial_node;
    aux h initial_node;
    {cfg=h; root_id=initial_node.id}

  let get (cfg : 'a t) (id : int) = match cfg with
    | {cfg=ht; _} -> Hashtbl.find ht id

  let succ_of (cfg : 'a t) (id : int) = (get cfg id).succ

  let pred_of (cfg : 'a t) (id : int) = (get cfg id).pred

  let get_data (cfg : 'a t) (id : int) = (get cfg id).exp

  let set_data (cfg : 'a t) (id : int) (expr: 'a) =
    let cfg =
      match cfg with
      | {cfg= ht; root_id} -> {cfg= Hashtbl.copy ht; root_id}
    in
    let item = get cfg id in
    let _ =
      match cfg with
      | {cfg=ht; _} -> Hashtbl.replace ht id (make_item item.idx expr item.pred item.succ)
    in
    cfg

  let root (cfg : 'a t) =
    match cfg with
    | {root_id; _} -> get cfg root_id

  let idx (_ : 'a t) (item: 'a item) =
    item.idx

  (* Auxiliary module used into the fold function *)
  module IntSet = Set.Make(struct
                      type t = int
                      let compare = compare
                    end)

  let fold (cfg : 'a t) (fn: 'a t -> 'a item -> 'b -> 'b) (acc: 'b) =
    (* performs a depth first visit of the graph *)
    let visited = IntSet.empty in
    let to_visit = [ idx cfg (root cfg) ] in
    let rec visit to_visit visited acc =
      match to_visit with
      | [] ->
         acc
      | hd::tl ->
         match IntSet.find_opt hd visited with
         | Some(_) ->
            visit tl visited acc
         | None ->
            let visited = IntSet.add hd visited in
            let successors = succ_of cfg hd in
            let acc = fn cfg (get cfg hd) acc in
            visit (successors @ tl) visited acc
    in
    visit to_visit visited acc
end
