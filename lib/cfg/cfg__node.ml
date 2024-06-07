(* Internal static counter for generating incremental IDs *)
open Sexplib.Std
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
      mutable exp   : 'a;
      mutable succ  : 'a t list;
      mutable pred  : int list;
    } [@@deriving show, sexp]

  let compare (node1: 'a t) (node2: 'a t) : bool =
    (* severely limited, only checks isomorphism for two children for each node,
       also doesn't return true for every isomophic graphs that have two
       children for each node, only for some *)
    (* TODO: make it better *)
    let associations = ref (Hashtbl.create 100) in (* lets hope that 100 is big enough *)
    Hashtbl.add !associations node1.id node2.id;

    let alreadyvisited = ref [] in

    let rec helper (node1: 'a t) (node2: 'a t) : bool =
      if List.mem node1.id !alreadyvisited then
        true
      else (
        alreadyvisited := node1.id :: !alreadyvisited;
        match (Hashtbl.find_opt !associations node1.id, node2.id) with
        | (Some a, b) -> (
          match (a == b, node1.exp = node2.exp, node1.succ, node2.succ) with
          | (true, true, [], []) -> true (* end of the graph *)
          | (true, true, [node1succ], [node2succ]) -> (* easy case, only 1 child *)
           Hashtbl.add !associations node1succ.id node2succ.id;
           helper node1succ node2succ
          | (true, true, a, b) when (List.length a) != (List.length b) -> (* not the right ammount of children *)
             false
          | (true, true, [n11; n12], [n21; n22]) -> (
            match (n11.exp = n21.exp, n11.exp = n22.exp) with
            | (true, _) -> (
              Hashtbl.add !associations n11.id n21.id;
              Hashtbl.add !associations n12.id n22.id;
              (helper n11 n21) && (helper n12 n22)
            )
            | (_, true) -> (
              Hashtbl.add !associations n11.id n22.id;
              Hashtbl.add !associations n12.id n21.id;
              (helper n11 n22) && (helper n12 n21)
            )
            | (false, false) -> false
          )
          | (true, true, _, _) -> (* Sir, This Is A Wendy's *)
             false
          | _ ->
             false
        )
        | (None, _) -> (* first time we see node1 (happens only at the beginning) *)
           false
      )
    in
    helper node1 node2


  let make (exp: 'a) (succ: 'a t list) (pred: int list) : 'a t =
    {id = next_id(); exp = exp; succ = succ; pred = pred}

  let makeWithId (id: int) (exp: 'a) (succ: 'a t list) (pred: int list) : 'a t =
    {id = id; exp = exp; succ = succ; pred = pred}

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

  let structure_without_loops_destructive (node : 'a t) : unit =
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

  (** given a node and an id, adds the latter to the predecessor list of the
      former*)
  let add_pred (node : 'a t) (pred : int) : unit =
  node.pred <- pred::(node.pred)

  let compute_pred (node : 'a t) : unit =
    let rec aux = function
      | { id; exp=_; succ; _ } ->
         List.iter (fun x -> (add_pred x id); aux x) succ;
    in aux node
end
