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