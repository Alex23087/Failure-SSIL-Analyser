open ControlFlowGraph

(** Data structure which represent a given state of the analysis *)
type analysis_state = {
  cfg: Cfg.t;
  last_block: int;
  last_statement: int;
}

let get_last_block_precondition (state: analysis_state) =
  let last_block = Cfg.get_data state.cfg state.last_block in
  last_block.precondition