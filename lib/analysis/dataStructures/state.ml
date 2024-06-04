open ControlFlowGraph

(** Data structure which represent a given state of the analysis *)
type analysis_state = {
  cfg: Cfg.t;
  last_block: int;
  last_statement: int;
}
