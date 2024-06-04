open ControlFlowGraph

type analysis_state = {
  cfg: Cfg.t;
  last_block: int;
  last_statement: int;
}
