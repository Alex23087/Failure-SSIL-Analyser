open NormalForm
open DataStructures.Analysis
open DataStructures.Analysis.State
open Utils

let get_postcondition = Commands.get_postcondition
let update_postcondition = Commands.update_postcondition

let starting_states (cfg: Cfg.t) =
  let block_start_postconditions (block: Cfg.block) =
    let fold_fun (start_indices, command_index) statement =
      match get_postcondition statement with
      | Some(_) -> command_index :: start_indices, command_index + 1
      | None -> start_indices, command_index + 1
    in
    let start_indices =
      match block.precondition with
      | Some(_) -> [ 0 ]
      | None -> []
    in
    let start_indices, _ = List.fold_left fold_fun (start_indices, 1) block.statements in
    start_indices
  in
  let rec starting_states (cfg: Cfg.t) (item: Cfg.item) =
    let idx = Cfg.idx cfg item in
    let block = Cfg.get_exp cfg idx in
    let map_fun x = {cfg; last_block = idx; last_statement = x} in
    let start_postconditions = block_start_postconditions block in
    List.map map_fun start_postconditions
  in
  Cfg.fold cfg (fun cfg x acc -> (starting_states cfg x) @ acc) []

let visit_limit (block: Cfg.block) =
  block.visit_count < 10

let annotation_conversion (annotation: Commands.annotation) =
  raise (Failure "not implemented")

let block_analysis_step (block: Cfg.block) (last_statement: int) : Cfg.block =
  let statement = unwrap_option (List.nth_opt block.statements last_statement) "unexpected" in
  let postcondition = unwrap_option (get_postcondition statement) "unexpected" in
  let precondition = Some(Atomic.weakest_precondition statement postcondition annotation_conversion) in
  Cfg.update_formula_at block (last_statement - 1) precondition

let analysis_step (state: analysis_state) : analysis_state list =
  let block_to_starting_state (cfg: Cfg.t) (idx: int) (block: Cfg.block) =
    let cfg = Cfg.set_exp cfg idx block in
    {
      cfg = cfg;
      last_block = idx;
      last_statement = List.length block.statements
    }
  in

  let cfg = state.cfg in
  let current_block = Cfg.get_exp cfg state.last_block in
  if state.last_statement = 0 then
    let map_fun idx =
      let block = Cfg.get_exp cfg idx in
      if visit_limit block then
        let block = Cfg.update_formula_at_last block current_block.precondition in
        Some(block_to_starting_state cfg idx block)
      else
        None
    in
    List.filter_map map_fun (Cfg.pred_of cfg state.last_block)
  else
    let block = block_analysis_step current_block state.last_statement in
    [ block_to_starting_state cfg state.last_block block ]