open DataStructures.Analysis
open Utils

let get_postcondition = Commands.get_postcondition
let update_postcondition = Commands.update_postcondition

let starting_states (cfg: Cfg.t) =
  let block_start_postconditions (block: CfgBlock.t) =
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
  let starting_states (cfg: Cfg.t) (item: Cfg.item) =
    let idx = Cfg.idx cfg item in
    let block = Cfg.get_data cfg idx in
    let map_fun x = {cfg; last_block = idx; last_statement = x} in
    let start_postconditions = block_start_postconditions block in
    List.map map_fun start_postconditions
  in
  Cfg.fold cfg (fun cfg x acc -> (starting_states cfg x) @ acc) []

let visit_limit (block: CfgBlock.t) =
  block.visit_count >= 10

let block_analysis_step (block: CfgBlock.t) (last_statement: int) : CfgBlock.t =
  let annotation_conversion (annotation: Commands.annotation) : NormalForm.annotation =
    {position= annotation.position}
  in
  let statement = unwrap_option (List.nth_opt block.statements last_statement) "unexpected" in
  let postcondition = unwrap_option (get_postcondition statement) "unexpected" in
  let precondition = Some(Atomic.weakest_precondition statement postcondition annotation_conversion) in
  CfgBlock.update_formula_at block (last_statement - 1) precondition

let analysis_step (state: analysis_state) : analysis_state list * analysis_state list =
  let block_to_starting_state (cfg: Cfg.t) (idx: int) (block: CfgBlock.t) =
    let cfg = Cfg.set_data cfg idx block in
    {
      cfg = cfg;
      last_block = idx;
      last_statement = List.length block.statements
    }
  in

  let cfg = state.cfg in
  let current_block = Cfg.get_data cfg state.last_block in
  if state.last_statement = 0 then
    let map_fun idx =
      let block = Cfg.get_data cfg idx in
      let iteration_limit_reached = visit_limit block in
      let block = CfgBlock.update_formula_at_last block current_block.precondition in
      let state = block_to_starting_state cfg idx block in
      if iteration_limit_reached then
        Either.Right(state)
      else
        Either.Left(state)
    in

    let next_states, end_states = List.partition_map map_fun (Cfg.pred_of cfg state.last_block) in
    match next_states with
    | [] -> [], state :: end_states
    | _ -> next_states, end_states
  else
    let block = block_analysis_step current_block state.last_statement in
    [ block_to_starting_state cfg state.last_block block ], []

let analyze_program (cfg: Cfg.t) =
  let states = starting_states cfg in
  let rec analyze (next_states: analysis_state list) (end_states: analysis_state list) =
    match next_states with
    | [] ->
      end_states
    | hd::tl ->
      let next_states, new_end_states = analysis_step hd in
      analyze (next_states @ tl) (new_end_states @ end_states)
  in
  analyze states []