let unwrap_option opt message =
  match opt with
  | Some(x) -> x
  | None -> raise (Failure message)