(** parsing utilities *)
  exception Syntax_error of Location.lexeme_pos * string

(** tries to parse what's inside lexbuf using lexer as a scanner; *)
let parse lexer lexbuf =
  try Parser.program lexer lexbuf with
  | Lexer.Lexing_error _ as sle ->
      raise sle
  | Syntax_error _ as spe ->
      raise spe
    (* unfortunately, a parse error has no info with it *)
  | Parser.Error ->
      raise (Syntax_error (Location.to_lexeme_position lexbuf, "Syntax error"))