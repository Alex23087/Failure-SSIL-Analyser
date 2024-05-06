(** parsing utilities *)
  exception Syntax_error of Location.lexeme_pos * string

  let parse _lexer _lexbuf =
    try Parser.program _lexer _lexbuf with
    | Lexer.Lexing_error _ as sle ->
        raise sle
    | Syntax_error _ as spe ->
        raise spe
    | Parser.Error ->
        raise (Syntax_error (Location.to_lexeme_position _lexbuf, "Syntax error"))