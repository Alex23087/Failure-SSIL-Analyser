type lexeme_pos = { line : int; start_column : int; end_column : int }
[@@deriving show, ord, eq]

type code_pos = {
  start_line : int;
  start_column : int;
  end_line : int;
  end_column : int;
}
[@@deriving show, ord, eq]

let to_lexeme_position lexbuf =
  let startp = Lexing.lexeme_start_p lexbuf in
  let endp = Lexing.lexeme_end_p lexbuf in
  let line = startp.Lexing.pos_lnum in
  let start_column = startp.Lexing.pos_cnum - startp.Lexing.pos_bol + 1 in
  let end_column = endp.Lexing.pos_cnum - endp.Lexing.pos_bol in
  { line; start_column; end_column }