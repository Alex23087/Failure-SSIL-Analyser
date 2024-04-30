open Prelude.Ast

let from_menhir_pos (position) =
    let line = position.Lexing.pos_lnum in
    let column = position.Lexing.pos_cnum in
    make_position line column
