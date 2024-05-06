let lex =
  let state = ref 1 in
  fun lexbuf -> let (next_state, next_buf) = if !state = 1
      then Lexer.next_token lexbuf
      else Lexer.consume_formula lexbuf in
      state := next_state;
      next_buf