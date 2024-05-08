{
    open Printf

    (* Auxiliary definitions *)
    exception Lexing_error of Location.lexeme_pos * string

    let create_hashtable size init =
        let tbl = Hashtbl.create size in
        List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
    tbl

    let keyword_table =
      let mapping = [
        ("skip", Parser.SKIP);
        ("alloc", Parser.ALLOC);
        ("free", Parser.FREE);
      ]
    in create_hashtable (List.length mapping) mapping

    (* current active rule: 1 is next_token, 0 is consume_formula *)
    let state = ref 1
}

(* Scanner specification *)

let digit_base10 = ['0'-'9']
let one_to_nine = ['1'-'9']
let one_to_f = ['1'-'9' 'A'-'F']
let alpha = ['a'-'z' 'A'-'Z']

let num_base10 = (one_to_nine digit_base10*) | '0'

let int = num_base10

let whitespace = [' ''\t']+
let newline = '\r' | '\n' | "\r\n"

let id = ('_' | alpha)('_' | alpha | digit_base10)*

rule next_token = parse
  | whitespace                            { next_token lexbuf }
  | newline                               { Lexing.new_line lexbuf; next_token lexbuf }

  | int as i                              { Parser.INT (int_of_string i) }
  | "True"                                { Parser.True }
  | "False"                               { Parser.False }

  | id as i                               {
                                            (* look up identifier to see if it's a keyword *)
                                            try
                                              let keyword_token = Hashtbl.find keyword_table i in keyword_token
                                            with Not_found -> Parser.IDENTIFIER i
                                          }
  | "<<"                                  { state := 0; Parser.LShift }
  | '+'                                   { Parser.Plus }
  | '-'                                   { Parser.Minus }
  | '*'                                   { Parser.Times }
  | '/'                                   { Parser.Div }
  | '%'                                   { Parser.Mod }

  | '='                                   { Parser.Equal }
  | "=="                                  { Parser.EQEQ }
  | "!="                                  { Parser.NotEqual }
  | '<'                                   { Parser.LessThan }
  | "<="                                  { Parser.LessOrEqual }
  | '>'                                   { Parser.GreaterThan }
  | ">="                                  { Parser.GreaterOrEqual }
  | "&&"                                  { Parser.And }
  | "||"                                  { Parser.Or }
  | '!'                                   { Parser.Not }

  | '?'                                   { Parser.QUESTION }

  | '('                                   { Parser.LParen }
  | ')'                                   { Parser.RParen }
  | '['                                   { Parser.LBRACKET }
  | ']'                                   { Parser.RBRACKET }
  | ';'                                   { Parser.Semicolon }

  | "//"                                  { consume_single_line_comment lexbuf }
  | "/*"                                  { consume_multi_line_comment lexbuf }
  | eof                                   { EOF }
  | _ as c
    {
      let err_msg = sprintf "Unrecognized character: %c --- " c in
      let pos = Location.to_lexeme_position lexbuf in
      raise (Lexing_error (pos, err_msg))
    }
and consume_multi_line_comment = parse
  | "*/"
    {
      next_token lexbuf
    }
  | newline
    {
      Lexing.new_line lexbuf;
      consume_multi_line_comment lexbuf
    }
  | eof
    {
      let err_msg = "Unterminated multiline comment" in
      let pos = Location.to_lexeme_position lexbuf in
      raise (Lexing_error(pos, err_msg))
    }
  | _
    {
      consume_multi_line_comment lexbuf
    }
and consume_single_line_comment = parse
  | newline
    {
      Lexing.new_line lexbuf;
      next_token lexbuf
    }
  | eof { EOF }
  | _
    {
      consume_single_line_comment lexbuf
    }
and consume_formula = parse
  | whitespace                            { consume_formula lexbuf }
  | int as i                              { Parser.Integer (int_of_string i) }
  | ">>"                                  { state := 1; Parser.RShift }
  | '+'                                   { Parser.Plus }
  | '-'                                   { Parser.Minus }
  | '*'                                   { Parser.Times }
  | '/'                                   { Parser.Div }
  | '%'                                   { Parser.Mod }
  | "true"                                { Parser.True }
  | "false"                               { Parser.False }
  | "exists"                              { Parser.Exists }
  | "->"                                  { Parser.Arrow }
  | "-/>"                                 { Parser.Void }
  | "emp"                                 { Parser.Emp }
  | '<'                                   { Parser.LessThan }
  | "<="                                  { Parser.LessOrEqual }
  | '>'                                   { Parser.GreaterThan }
  | ">="                                  { Parser.GreaterOrEqual }
  | "=="                                  { Parser.Equal }
  | "!="                                  { Parser.NotEqual }
  | "&&"                                  { Parser.And }
  | "||"                                  { Parser.Or }
  | "("                                   { Parser.LParen }
  | ")"                                   { Parser.RParen }
  | id as i                               { Parser.Identifier i }
  | newline
    {
      Lexing.new_line lexbuf;
      consume_formula lexbuf
    }
  | eof
    {
      let err_msg = "Unterminated formula" in
      let pos = Location.to_lexeme_position lexbuf in
      raise (Lexing_error(pos, err_msg))
    }
  | _
    {
      consume_formula lexbuf
    }

{

  (* it is the lexer used by the parser *)
  let lex = fun lexbuf ->
    if !state = 1
      then next_token lexbuf
      else consume_formula lexbuf
}