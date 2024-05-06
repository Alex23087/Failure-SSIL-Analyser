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

  | int as i                              { (1, Parser.INT (int_of_string i)) }
  | "True"                                { (1, Parser.TRUE) }
  | "False"                               { (1, Parser.FALSE) }

  | id as i                               {
                                            (* look up identifier to see if it's a keyword *)
                                            try
                                              let keyword_token = Hashtbl.find keyword_table i in (1, keyword_token)
                                            with Not_found -> (1, Parser.IDENTIFIER i)
                                          }
  | "<<"                                  { consume_formula lexbuf }
  | '+'                                   { (1, Parser.PLUS) }
  | '-'                                   { (1, Parser.MINUS) }
  | '*'                                   { (1, Parser.STAR) }
  | '/'                                   { (1, Parser.DIV) }
  | '%'                                   { (1, Parser.MOD) }

  | '='                                   { (1, Parser.EQ) }
  | "=="                                  { (1, Parser.EQEQ) }
  | "!="                                  { (1, Parser.NEQ) }
  | '<'                                   { (1, Parser.LT) }
  | "<="                                  { (1, Parser.LE) }
  | '>'                                   { (1, Parser.GT) }
  | ">="                                  { (1, Parser.GE) }
  | "&&"                                  { (1, Parser.AND) }
  | "||"                                  { (1, Parser.OR) }
  | '!'                                   { (1, Parser.NOT) }

  | '?'                                   { (1, Parser.QUESTION) }

  | '('                                   { (1, Parser.LPAREN) }
  | ')'                                   { (1, Parser.RPAREN) }
  | '['                                   { (1, Parser.LBRACKET) }
  | ']'                                   { (1, Parser.RBRACKET) }
  | ';'                                   { (1, Parser.SEMICOLON) }

  | "//"                                  { consume_single_line_comment lexbuf }
  | "/*"                                  { consume_multi_line_comment lexbuf }
  | eof                                   { (1, EOF) }
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
  | eof { (1, EOF) }
  | _
    {
      consume_single_line_comment lexbuf
    }
and consume_formula = parse
  | whitespace                            { consume_formula lexbuf }
  | int as i                              { (0, Parser.Integer (int_of_string i)) }
  | ">>"                                  { next_token lexbuf }
  | '+'                                   { (0, Parser.Plus) }
  | '-'                                   { (0, Parser.Minus) }
  | '*'                                   { (0, Parser.Times) }
  | '/'                                   { (0, Parser.Div) }
  | '%'                                   { (0, Parser.Mod) }
  | "true"                                { (0, Parser.True) }
  | "false"                               { (0, Parser.False) }
  | "exists"                              { (0, Parser.Exists) }
  | "^"                                   { (0, Parser.Star) }
  | "->"                                  { (0, Parser.Arrow) }
  | "-/>"                                 { (0, Parser.Void) }
  | "emp"                                 { (0, Parser.Emp) }
  | '<'                                   { (0, Parser.LTf) }
  | "<="                                  { (0, Parser.LEf) }
  | '>'                                   { (0, Parser.GTf) }
  | ">="                                  { (0, Parser.GEf) }
  | "=="                                  { (0, Parser.EQf) }
  | "!="                                  { (0, Parser.NEf) }
  | "&&"                                  { (0, Parser.And) }
  | "||"                                  { (0, Parser.Or) }
  | id as i                               { (0, Parser.Identifier i) }
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