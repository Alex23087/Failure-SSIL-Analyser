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
        ("skip", Commands.SKIP);
        ("alloc", Commands.ALLOC);
        ("free", Commands.FREE);
      ]
    in create_hashtable (List.length mapping) mapping

}

(* Scanner specification *)

let digit = ['0'-'9']
let hex_digit = ['0'-'9' 'A'-'F']
let one_to_nine = ['1'-'9']

let base10int = ('-'? one_to_nine digit*) | '0'
let base16int = "0x" hex_digit hex_digit

let int = base10int | base16int

let whitespace = [' ''\t']+
let newline = '\r' | '\n' | "\r\n"


let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']*

rule next_token = parse
| whitespace                            { next_token lexbuf }
| newline                               { Lexing.new_line lexbuf; next_token lexbuf }

| "Int(" int as i ")"                   { Commands.INT (int_of_string i) }
| "True"                                { Commands.TRUE }
| "False"                               { Commands.FALSE }

| id as i                               {
                                          (* look up identifier to see if it's a keyword *)
                                          try
                                            let keyword_token = Hashtbl.find keyword_table i in keyword_token
                                          with Not_found -> Commands.IDENTIFIER i
                                        }

| '+'                                   { Commands.PLUS }
| '-'                                   { Commands.MINUS }
| '*'                                   { Commands.STAR }
| '/'                                   { Commands.DIV }
| '%'                                   { Commands.MOD }

| '='                                   { Commands.EQ }
| "=="                                  { Commands.EQEQ }
| "!="                                  { Commands.NEQ }
| '<'                                   { Commands.LT }
| "<="                                  { Commands.LE }
| '>'                                   { Commands.GT }
| ">="                                  { Commands.GE }
| "&&"                                  { Commands.AND }
| "||"                                  { Commands.OR }
| '!'                                   { Commands.NOT }

| '?'                                   { Commands.QUESTION }

| '('                                   { Commands.LPAREN }
| ')'                                   { Commands.RPAREN }
| '['                                   { Commands.LBRACKET }
| ']'                                   { Commands.RBRACKET }
| ';'                                   { Commands.SEMICOLON }

| "//"                                  { consume_single_line_comment lexbuf }
| "/*"                                  { consume_multi_line_comment lexbuf }
| "<<"                                  { consume_formula lexbuf }
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
  | "Int(" int as i ")"                   { Commands.Integer (int_of_string i) }
  | ">>"
    {
      next_token lexbuf
    }
  | '+'                                   { Commands.Plus }
  | '-'                                   { Commands.Minus }
  | '*'                                   { Commands.Times }
  | '/'                                   { Commands.Div }
  | '%'                                   { Commands.Mod }
  | "true"                                { Commands.True }
  | "false"                               { Commands.False }
  | "exists"                              { Commands.Exists }
  | "^"                                   { Commands.Star }
  | "->"                                  { Commands.Arrow }
  | "-/>"                                 { Commands.Void }
  | "emp"                                 { Commands.Emp }
  | '<'                                   { Commands.LTf }
  | "<="                                  { Commands.LEf }
  | '>'                                   { Commands.GTf }
  | ">="                                  { Commands.GEf }
  | "=="                                  { Commands.EQf }
  | "!="                                  { Commands.NEf }
  | "&&"                                  { Commands.And }
  | "||"                                  { Commands.Or }
  | id as i                               { Commands.Identifier i }
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