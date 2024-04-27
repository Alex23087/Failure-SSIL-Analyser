{
    open Parser
    open Printf

    (* Auxiliary definitions *)
    exception Lexing_error of Location.lexeme_pos * string

    let create_hashtable size init =
        let tbl = Hashtbl.create size in
        List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
    tbl

    let keyword_table =
      let mapping = [
        ("skip", SKIP);
        ("alloc", ALLOC);
        ("free", FREE);
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

| "Int(" int as i ")"                   { printf "int: %s\n" i; Parser.INTEGER (int_of_string i) }
| "True"                                { Parser.TRUE }
| "False"                               { Parser.FALSE }

| id as i                               {
                                          (* look up identifier to see if it's a keyword *)
                                          try
                                            let keyword_token = Hashtbl.find keyword_table i in
                                            printf "keyword: %s\n" i;
                                            (* printf "position: %s\n" (Location.show_lexeme_pos (Location.to_lexeme_position lexbuf)); *)
                                            keyword_token
                                          with Not_found ->
                                            printf "id: %s\n" i;
                                            Parser.ID i
                                        }

| '+'                                   { Parser.PLUS }
| '-'                                   { Parser.MINUS }
| '*'                                   { Parser.STAR }
| '/'                                   { Parser.DIV }
| '%'                                   { Parser.MOD }

| '='                                   { Parser.EQ }
| "=="                                  { Parser.EQEQ }
| "!="                                  { Parser.NOTEQ }
| '<'                                   { Parser.LESS }
| "<="                                  { Parser.LESSEQ }
| '>'                                   { Parser.GREATER }
| ">="                                  { Parser.GREATEREQ }
| "&&"                                  { Parser.AND }
| "||"                                  { Parser.OR }
| '!'                                   { Parser.NOT }

| '?'                                   { Parser.QUESTION }

| '('                                   { Parser.LPAREN }
| ')'                                   { Parser.RPAREN }
| '{'                                   { Parser.LBRACE }
| '}'                                   { Parser.RBRACE }
| '['                                   { Parser.LBRACK }
| ']'                                   { Parser.RBRACK }
| ';'                                   { Parser.SEMICOLON }

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