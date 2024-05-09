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
        ("skip", Parser.Skip);
        ("alloc", Parser.Alloc);
        ("free", Parser.Free);
        ("true", Parser.True);
        ("false", Parser.False);
        ("exists", Parser.Exists);
        ("nondet", Parser.NonDet);
        ("if", Parser.If);
        ("then", Parser.Then);
        ("else", Parser.Else);
        ("while", Parser.While);
        ("emp", Parser.Emp);
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

  | int as i                              { Parser.Integer (int_of_string i) }
  | "<<"                                  { Parser.LShift }
  | ">>"                                  { Parser.RShift }
  | "->"                                  { Parser.Arrow }
  | "-/>"                                 { Parser.Void }
  | '+'                                   { Parser.Plus }
  | '-'                                   { Parser.Minus }
  | '*'                                   { Parser.Times }
  | '/'                                   { Parser.Div }
  | '%'                                   { Parser.Mod }
  | '.'                                   { Parser.Dot }
  | "=="                                  { Parser.EqualEqual }
  | '='                                   { Parser.Equal }
  | "!="                                  { Parser.NotEqual }
  | '<'                                   { Parser.LessThan }
  | "<="                                  { Parser.LessOrEqual }
  | '>'                                   { Parser.GreaterThan }
  | ">="                                  { Parser.GreaterOrEqual }
  | "&&"                                  { Parser.And }
  | "||"                                  { Parser.Or }
  | '!'                                   { Parser.Not }

  | '?'                                   { Parser.Question }

  | '('                                   { Parser.LParen }
  | ')'                                   { Parser.RParen }
  | '['                                   { Parser.LBracket }
  | ']'                                   { Parser.RBracket }
  | '{'                                   { Parser.LBrace }
  | '}'                                   { Parser.RBrace }
  | ';'                                   { Parser.Semicolon }

  | "//"                                  { consume_single_line_comment lexbuf }
  | "/*"                                  { consume_multi_line_comment lexbuf }
  | id as i                               {
                                            (* look up identifier to see if it's a keyword *)
                                            match Hashtbl.find_opt keyword_table i with
                                              | Some keyword -> keyword
                                              | None -> Parser.Identifier i
                                          }
  | eof                                   { Eof }
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
  | eof    {
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
  | eof { Eof }
  | _
    {
      consume_single_line_comment lexbuf
    }

{

  (* it is the lexer used by the parser *)
  let lex = next_token
}