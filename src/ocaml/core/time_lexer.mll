{
  open Time_parser
  open Lexing
  exception Error of string
}

let digit = ['0'-'9']

                      rule token = parse
            | '.'           { DOT                                  }
            | 'Z'           { OFFSET_ZERO                          }
            | 'T'           { DATE_TIME_SEP                        }
            | '+'           { PLUS                                }
            | '-'           { MINUS                                }
            | ':'           { COLON                               }
            | digit { DIGIT (lexeme lexbuf)}
            | eof           { EOF                                  }
            | _             { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf)))}
