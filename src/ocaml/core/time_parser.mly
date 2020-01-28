%{
open Time_rfc3339_syntax

let string_to_milisec s =
  let milisec_len = 6 in
  let padding = String.make (max 0 (milisec_len - String.length s)) '0' in
  let s' = s ^ padding in
  int_of_string s'
%}

%token DOT OFFSET_ZERO COLON PLUS MINUS DATE_TIME_SEP
%token <string> DIGIT
%token EOF

%start parse
%type <Time_rfc3339_syntax.date_time> parse
%%

%public two_digits:
  | DIGIT DIGIT { $1 ^ $2}
;
%public four_digits:
  | DIGIT DIGIT DIGIT DIGIT { String.concat "" [$1;$2;$3;$4]}
;

parse: | date_time EOF { $1 }
date_time: | full_year DATE_TIME_SEP full_time { {full_year = $1; full_time = $3} }
full_year: | four_digits MINUS two_digits MINUS two_digits { {year = int_of_string $1; month = int_of_string $3; mday = int_of_string $5} }
full_time: | two_digits COLON two_digits COLON two_digits option(secfrac) time_offset {
                     {hour = int_of_string $1;
                      minute = int_of_string $3;
                      second = int_of_string $5;
                      sec_frac = $6;
                      offset = $7;
                     }
                   }
time_offset:
  | OFFSET_ZERO { Z }
  | PLUS two_digits COLON two_digits {Num_offset (Plus, int_of_string $2, int_of_string $4)}
  | MINUS two_digits COLON two_digits {Num_offset (Minus, int_of_string $2, int_of_string $4)}
secfrac: | DOT nonempty_list(DIGIT) {string_to_milisec (String.concat "" $2)}
