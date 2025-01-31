{

  open Better

  module Newick:
    sig
      type t
      val create: ?rich_format:bool -> unit -> t
      val is_rich_format: t -> bool
      val incr_line: t -> unit
      exception ParseError of string
      val parse_error: t -> string -> 'a
    end
  = struct
      type t = {
        rich_format: bool;
        mutable line: int
      }
      let create ?(rich_format = true) () = {
        rich_format;
        line = 1
      }
      let is_rich_format state = state.rich_format
      let incr_line state =
        state.line <- state.line + 1
      exception ParseError of string
      let parse_error state s =
        ParseError (Printf.sprintf "On line %d: %s" state.line s) |> raise
    end

  module Splits:
    sig
      type t
      val create: unit -> t
      val incr_line: t -> unit
      exception ParseError of string
      val parse_error: t -> string -> 'a
    end
  = struct
      type t = {
        mutable line: int
      }
      let create () = {
        line = 1
      }
      let incr_line state =
        state.line <- state.line + 1
      exception ParseError of string
      let parse_error state s =
        ParseError (Printf.sprintf "On line %d: %s" state.line s) |> raise
    end

}

(* Tokenisers for Newick format *)

rule newick state = parse
| '\n'
  { Newick.incr_line state;
    newick state lexbuf }
| '\''
  { (* Beginning of string *)
    Trees_Parse.Newick_NAME (newick_end_of_string state lexbuf) }
| ['\r' '\t' ' ']+
  { newick state lexbuf }
| '('
  { Trees_Parse.Newick_LBRACK }
| ')'
  { Trees_Parse.Newick_RBRACK }
| ','
  { Trees_Parse.Newick_COMMA }
| ':' (( '.'['0'-'9']+ | ['0'-'9']+('.'(['0'-'9']*))?) (['e''E']['+''-']?['0'-'9']*)? as n)
  { try
      Trees_Parse.Newick_LENGTH (float_of_string n)
    with _ ->
      Newick.parse_error state ("Invalid number '" ^ n ^ "'") }
| ':'
  { Trees_Parse.Newick_COLON }
| ';'
  { Trees_Parse.Newick_SEMI }
| "[&R]" | "[&r]"
  { (* In regular Newick format, by default trees are rooted *)
    Trees_Parse.Newick_ROOTED true }
| "[&U]" | "[&u]"
  { (* Note that this directive must be parsed differently depending on the format accepted *)
    Trees_Parse.Newick_ROOTED (Newick.is_rich_format state |> not) }
| "[&&NHX"
  { (* Beginning of a New Hampshire Extended dictionary.
       Note that this directive must be parsed differently depending on the format accepted *)
    if Newick.is_rich_format state then
      Trees_Parse.Newick_DICT (newick_end_of_nhx state lexbuf)
    else
      (* We treat the rest as a regular comment *)
      newick_end_of_comment state lexbuf }
| "[&"
  { (* Beginning of a BEAST-like dictionary.
       Note that this directive must be parsed differently depending on the format accepted *)
    if Newick.is_rich_format state then
      Trees_Parse.Newick_DICT (newick_end_of_dict true state lexbuf)
    else
      (* We treat the rest as a regular comment *)
      newick_end_of_comment state lexbuf }
| '['
  { (* Beginning of an actual comment *)
    newick_end_of_comment state lexbuf }
| '#' (("H" | "h" | "LGT" | "lgt" | "R" | "r")? as s) (['0'-'9']+ as n)
  { try
      let n = int_of_string n in
      Trees_Parse.Newick_HYBRID begin
        match s with
        | "H" | "h" -> Trees_Base.Newick.Hybridization n
        | "LGT" | "lgt" -> Trees_Base.Newick.GeneTransfer n
        | "R" | "r" -> Trees_Base.Newick.Recombination n
        | _ -> assert false
      end
    with _ ->
      Newick.parse_error state ("Invalid number '" ^ n ^ "'") }
| [ ^ '\n' '\r' '\t' ' ' '(' ')' ':' ';' ',' '\'' '[' ']' '#' ]+ as s
  { Trees_Parse.Newick_NAME s }
| eof
  { Trees_Parse.Newick_EOF }
| _ as c
  { Newick.parse_error state ("Invalid character '" ^ String.make 1 c ^ "'") }
and newick_end_of_string state = parse
| "\'\'"
  { (* Escaped quote *)
    "\'" ^ newick_end_of_string state lexbuf }
| '\''
  { (* End of string *)
    "" }
| '\r' | '\n' | eof
  { Newick.parse_error state "Unterminated string" }
| [ ^ '\'' '\r' '\n' ]+ as s
  { s ^ newick_end_of_string state lexbuf }
and newick_end_of_nhx state = parse
| ':' ([ ^ ':' '=' ']' ]+ as k) '=' ([ ^ ':' '=' ']' ]+ as v)
  { StringMap.add k v (newick_end_of_nhx state lexbuf) }
| ']'
  { (* End of NHX dictionary *)
    StringMap.empty }
| _ as c
  { Newick.parse_error state ("Invalid character '" ^ String.make 1 c ^ "' in NHX dictionary") }
and newick_end_of_dict is_first state = parse
| ','
  { if is_first then
      Newick.parse_error state "Invalid character ',' in dictionary"
    else
      newick_end_of_dict false state lexbuf }
| ([ ^ ',' '=' ']' ]+ as k) '=' ([ ^ ',' '=' ']' ]+ as v)
  { StringMap.add k v (newick_end_of_dict false state lexbuf) }
| ']'
  { (* End of dictionary *)
    StringMap.empty }
| _ as c
  { Newick.parse_error state ("Invalid character '" ^ String.make 1 c ^ "' in dictionary") }
and newick_end_of_comment state = parse
| '\n'
  { Newick.incr_line state;
    newick_end_of_comment state lexbuf }
| ']'
  { (* End of comment *)
    newick state lexbuf }
| eof
  { Newick.parse_error state "Unterminated comment" }
| _
  { newick_end_of_comment state lexbuf }

(* Tokeniser for split sets *)

and splits state = parse
| '\n'
  { Splits.incr_line state;
    splits state lexbuf }
| '\''
  { (* Beginning of string *)
    Trees_Parse.Splits_NAME (splits_end_of_string state lexbuf) }
| ['\r' '\t' ' ']+
  { splits state lexbuf }
| ':'
  { Trees_Parse.Splits_COLON }
| ';'
  { Trees_Parse.Splits_SEMICOLON }
| '('
  { Trees_Parse.Splits_LBRACK }
| ')'
  { Trees_Parse.Splits_RBRACK }
| ','
  { Trees_Parse.Splits_COMMA }
| (( '.'['0'-'9']+ | ['0'-'9']+'.'['0'-'9']*) (['e''E']['+''-']?['0'-'9']*)? as n)
  { try
      Trees_Parse.Splits_FLOAT (float_of_string n)
    with _ ->
      Splits.parse_error state ("Invalid number '" ^ n ^ "'") }
| ('0'(['B' 'O' 'X' 'b' 'o' 'x'] as what)['0'-'9']+ as n)
  { try
      begin match what with
      | 'B' | 'b' ->
        Trees_Parse.Splits_BINARY (Trees_Base.Splits.Split.of_string n)
      | 'O' | 'o' ->
        Trees_Parse.Splits_OCTAL (Trees_Base.Splits.Split.of_string n)
      | 'X' | 'x' ->
        Trees_Parse.Splits_HEXADECIMAL (Trees_Base.Splits.Split.of_string n)
      | _ ->
        assert false
      end
    with _ ->
      Splits.parse_error state ("Invalid number '" ^ n ^ "'") }
| (['0'-'9']+ as n)
  { try
      Trees_Parse.Splits_DECIMAL (int_of_string n)
    with _ ->
      Splits.parse_error state ("Invalid number '" ^ n ^ "'") }
| [ ^ '\n' '\r' '\t' ' ' '(' ')' ':' ';' ',' '\'' '[' ']' '#' ]+ as s
  (* We intentionally define unquoted names as in Newick *)
  { Trees_Parse.Splits_NAME s }
| eof
  { Trees_Parse.Splits_EOF }
| _ as c
  { Splits.parse_error state ("Invalid character '" ^ String.make 1 c ^ "'") }
and splits_end_of_string state = parse
| "\'\'"
  { (* Escaped quote *)
    "\'" ^ splits_end_of_string state lexbuf }
| '\''
  { (* End of string *)
    "" }
| '\r' | '\n' | eof
  { Splits.parse_error state "Unterminated string" }
| [ ^ '\'' '\r' '\n' ]+ as s
  { s ^ splits_end_of_string state lexbuf }

