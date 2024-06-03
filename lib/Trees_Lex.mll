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

}

rule newick state = parse
| '\n' eof
  { Newick.incr_line state;
    newick state lexbuf }
| '\n'
  { Newick.incr_line state;
    (* We add an implicit root token *)
    Trees_Parse.Newick_ROOTED (Newick.is_rich_format state |> not) }
| ['\r' '\t' ' ']+
  { newick state lexbuf }
| '('
  { Trees_Parse.Newick_LBRACK }
| ')'
  { Trees_Parse.Newick_RBRACK }
| ','
  { Trees_Parse.Newick_COMMA }
| ':' (( '.'['0'-'9']+ | ['0'-'9']+('.'(['0'-'9']*))?) as n)
  { try
      Trees_Parse.Newick_LENGTH (float_of_string n)
    with _ ->
      Newick.parse_error state ("Invalid number '" ^ n ^ "'") }
| ':'
  { Trees_Parse.Newick_COLON }
| ';'
  { Trees_Parse.Newick_SEMI }
| '\''
  { (* Beginning of string *)
    Trees_Parse.Newick_NAME (newick_end_of_string state lexbuf) }
| "[&R]" | "[&r]"
  { (* Note that this directive must be parsed differently depending on the format accepted *)
    Trees_Parse.Newick_ROOTED (true && Newick.is_rich_format state) }
| "[&U]" | "[&u]"
  { Trees_Parse.Newick_ROOTED false }
| '['
  { (* Beginning of an actual comment *)
    newick_end_of_comment state lexbuf;
    newick state lexbuf }
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
and newick_end_of_comment state = parse
| '\n'
  { Newick.incr_line state;
    newick_end_of_comment state lexbuf }
| ']'
  { (* End of comment *)
    () }
| eof
  { Newick.parse_error state "Unterminated comment" }
| _
  { newick_end_of_comment state lexbuf }

