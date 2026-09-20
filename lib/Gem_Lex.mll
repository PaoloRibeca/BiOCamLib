{
  (*
      Gem_Lex.mll -- (c) 2026 Paolo Ribeca, <paolo.ribeca@gmail.com>

      This file is part of BiOCamLib, the OCaml foundations upon which
      a number of the bioinformatics tools I developed are built.

      Gem_Lex.mll tokenises the GEM mapper's own MAP format one field, and one
      placement, at a time, so that Files.Gem can stream a record without ever
      holding a whole line: a read landing on a repeat lists every copy on its
      line, and that line need not fit in memory.

      This program was designed and developed by the author(s),
      with the assistance of the following AI tool(s):
        2026 Claude (Anthropic).
      The final logic and implementation were reviewed and verified in
      their entirety by the author(s).

      This program is free software: you can redistribute it and/or modify
      it under the terms of the GNU General Public License as published by
      the Free Software Foundation, either version 3 of the License, or
      (at your option) any later version.

      This program is distributed in the hope that it will be useful,
      but WITHOUT ANY WARRANTY; without even the implied warranty of
      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
      GNU General Public License for more details.

      You should have received a copy of the GNU General Public License
      along with this program.  If not, see <https://www.gnu.org/licenses/>.
  *)

  (* The atoms of a GIGAR, the mapper's own alignment string. The sign of an indel is GEM's, and
      was checked against what gem3-mapper writes: '>n+' is a deletion, n reference bases the read
      lacks, and '>n-' an insertion, n read bases the reference lacks *)
  type atom_t =
    | Match of int
    | Mismatch of char (* The reference base *)
    | Trim of int
    | Deletion of int
    | Insertion of int
    | Splice of int
  (* What ends an alignment string: a run of colons (one or three introduce a score, two the read's
      other mate), a comma before the next placement, or the end of the record or of the file *)
  type sep_t = Colons of int | Comma | Eol | Eof
  type gigar_t = Atom of atom_t | Sep of sep_t
  type maps_t = Unmapped | Mapped
  exception Error of string
  let error what lexbuf =
    raise (Error (Printf.sprintf "%s, found '%s'" what (Lexing.lexeme lexbuf)))
}

let digit = ['0'-'9']
let base = ['A' 'C' 'G' 'T' 'N' 'a' 'c' 'g' 't' 'n']
let in_field = [^ '\t' '\n']

(* The read tag and its tab, or None at the end of the file. An empty line is skipped *)
rule tag = parse
  | eof { None }
  | '\n' { Lexing.new_line lexbuf; tag lexbuf }
  | (in_field+ as s) '\t' { Some s }
  | _ { error "expected a read tag followed by a tab" lexbuf }
(* A bounded field and its tab *)
and field = parse
  | (in_field* as s) '\t' { s }
  | _ { error "expected a field followed by a tab" lexbuf }
(* An unmapped read writes a dash for its placements *)
and maps = parse
  | '-' '\n' { Lexing.new_line lexbuf; Unmapped }
  | "" { Mapped }
(* A contig name and its colon *)
and name = parse
  | ([^ ':' ',' '\t' '\n']+ as s) ':' { s }
  | _ { error "expected a contig name followed by a colon" lexbuf }
(* The strand and its colon *)
and strand = parse
  | '+' ':' { true }
  | '-' ':' { false }
  | _ { error "expected a strand, '+' or '-', followed by a colon" lexbuf }
(* The 1-based position on the contig and its colon *)
and position = parse
  | (digit+ as s) ':' { int_of_string s }
  | _ { error "expected a position followed by a colon" lexbuf }
(* One atom of the alignment string, or whatever ends it *)
and gigar = parse
  | (digit+ as s) { Atom (Match (int_of_string s)) }
  | (base as c) { Atom (Mismatch c) }
  | '(' (digit+ as s) ')' { Atom (Trim (int_of_string s)) }
  | '>' (digit+ as s) base* '+' { Atom (Deletion (int_of_string s)) }
  | '>' (digit+ as s) base* '-' { Atom (Insertion (int_of_string s)) }
  | '>' (digit+ as s) base* ['*' '/' '%'] { Atom (Splice (int_of_string s)) }
  | (':'+ as s) { Sep (Colons (String.length s)) }
  | ',' { Sep Comma }
  | '\n' { Lexing.new_line lexbuf; Sep Eol }
  | eof { Sep Eof }
  | _ { error "unexpected character in an alignment string" lexbuf }
(* A score, up to but not including whatever follows it *)
and score = parse
  | [^ ':' ',' '\n']* { () }
