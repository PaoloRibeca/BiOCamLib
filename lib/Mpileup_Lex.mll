(*
    Mpileup_Lex.mll -- (c) 2026 Paolo Ribeca, <paolo.ribeca@gmail.com>

    This file is part of BiOCamLib, the OCaml foundations upon which
    a number of the bioinformatics tools I developed are built.

    Mpileup_Lex.mll tokenises the read-bases column of a pileup
    line.  It is where the one context-sensitive part of the
    format lives: an indel is written as a sign, a length and then
    that many characters, and no grammar can say "then n more
    characters" -- a parameterised lexer rule can.

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

{
open Better
open Mpileup_Parse

(* The column is read as a whole, so a position within it is a character offset
   rather than a line and a column.  Reporting it is worth the little it costs:
   a pileup is machine-written and an unexpected character in it means either a
   tool this reader has not met or a truncated file, and neither is diagnosable
   from the character alone. *)
let raise_at lexbuf message =
  Exception.raise __FUNCTION__ IO_Format
    (Printf.sprintf "At offset %d of the read-bases column: %s"
       (Lexing.lexeme_start lexbuf) message)
}

let digit = ['0'-'9']
let forward_base = ['A' 'C' 'G' 'T' 'N']
let reverse_base = ['a' 'c' 'g' 't' 'n']

rule bases = parse
  (* The read agrees with the reference; the case says which strand it is on *)
  | '.' { Mp_REFERENCE_FWD }
  | ',' { Mp_REFERENCE_REV }
  (* An explicit base.  The case is the strand and not part of the call, so it
     is taken off here rather than left for every caller to take off again *)
  | forward_base as c { Mp_BASE_FWD c }
  | reverse_base as c { Mp_BASE_REV (Char.uppercase_ascii c) }
  (* A position inside a deletion announced by an earlier line.  Older samtools
     wrote '*' whatever the strand; since 1.7 it writes '#' for the reverse *)
  | '*' { Mp_GAP_FWD }
  | '#' { Mp_GAP_REV }
  (* The read skips the reference here, which is what an intron looks like *)
  | '>' { Mp_SKIP_FWD }
  | '<' { Mp_SKIP_REV }
  (* A read ends after the base just read *)
  | '$' { Mp_END }
  (* A read begins with the base about to be read, and the character after the
     caret is its mapping quality, offset like everything else by 33 *)
  | '^' _ as s { Mp_START (Char.code s.[1] - 33) }
  (* An indel following the base just read.  The length is written out and then
     that many bases follow it, which is the whole reason this is a lexer and
     not a grammar *)
  | ('+' | '-') as sign (digit+ as n) {
      let n =
        match int_of_string_opt n with
        | Some n when n > 0 -> n
        | Some n -> raise_at lexbuf (Printf.sprintf "Indel of length %d" n)
        | None -> raise_at lexbuf (Printf.sprintf "Unreadable indel length %S" n) in
      let s = indel_bases n (Buffer.create n) lexbuf in
      if sign = '+' then Mp_INSERTION s else Mp_DELETION s
    }
  | eof { Mp_EOF }
  | _ as c { raise_at lexbuf (Printf.sprintf "Unexpected character %C" c) }

(* Take exactly [n] characters, whatever they are: the length said so, and a
   base of an indel is not otherwise constrained *)
and indel_bases n buf = parse
  | _ as c {
      Buffer.add_char buf (Char.uppercase_ascii c);
      if n > 1 then
        indel_bases (n - 1) buf lexbuf
      else
        Buffer.contents buf
    }
  | eof {
      raise_at lexbuf
        (Printf.sprintf "Indel runs %d %s past the end of the column"
           n (if n = 1 then "character" else "characters"))
    }
