(*
    Mpileup_Base.ml -- (c) 2026 Paolo Ribeca, <paolo.ribeca@gmail.com>

    This file is part of BiOCamLib, the OCaml foundations upon which
    a number of the bioinformatics tools I developed are built.

    Mpileup_Base.ml holds the format-independent types of the pileup
    format: what one read contributes at one position, and the record
    of everything piled up there.  No parsing lives here.

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

open Better

(* What one read says about one position.  The strand is carried separately
   rather than left implicit in the case of a letter, which is how the format
   writes it -- '.' and an upper-case base are forward, ',' and a lower-case one
   reverse -- because a caller that wants to count bases should not have to know
   that, and one that wants to count strands should not have to lower-case. *)
module Call =
  struct
    type t =
      (* '.' or ',': the read agrees with the reference *)
      | Reference
      (* An explicit base, upper- or lower-cased in the file according to the
         strand and normalised to upper case here *)
      | Base of char
      (* '*' or '#': this position falls inside a deletion reported by an
         earlier line, and the read has nothing to say about it *)
      | Gap
      (* '<' or '>': the read skips the reference here, which is what an
         alignment over an intron looks like *)
      | Skip
    let to_string = function
      | Reference -> "."
      | Base c -> String.make 1 c
      | Gap -> "*"
      | Skip -> ">"
  end

(* An indel is written after the base it follows, as a sign, a length and that
   many characters: '+2AC' is two bases inserted after this read's base.  The
   sequence is normalised to upper case, the format's own case carrying the
   strand of the read rather than anything about the inserted bases. *)
module Indel =
  struct
    type t =
      | Insertion of string
      | Deletion of string
    let to_string = function
      | Insertion s -> "+" ^ s
      | Deletion s -> "-" ^ s
    let length = function
      | Insertion s | Deletion s -> String.length s
  end

(* One read's whole contribution at one position: what it says, how confident
   the sequencer was, whatever indel follows, and whether the read begins or
   ends here.  A read that begins here carries the mapping quality that '^'
   introduces; every other case has none. *)
module Read =
  struct
    type t = {
      call: Call.t;
      strand: Sequences.Types.strand_t;
      (* Decoded with the caller's offset.  A call that consumes no quality --
         which is none of them, every call in the format consuming one -- would
         carry zero, and an indel's own bases never carry any: mpileup reports
         them without qualities, and the caller is expected to look the read up
         elsewhere if it wants them *)
      quality: int;
      indel: Indel.t option;
      starts_read: int option;
      ends_read: bool
    }
  end

(* A whole pileup line.  [reads] is in the order the file wrote them, which is
   the order of the reads in the alignment, and is worth keeping: a caller
   summarising by strand or by quality can do so in one pass, and one that wants
   to know which read said what still can. *)
type t = {
  seq: string;
  (* 1-based, as the format writes it and as every tool that consumes one
     expects.  This is the one place in the library where a coordinate is not
     converted on the way in: a pileup line is a report about a position rather
     than an interval, and [Sequences.Types] has nothing to say about it *)
  pos: int;
  reference: char;
  depth: int;
  reads: Read.t array
}

let empty = {
  seq = "";
  pos = 0;
  reference = 'N';
  depth = 0;
  reads = [||]
}
