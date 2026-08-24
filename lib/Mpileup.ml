(*
    Mpileup.ml -- (c) 2026 Paolo Ribeca, <paolo.ribeca@gmail.com>

    This file is part of BiOCamLib, the OCaml foundations upon which
    a number of the bioinformatics tools I developed are built.

    Mpileup.ml reads the pileup format: the public half of a
    subsystem whose types are in Mpileup_Base, whose read-bases column
    is tokenised by Mpileup_Lex and assembled by Mpileup_Parse.  What
    lives here is everything the grammar cannot know about -- the
    columns either side of it, the qualities that are counted out
    against the calls, and the file the lines come from.

    The format is older than SAM by about a decade, and older than the
    tool most people meet it through: samtools writes it, and named a
    subcommand after it, but did not invent it.  Its habits are worth
    reading in that light -- the counting of qualities against calls,
    and an indel written as a length and then that many characters,
    are of a piece with CIGAR and with the rest of what was in the air
    at Sanger before either had a specification.

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

include (
  struct
    include Mpileup_Base
    (* The columns, found by walking to each tab rather than by splitting the
       line into a list: the read-bases column of a deep pileup is long, and the
       point of the exercise is not to touch it more often than once. *)
    let column line lo hi = String.sub line lo (hi - lo)
    let columns_of_line line =
      let len = String.length line in
      let starts = Array.make 6 0 and stops = Array.make 6 0 in
      let n = ref 0 and pos = ref 0 and finished = ref false in
      while not !finished && !n < 6 do
        let stop =
          match String.index_from_opt line !pos '\t' with
          | Some i -> i
          | None -> len in
        starts.(!n) <- !pos;
        stops.(!n) <- stop;
        incr n;
        if stop >= len then
          finished := true
        else
          pos := stop + 1
      done;
      !n, starts, stops
    let raise_in ~where message =
      Exception.raise __FUNCTION__ IO_Format
        (match where with
         | None -> message
         | Some n -> Printf.sprintf "On line %d: %s" n message)
    (* One line.  [quality_offset] is 33 for everything written this century;
       [line_number], when given, is put in front of whatever goes wrong, a
       pileup being long enough that the number is most of the diagnosis. *)
    let of_line ?(quality_offset = 33) ?line_number line =
      let where = line_number in
      let n_columns, starts, stops = columns_of_line line in
      if n_columns < 6 then
        raise_in ~where
          (Printf.sprintf "Expected at least 6 columns, found %d" n_columns);
      let field i = column line starts.(i) stops.(i) in
      let seq = field 0 and reference = field 2 and bases = field 4 and quals = field 5 in
      let pos =
        match int_of_string_opt (field 1) with
        | Some p when p > 0 -> p
        | _ -> raise_in ~where (Printf.sprintf "Invalid position %S" (field 1)) in
      let depth =
        match int_of_string_opt (field 3) with
        | Some d when d >= 0 -> d
        | _ -> raise_in ~where (Printf.sprintf "Invalid depth %S" (field 3)) in
      if String.length reference <> 1 then
        raise_in ~where (Printf.sprintf "Invalid reference base %S" reference);
      (* A line at depth zero writes '*' in both of the columns that would
         otherwise hold calls and qualities.  That asterisk is a placeholder and
         not a deleted base, and reading it as one would invent a read where the
         file says there are none. *)
      let reads =
        if depth = 0 then
          [||]
        else begin
          let parsed =
            try
              Mpileup_Parse.read_bases Mpileup_Lex.bases (Lexing.from_string bases)
            with Exception.E (_, _, message) -> raise_in ~where message in
          let reads = Array.of_list parsed and n_quals = String.length quals in
          if Array.length reads <> n_quals then
            raise_in ~where
              (Printf.sprintf
                 "The bases and qualities columns disagree: %d %s against %d"
                 (Array.length reads)
                 (String.pluralize_int "call" (Array.length reads)) n_quals);
          Array.mapi
            (fun i read ->
              { read with Read.quality = Char.code quals.[i] - quality_offset })
            reads
        end in
      (* The depth column is what the aligner counted, and a reader that
         silently disagreed with it would be hiding the more interesting of the
         two possibilities: that the line is truncated. *)
      if depth <> Array.length reads then
        raise_in ~where
          (Printf.sprintf "Depth column says %d, the bases column holds %d"
             depth (Array.length reads));
      { seq; pos; reference = reference.[0]; depth; reads }
    (* Back out again, which is what says the reader kept everything: the case
       of a base is the strand, an indel goes after the base it follows, and a
       read that begins or ends here says so either side of it. *)
    let to_string ?(quality_offset = 33) t =
      let bases = Buffer.create (Array.length t.reads * 2)
      and quals = Buffer.create (Array.length t.reads) in
      if t.reads = [||] then begin
        Buffer.add_char bases '*';
        Buffer.add_char quals '*'
      end else
        Array.iter
          (fun read ->
            let forward =
              match read.Read.strand with
              | Sequences.Types.Forward _ -> true
              | Sequences.Types.Reverse _ -> false in
            Option.iter
              (fun q -> Printf.bprintf bases "^%c" (Char.chr (q + quality_offset)))
              read.Read.starts_read;
            (match read.Read.call with
             | Call.Reference -> Buffer.add_char bases (if forward then '.' else ',')
             | Call.Base c ->
               Buffer.add_char bases
                 (if forward then c else Char.lowercase_ascii c)
             | Call.Gap -> Buffer.add_char bases (if forward then '*' else '#')
             | Call.Skip -> Buffer.add_char bases (if forward then '>' else '<'));
            Option.iter
              (fun indel ->
                let sign, s =
                  match indel with
                  | Indel.Insertion s -> '+', s
                  | Indel.Deletion s -> '-', s in
                Printf.bprintf bases "%c%d%s" sign (String.length s) s)
              read.Read.indel;
            if read.Read.ends_read then
              Buffer.add_char bases '$';
            Buffer.add_char quals (Char.chr (read.Read.quality + quality_offset)))
          t.reads;
      Printf.sprintf "%s\t%d\t%c\t%d\t%s\t%s"
        t.seq t.pos t.reference t.depth (Buffer.contents bases) (Buffer.contents quals)
    let iter_string ?quality_offset f s =
      List.iteri
        (fun i line ->
          if line <> "" then
            f (of_line ?quality_offset ~line_number:(i + 1) line))
        (String.Split.on_char_as_list '\n' s)
    let iter ?quality_offset f path =
      let ic = open_in path and n = ref 0 in
      Fun.protect ~finally:(fun () -> close_in ic)
        (fun () ->
          try
            while true do
              let line = input_line ic in
              incr n;
              if line <> "" then
                f (of_line ?quality_offset ~line_number:!n line)
            done
          with End_of_file -> ())
  end: sig
    module Call:
      sig
        type t =
          | Reference
          | Base of char
          | Gap
          | Skip
        val to_string: t -> string
      end
    module Indel:
      sig
        type t =
          | Insertion of string
          | Deletion of string
        val to_string: t -> string
        val length: t -> int
      end
    module Read:
      sig
        type t = {
          call: Call.t;
          strand: Sequences.Types.strand_t;
          quality: int;
          indel: Indel.t option;
          starts_read: int option;
          ends_read: bool
        }
      end
    type t = {
      seq: string;
      pos: int;
      reference: char;
      depth: int;
      reads: Read.t array
    }
    val empty: t
    val of_line: ?quality_offset:int -> ?line_number:int -> string -> t
    val to_string: ?quality_offset:int -> t -> string
    (* Over every line of a string, and of a file, the line number going into
       whatever goes wrong *)
    val iter_string: ?quality_offset:int -> (t -> unit) -> string -> unit
    val iter: ?quality_offset:int -> (t -> unit) -> string -> unit
  end
)
