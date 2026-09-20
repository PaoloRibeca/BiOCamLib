(*
    Mpileup.ml -- (c) 2026 Paolo Ribeca, <paolo.ribeca@gmail.com>

    This file is part of BiOCamLib, the OCaml foundations upon which
    a number of the bioinformatics tools I developed are built.

    Mpileup.ml reads the pileup format: one line per reference
    position, saying what every read aligned there had to say about it.

    The format is older than SAM by about a decade, and older than the
    tool most people meet it through: samtools writes it and named a
    subcommand after it, but did not invent it.  Its habits are worth
    reading in that light -- qualities counted out against calls, an
    indel written as a length and then that many characters -- being of
    a piece with CIGAR and the rest of what was in the air at Sanger
    before either had a specification.

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
    (* What one read says about one position.  The strand is carried in a field
       of its own rather than left implicit in the case of a letter, which is how
       the format writes it: a caller counting bases would otherwise have to
       lower-case first, and one counting strands to upper-case. *)
    module Call =
      struct
        type t =
          (* '.' or ',': the read agrees with the reference *)
          | Reference
          (* An explicit base, cased in the file by strand and upper here *)
          | Base of char
          (* '*' or '#': inside a deletion an earlier line announced *)
          | Gap
          (* '<' or '>': the read skips the reference, as over an intron *)
          | Skip
        let to_string = function
          | Reference -> "."
          | Base c -> String.make 1 c
          | Gap -> "*"
          | Skip -> ">"
      end
    (* Written after the base it follows, as a sign, a length and that many
       characters: '+2AC' is two bases inserted after this read's base. *)
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
    module Read =
      struct
        type t = {
          call: Call.t;
          strand: Sequences.Types.strand_t;
          (* Mutable, and only because the qualities are a column of their own:
             the record is built while the calls column is walked and cannot
             know its quality yet, and filling the field afterwards is one
             record per read where rebuilding it would be two.  Nothing else
             writes to it *)
          mutable quality: int;
          indel: Indel.t option;
          starts_read: int option;
          ends_read: bool
        }
        (* Somewhere to point an array at before it is filled *)
        let placeholder = {
          call = Call.Gap;
          strand = Sequences.Types.forward;
          quality = 0;
          indel = None;
          starts_read = None;
          ends_read = false
        }
      end
    (* [reads] is in the order the file wrote them, which is the order of the
       reads in the alignment: a caller summarising by strand or by quality does
       so in one pass, and one that wants to know which read said what still
       can. *)
    type t = {
      seq: string;
      (* 1-based, as the format writes it.  This is the one place in the library
         where a coordinate is not converted on the way in: a pileup line is a
         report about a position rather than an interval, and [Sequences.Types]
         has nothing to say about it *)
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
    let raise_in ~where message =
      Exception.raise __FUNCTION__ IO_Format
        (match where with
         | None -> message
         | Some n -> Printf.sprintf "On line %d: %s" n message)
    (* The columns, found by walking to each tab rather than by splitting the
       line into a list: the read-bases column of a deep pileup is long, and the
       point is not to touch it more often than once. *)
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
    (* The read-bases column, walked one character at a time, handing each call
       to whoever asked for it.  Matching on characters rather than on
       one-character strings is the whole performance story of this reader: the
       version it replaces took [String.sub s i 1] per base and compared the
       result against string literals, which is an allocation for every base in
       a file that has billions of them.  An ocamllex-and-menhir reader was
       written in between and measured slower still; the design note carries the
       figures.
       It is one scanner rather than two because there are two things worth
       doing with a pileup line -- keeping every read, and counting genotypes --
       and the second must not go through the first: at the depths this format
       exists for, one record per read per position is a great deal of garbage
       where a table of half a dozen counters would do.  Writing the walk twice
       would be worse still, the two copies differing by the time anyone
       noticed. *)
    let scan ~where bases quals ~quality_offset ~on_call ~on_indel ~on_end =
      let len = String.length bases and n_quals = String.length quals in
      let i = ref 0 and qpos = ref 0 and pending_start = ref None in
      let fail at message =
        raise_in ~where
          (Printf.sprintf "At offset %d of the read-bases column: %s" at message) in
      let call c strand =
        (* Noticed here rather than at the end, which says where.  The other
           direction -- more qualities than calls -- can only be seen once the
           column has run out, and says so below in the same words. *)
        if !qpos >= n_quals then
          fail !i
            (Printf.sprintf
               "The bases and qualities columns disagree: more than %d %s"
               n_quals (String.pluralize_int "call" n_quals));
        on_call c strand
          (Char.code (String.unsafe_get quals !qpos) - quality_offset)
          !pending_start;
        pending_start := None;
        incr qpos in
      while !i < len do
        (match String.unsafe_get bases !i with
         | '.' -> call Call.Reference Sequences.Types.forward
         | ',' -> call Call.Reference Sequences.Types.reverse
         | 'A' | 'C' | 'G' | 'T' | 'N' as c ->
           call (Call.Base c) Sequences.Types.forward
         | 'a' | 'c' | 'g' | 't' | 'n' as c ->
           call (Call.Base (Char.uppercase_ascii c)) Sequences.Types.reverse
         (* A deleted base is written '*' on BOTH strands unless the writer
            was asked for --reverse-del, which spells the reverse ones '#'.  So
            a '#' does say reverse, but a '*' says nothing at all about strand
            and the forward here is a placeholder, not a reading of the file.
            It does not reach a genotype -- a read inside a deletion votes for
            nothing -- but a caller filtering by strand should know that the
            gaps it keeps or drops were never labelled *)
         | '*' -> call Call.Gap Sequences.Types.forward
         | '#' -> call Call.Gap Sequences.Types.reverse
         | '>' -> call Call.Skip Sequences.Types.forward
         | '<' -> call Call.Skip Sequences.Types.reverse
         (* A dollar and an indel both belong to the call just made *)
         | '$' ->
           if !qpos = 0 then
             fail !i "A read-end marker before any call";
           on_end ()
         | '^' ->
           (* The character after the caret is the read's mapping quality, and
              may be anything at all -- including something that would otherwise
              have been read as a call *)
           incr i;
           if !i >= len then
             fail (!i - 1) "A read-start marker at the end of the column";
           pending_start :=
             Some (Char.code (String.unsafe_get bases !i) - quality_offset)
         | '+' | '-' as sign ->
           let at = !i and how_many = ref 0 and digits = ref 0 in
           let scanning = ref true in
           while !scanning do
             incr i;
             if !i >= len then
               fail at "An indel with no bases after its length"
             else
               match String.unsafe_get bases !i with
               | '0' .. '9' as d ->
                 how_many := !how_many * 10 + (Char.code d - Char.code '0');
                 incr digits
               | _ -> scanning := false
           done;
           if !digits = 0 then
             fail at "An indel with no length";
           if !how_many <= 0 then
             fail at (Printf.sprintf "An indel of length %d" !how_many);
           if !i + !how_many > len then
             fail at
               (Printf.sprintf "An indel running %d %s past the end of the column"
                  (!i + !how_many - len)
                  (String.pluralize_int "character" (!i + !how_many - len)));
           if !qpos = 0 then
             fail at "An indel before any call";
           on_indel sign (String.uppercase_ascii (String.sub bases !i !how_many));
           i := !i + !how_many - 1
         | c -> fail !i (Printf.sprintf "Unexpected character %C" c));
        incr i
      done;
      if !qpos <> n_quals then
        raise_in ~where
          (Printf.sprintf "The bases and qualities columns disagree: %d %s against %d"
             !qpos (String.pluralize_int "call" !qpos) n_quals);
      !qpos
    (* Every read the line reports, in the order it reported them. *)
    let read_bases ~where bases quals ~quality_offset =
      let calls = ref [] in
      let amend f =
        match !calls with
        | last :: rest -> calls := f last :: rest
        | [] -> () in
      let n_calls =
        scan ~where bases quals ~quality_offset
          ~on_call:(fun call strand quality starts_read ->
            List.accum calls
              { Read.call; strand; quality; indel = None; starts_read;
                ends_read = false })
          ~on_indel:(fun sign s ->
            amend
              (fun last ->
                { last with
                  Read.indel =
                    Some (if sign = '+' then Indel.Insertion s
                          else Indel.Deletion s) }))
          ~on_end:(fun () -> amend (fun last -> { last with Read.ends_read = true })) in
      (* [calls] is backwards, having been accumulated, so the array is filled
         from the end rather than reversed into a second list first. *)
      let reads = Array.make n_calls Read.placeholder in
      List.iteri (fun k read -> reads.(n_calls - 1 - k) <- read) !calls;
      reads
    (* A distribution of qualities, as a dense histogram rather than a tree.
       Phred qualities are small and bounded, so a bucket per value is one
       increment and no allocation, where a map is a lookup and a rebalance with
       allocation along the path -- and this is counted once per base at every
       position of a genome, which is the one place in this library where that
       difference is worth caring about.  Merging is adding two arrays, which is
       what the null distribution of a variant needs: everything that is not
       that variant, taken together. *)
    module Qualities =
      struct
        (* Wide enough for every Phred scale in use; anything outside is the
           caller's mistake and is refused rather than folded into the edge *)
        let range = 128
        type t = int array
        let make () = Array.make range 0
        let add t q =
          if q < 0 || q >= range then
            Exception.raise __FUNCTION__ IO_Format
              (Printf.sprintf "Quality %d is outside 0..%d" q (range - 1));
          t.(q) <- t.(q) + 1
        let cardinal t = Array.fold_left ( + ) 0 t
        (* Empty buckets are skipped, so a caller rebuilding a sparse structure
           from this pays for the qualities that are there rather than for the
           whole scale *)
        let iter f t = Array.iteri (fun q c -> if c <> 0 then f q c) t
        let merge_into ~into t = Array.iteri (fun q n -> into.(q) <- into.(q) + n) t
        let mean t =
          let n = ref 0 and acc = ref 0 in
          Array.iteri (fun q c -> n := !n + c; acc := !acc + q * c) t;
          if !n = 0 then 0. else float_of_int !acc /. float_of_int !n
        (* Two passes over 128 buckets, which is cheap, rather than the sum of
           squares less the square of the mean, which loses digits when the
           qualities are large and alike -- as they are on good data *)
        let variance t =
          let n = cardinal t in
          if n < 2 then
            0.
          else begin
            let mean = mean t and acc = ref 0. in
            Array.iteri
              (fun q c ->
                if c <> 0 then begin
                  let d = float_of_int q -. mean in
                  acc := !acc +. float_of_int c *. d *. d
                end)
              t;
            !acc /. float_of_int (n - 1)
          end
        (* The mean of what is left after the lowest [fraction] of the
           observations is dropped.  SiNPle drops the lowest quarter of a
           variant's qualities before comparing it against the null, on the
           grounds that sequencing errors sit there even in a real variant *)
        let mean_above_fraction t fraction =
          let n = cardinal t in
          if n = 0 then
            0.
          else begin
            let to_drop = int_of_float (ceil (fraction *. float_of_int n)) in
            let dropped = ref 0 and kept = ref 0 and acc = ref 0 in
            Array.iteri
              (fun q c ->
                let here = min c (max 0 (to_drop - !dropped)) in
                dropped := !dropped + here;
                let keep = c - here in
                kept := !kept + keep;
                acc := !acc + q * keep)
              t;
            if !kept = 0 then 0. else float_of_int !acc /. float_of_int !kept
          end
      end
    (* One genotype at one position, in the sense the model uses: a symbol that
       reads voted for.  The reference has no special status among them -- a
       read that wrote '.' and one that spelled the base out are the same vote,
       and are resolved to the same symbol here. *)
    module Genotype =
      struct
        type kind_t =
          | Base
          (* A single base inserted or deleted, against several: the two carry
             different baseline error rates in the model that consumes this *)
          | Short_indel
          | Long_indel
        type t = {
          symbol: string;
          kind: kind_t;
          count: int;
          (* Absent for an indel, and absent rather than zero: the machine
             assigns qualities to bases, including the bases within an
             insertion, but none to the presence of the indel itself.  A caller
             that wants one supplies it from its own parameters, which is what
             the model does; inventing a zero here would put a number where
             there is no measurement *)
          qualities: Qualities.t option
        }
      end
    module Summary =
      struct
        type t = {
          seq: string;
          pos: int;
          reference: char;
          (* What the depth column said *)
          depth: int;
          (* Reads that voted for something.  Not the same as [depth]: a read
             inside a deletion from an earlier line, or skipping the reference
             over an intron, is counted by the aligner but votes for nothing *)
          voting: int;
          gaps: int;
          skips: int;
          genotypes: Genotype.t list
        }
        (* One line per position, the genotypes sorted by symbol, each as
           symbol:kind:count and, for a base, its qualities as quality=times
           pairs -- a form two pileups can be compared in whatever wrote them *)
        let to_string t =
          let kind = function
            | Genotype.Base -> 'B'
            | Genotype.Short_indel -> 'S'
            | Genotype.Long_indel -> 'L' in
          let genotypes =
            List.sort (fun (a: Genotype.t) (b: Genotype.t) -> compare a.symbol b.symbol) t.genotypes
              |> List.map
                (fun (g: Genotype.t) ->
                  let quals = Buffer.create 64 in
                  Option.iter (Qualities.iter (fun q c -> Printf.bprintf quals ",%d=%d" q c)) g.qualities;
                  Printf.sprintf "%s:%c:%d%s" g.symbol (kind g.kind) g.count (Buffer.contents quals)) in
          Printf.sprintf "%s\t%d\t%c\t%d\t%d\t%d\t%d\t%s" t.seq t.pos t.reference t.depth t.voting t.gaps
            t.skips (String.concat " " genotypes)
      end
    let base_index = function
      | 'A' -> 0 | 'C' -> 1 | 'G' -> 2 | 'T' -> 3 | 'N' -> 4 | _ -> -1
    let base_of_index = [| 'A'; 'C'; 'G'; 'T'; 'N' |]
    (* The list the model consumes, built the same way from a line and from the
       mapper's placements: the bases in a fixed order, each with its qualities,
       then the indels, which have none *)
    let genotypes_of counts quals indels =
      let acc = ref [] in
      List.iter
        (fun (symbol, count) ->
          List.accum acc
            { Genotype.symbol; count;
              kind =
                if String.length symbol = 2 then
                  Genotype.Short_indel
                else
                  Genotype.Long_indel;
              qualities = None })
        indels;
      for i = 4 downto 0 do
        if counts.(i) <> 0 then
          List.accum acc
            { Genotype.symbol = String.make 1 base_of_index.(i);
              kind = Genotype.Base;
              count = counts.(i);
              qualities = quals.(i) }
      done;
      !acc
    (* One line.  [quality_offset] is 33 for everything written this century;
       [line_number], when given, goes in front of whatever turns out to be
       wrong, a pileup being long enough that the number is most of the
       diagnosis. *)
    let of_line ?(quality_offset = 33) ?line_number line =
      let where = line_number in
      let n_columns, starts, stops = columns_of_line line in
      if n_columns < 6 then
        raise_in ~where
          (Printf.sprintf "Expected at least 6 columns, found %d" n_columns);
      let column i = String.sub line starts.(i) (stops.(i) - starts.(i)) in
      let seq = column 0 and reference = column 2 in
      let pos =
        match int_of_string_opt (column 1) with
        | Some p when p > 0 -> p
        | _ -> raise_in ~where (Printf.sprintf "Invalid position %S" (column 1)) in
      let depth =
        match int_of_string_opt (column 3) with
        | Some d when d >= 0 -> d
        | _ -> raise_in ~where (Printf.sprintf "Invalid depth %S" (column 3)) in
      if String.length reference <> 1 then
        raise_in ~where (Printf.sprintf "Invalid reference base %S" reference);
      (* A line at depth zero writes '*' in both of the columns that would
         otherwise hold calls and qualities.  That asterisk is a placeholder and
         not a deleted base, and reading it as one would invent a read where the
         file says there are none. *)
      let reads =
        if depth = 0 then
          [||]
        else
          read_bases ~where (column 4) (column 5) ~quality_offset in
      (* The depth column is what the aligner counted, and a reader that silently
         disagreed with it would be hiding the more interesting of the two
         possibilities: that the line is truncated. *)
      if depth <> Array.length reads then
        raise_in ~where
          (Printf.sprintf "Depth column says %d, the bases column holds %d"
             depth (Array.length reads));
      { seq; pos; reference = reference.[0]; depth; reads }
    (* One line, counted rather than kept.  This is the shape the variant
       callers want, and it is reached without building a read: at the depths
       this format exists for -- tens of thousands of reads at a position, for
       every position of a genome -- one record per read is a great deal of
       rubbish to make and collect in order to add one to a counter.
       [strand], when given, keeps only the reads on it: a directional protocol
       is evidence about one strand and the other's reads are not evidence
       about it. *)
    let summarize ?(quality_offset = 33) ?strand ?line_number line =
      let where = line_number in
      let n_columns, starts, stops = columns_of_line line in
      if n_columns < 6 then
        raise_in ~where
          (Printf.sprintf "Expected at least 6 columns, found %d" n_columns);
      let column i = String.sub line starts.(i) (stops.(i) - starts.(i)) in
      let seq = column 0 and reference = column 2 in
      let pos =
        match int_of_string_opt (column 1) with
        | Some p when p > 0 -> p
        | _ -> raise_in ~where (Printf.sprintf "Invalid position %S" (column 1)) in
      let depth =
        match int_of_string_opt (column 3) with
        | Some d when d >= 0 -> d
        | _ -> raise_in ~where (Printf.sprintf "Invalid depth %S" (column 3)) in
      if String.length reference <> 1 then
        raise_in ~where (Printf.sprintf "Invalid reference base %S" reference);
      let reference = reference.[0] in
      (* Five slots for the bases and a short list for the indels, there being
         at most a handful of either at one position: a map would be a lookup
         and an allocation per base to save a scan of five *)
      let counts = Array.make 5 0
      and quals = Array.init 5 (fun _ -> Qualities.make ())
      and indels = ref [] and gaps = ref 0 and skips = ref 0 and voting = ref 0 in
      let wanted s =
        match strand, s with
        | None, _ -> true
        | Some (Sequences.Types.Forward _), Sequences.Types.Forward _ -> true
        | Some (Sequences.Types.Reverse _), Sequences.Types.Reverse _ -> true
        | Some _, _ -> false in
      let last_wanted = ref true in
      if depth > 0 then begin
        let n_calls =
          scan ~where (column 4) (column 5) ~quality_offset
            ~on_call:(fun call s quality _ ->
              last_wanted := wanted s;
              if !last_wanted then
                match call with
                | Call.Gap -> incr gaps
                | Call.Skip -> incr skips
                | Call.Reference | Call.Base _ ->
                  let c =
                    match call with
                    | Call.Base c -> c
                    | _ -> reference in
                  let i = base_index (Char.uppercase_ascii c) in
                  if i < 0 then
                    raise_in ~where
                      (Printf.sprintf "Not a base this reader knows: %C" c);
                  counts.(i) <- counts.(i) + 1;
                  Qualities.add quals.(i) quality;
                  incr voting)
            ~on_indel:(fun sign s ->
              if !last_wanted then begin
                let symbol = (if sign = '+' then "+" else "-") ^ s in
                match List.assoc_opt symbol !indels with
                | Some n -> indels := (symbol, n + 1) :: List.remove_assoc symbol !indels
                | None -> indels := (symbol, 1) :: !indels
              end)
            ~on_end:(fun () -> ()) in
        if depth <> n_calls then
          raise_in ~where
            (Printf.sprintf "Depth column says %d, the bases column holds %d"
               depth n_calls)
      end;
      let genotypes = genotypes_of counts (Array.map Option.some quals) !indels in
      { Summary.seq; pos; reference; depth; voting = !voting;
        gaps = !gaps; skips = !skips; genotypes }
    (* Back out again, which is what says the reading kept everything: the case
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
               Buffer.add_char bases (if forward then c else Char.lowercase_ascii c)
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
    (* THE SAME SUMMARIES FROM THE MAPPER'S OWN OUTPUT.  gem3-mapper -F MAP says
       where every read went and, through its alignment string, what the read
       had to say at each position it covers; walking that against the reference
       gives what samtools mpileup would have written, with no SAM in between.
       The records come in the reads' order and a pileup goes in the reference's,
       so what each placement says is kept in a compact form -- where it starts,
       one call per position it covers, two bytes, and its indels, the tag, the
       sequence and the qualities being dropped -- and sorted by where it starts
       once the input is over; the placements then reach cells that live only
       while some placement still covers them, and the summaries come out in
       order, one per position, uncovered ones included as mpileup -a writes
       them.  The sort is where the memory goes, and it can be bounded: past a
       budget what has been kept is sorted and written to a temporary file, a
       run, and the runs are merged when the input is over, so that the input
       can be a pipe whatever its size.
       What a read says follows the pileup: a base it aligns, with its quality,
       or a gap where it carries a deletion, and an insertion or a deletion as an
       indel attached to the base before it on the forward strand -- the bases of
       an insertion carrying no quality, as in the format, and an indel before
       the read's first forward base attaching to nothing, as the format cannot
       say it.  A read on the reverse strand has its alignment string in its own
       direction, so it is walked forward while the reference is walked back from
       the far end of the span, its bases complemented, and an indel's sequence
       is written the forward way.  Every placement the reader delivers counts,
       so a read on several copies of a repeat votes at each of them; the strata
       to keep are the reader's business.  What a read says is checked against
       the reference as it goes -- a matched base must be the reference's, a
       mismatch letter the reference base as the read sees it -- so that a
       reference other than the one the reads were mapped to is refused rather
       than counted. *)
    module Gem =
      struct
        let gap_code = 5 and skip_code = 6
        let complement = function
          | 'A' -> 'T' | 'C' -> 'G' | 'G' -> 'C' | 'T' -> 'A' | c -> c
        (* Where a placement starts on its contig and how many positions it covers: every one
           of them gets exactly one call, a vote, a gap or a skip *)
        let extent m =
          let span =
            List.fold_left
              (fun acc -> function
                | Files.Gem.Gigar.Match n | Files.Gem.Gigar.Deletion n | Files.Gem.Gigar.Splice n ->
                  acc + n
                | Files.Gem.Gigar.Mismatch _ -> acc + 1
                | Files.Gem.Gigar.Trim _ | Files.Gem.Gigar.Insertion _ -> acc)
              0 m.Files.Gem.gigar in
          m.Files.Gem.position, span
        (* One placement walked against its contig: what the read says at each position it
           covers goes to [record] as (position, code, strand, quality) and each indel to
           [note_indel] as (position, symbol, strand) *)
        let walk ~where ~quality_offset ~missing_quality ~record ~note_indel (name, sequence) read
            m =
          let malformed message = Exception.raise where IO_Format message in
          let tag = read.Files.Gem.tag in
          let len = String.length sequence in
          let bases = String.uppercase_ascii read.Files.Gem.sequence in
          let quality_at i =
            match read.Files.Gem.qualities, missing_quality with
            | Some quals, _ -> Char.code quals.[i] - quality_offset
            | None, Some quality -> quality
            | None, None ->
              malformed "the records carry no qualities and no quality was supplied for them" in
          let forward = m.Files.Gem.forward in
          let s = if forward then 0 else 1 in
          let first, span = extent m in
          let last = first + span - 1 in
          if first < 1 || last > len then
            Printf.sprintf "read '%s' is placed over %d-%d of '%s', which is %d long" tag first
              last name len
              |> malformed;
          (* The read is walked in its own direction; the reference goes with it on the forward
             strand and against it on the reverse, from the far end of the span *)
          let i = ref 0 and r = ref (if forward then first else last) and consumed = ref 0
          and d = if forward then 1 else -1 in
          let base_at i =
            let c = bases.[i] in
            if forward then c else complement c in
          let vote () =
            let base = base_at !i in
            let code = base_index base in
            if code < 0 then
              Printf.sprintf "read '%s' carries a base this reader does not know, %C" tag base
                |> malformed;
            let quality = quality_at !i in
            if quality < 0 || quality >= Qualities.range then
              Printf.sprintf "read '%s' carries a quality of %d, outside 0..%d" tag quality
                (Qualities.range - 1)
                |> malformed;
            record !r code s quality;
            incr i;
            r := !r + d;
            incr consumed in
          List.iter
            (function
              | Files.Gem.Gigar.Match n ->
                for _ = 1 to n do
                  let base = base_at !i and expected = sequence.[!r - 1] in
                  if base <> expected && expected <> 'N' then
                    Printf.sprintf "read '%s' matches %C at %s:%d, where the reference has %C" tag
                      base name !r expected
                      |> malformed;
                  vote ()
                done
              | Files.Gem.Gigar.Mismatch letter ->
                let letter = if forward then letter else complement letter in
                if letter <> sequence.[!r - 1] then
                  Printf.sprintf "read '%s' mismatches %C at %s:%d, where the reference has %C" tag
                    letter name !r sequence.[!r - 1]
                    |> malformed;
                vote ()
              | Files.Gem.Gigar.Trim n ->
                i := !i + n
              | Files.Gem.Gigar.Insertion n ->
                (* After the base before it on the forward strand: the last consumed going
                   forward, the next to be consumed going back *)
                let inserted = String.sub bases !i n in
                let inserted = if forward then inserted else Sequences.Lint.rc inserted in
                if forward && !consumed > 0 then
                  note_indel (!r - 1) ("+" ^ inserted) s
                else if not forward && !consumed < span then
                  note_indel !r ("+" ^ inserted) s;
                i := !i + n
              | Files.Gem.Gigar.Deletion n ->
                let lo, hi = if forward then !r, !r + n - 1 else !r - n + 1, !r in
                if lo < 1 || hi > len then
                  Printf.sprintf "read '%s' deletes %d-%d of '%s', which is %d long" tag lo hi name
                    len
                    |> malformed;
                let deleted = String.sub sequence (lo - 1) n in
                if forward && !consumed > 0 then
                  note_indel (!r - 1) ("-" ^ deleted) s
                else if not forward && !consumed + n < span then
                  note_indel (!r - n) ("-" ^ deleted) s;
                for pos = lo to hi do
                  record pos gap_code s 0
                done;
                r := !r + d * n;
                consumed := !consumed + n
              | Files.Gem.Gigar.Splice n ->
                let lo, hi = if forward then !r, !r + n - 1 else !r - n + 1, !r in
                if lo < 1 || hi > len then
                  Printf.sprintf "read '%s' skips %d-%d of '%s', which is %d long" tag lo hi name
                    len
                    |> malformed;
                for pos = lo to hi do
                  record pos skip_code s 0
                done;
                r := !r + d * n;
                consumed := !consumed + n)
            m.Files.Gem.gigar
        (* WHAT IS KEPT OF A PLACEMENT, laid end to end in a store: the contig, where it starts,
           how many positions it covers and the record's own length, four bytes each; one call
           per position, two bytes, the base or the gap or the skip with the strand, and the
           quality; then each indel as its offset from the start, its strand, its symbol's length
           and the symbol.  Some 20 bytes and 2 per position, against the tag, the sequence and
           the qualities of the record it came from *)
        let header_length = 16
        type store_t = {
          mutable bytes: Bytes.t;
          mutable length: int;
          mutable index: int array;
          mutable count: int
        }
        let get_int32 bytes at = Bytes.get_int32_le bytes at |> Int32.to_int
        let set_int32 bytes at n = Bytes.set_int32_le bytes at (Int32.of_int n)
        (* Room for [n] more bytes, doubling the store unless a budget says how far *)
        let reserve ?budget store n =
          if store.length + n > Bytes.length store.bytes then begin
            let target = max (store.length + n) (2 * Bytes.length store.bytes) in
            let target =
              match budget with
              | Some b -> max (store.length + n) (min target b)
              | None -> target in
            let bigger = Bytes.create target in
            Bytes.blit store.bytes 0 bigger 0 store.length;
            store.bytes <- bigger
          end
        let key bytes at = get_int32 bytes at, get_int32 bytes (at + 4)
        (* The records of the store in the reference's order *)
        let sorted store =
          let index = Array.sub store.index 0 store.count in
          Array.sort (fun a b -> compare (key store.bytes a) (key store.bytes b)) index;
          index
        (* A CELL is what the reads have said at one position, counted as summarize counts a
           line: the bases, each with its qualities, the gaps, the skips and the indels.  It
           lives from the first placement reaching its position to the last, then becomes the
           position's summary *)
        type cell_t = {
          mutable counts: int array;
          mutable quals: Qualities.t option array;
          mutable gaps: int;
          mutable skips: int;
          mutable indels: (string * int) list
        }
        let empty_cell () =
          { counts = Array.make 5 0; quals = Array.make 5 None; gaps = 0; skips = 0; indels = [] }
        let summary_of name sequence pos cell =
          let voting = Array.fold_left ( + ) 0 cell.counts in
          { Summary.seq = name; pos; reference = sequence.[pos - 1];
            depth = voting + cell.gaps + cell.skips; voting; gaps = cell.gaps; skips = cell.skips;
            genotypes = genotypes_of cell.counts cell.quals (List.sort compare cell.indels) }
        (* The live cells, in a ring from the earliest position some placement still covers;
           every slot beyond the live ones holds a cell nothing has touched *)
        type ring_t = {
          mutable cells: cell_t array;
          mutable head: int;
          mutable live: int
        }
        let ring_get ring k =
          let size = Array.length ring.cells in
          if k >= size then begin
            let bigger = Array.init (max (k + 1) (2 * size)) (fun _ -> empty_cell ()) in
            for i = 0 to ring.live - 1 do
              bigger.(i) <- ring.cells.((ring.head + i) mod size)
            done;
            ring.cells <- bigger;
            ring.head <- 0
          end;
          if k >= ring.live then
            ring.live <- k + 1;
          ring.cells.((ring.head + k) mod Array.length ring.cells)
        let ring_pop ring =
          if ring.live = 0 then
            empty_cell ()
          else begin
            let cell = ring.cells.(ring.head) in
            ring.cells.(ring.head) <- empty_cell ();
            ring.head <- (ring.head + 1) mod Array.length ring.cells;
            ring.live <- ring.live - 1;
            cell
          end
        let iter ?(qualities = false) ?strata ?(quality_offset = 33) ?missing_quality ?(path = "-")
            ?strand ?memory ?(verbose = false) ~reference f ic =
          let malformed message = Exception.raise __FUNCTION__ IO_Format message in
          (* The reference as the reads were mapped to it *)
          let contigs =
            Array.map (fun (name, sequence) -> name, String.uppercase_ascii sequence) reference in
          let index = Hashtbl.create (Array.length contigs) in
          Array.iteri (fun i (name, _) -> Hashtbl.replace index name i) contigs;
          let wanted s =
            match strand with
            | None -> true
            | Some (Sequences.Types.Forward _) -> s = 0
            | Some (Sequences.Types.Reverse _) -> s = 1 in
          (* THE INPUT, PLACEMENT BY PLACEMENT, INTO THE STORE -- and out of it into runs when a
             budget says the store is full *)
          let store =
            { bytes = Bytes.create 65536; length = 0; index = Array.make 1024 0; count = 0 }
          and runs = ref [] and placements = ref 0 in
          let spill () =
            let path = Filename.temp_file "BiOCamLib_Mpileup_Gem_" ".run" in
            List.accum runs path;
            let oc = open_out_bin path in
            Array.iter (fun at -> output oc store.bytes at (get_int32 store.bytes (at + 12)))
              (sorted store);
            close_out oc;
            store.length <- 0;
            store.count <- 0 in
          let keep read m =
            let i =
              match Hashtbl.find_opt index m.Files.Gem.contig with
              | Some i -> i
              | None ->
                Printf.sprintf "read '%s' is placed on '%s', which the reference does not contain"
                  read.Files.Gem.tag m.Files.Gem.contig
                |> malformed in
            let first, span = extent m in
            let at = store.length in
            reserve ?budget:memory store (header_length + 2 * span);
            set_int32 store.bytes at i;
            set_int32 store.bytes (at + 4) first;
            set_int32 store.bytes (at + 8) span;
            store.length <- at + header_length + 2 * span;
            walk ~where:__FUNCTION__ ~quality_offset ~missing_quality
              ~record:(fun pos code s quality ->
                let k = at + header_length + 2 * (pos - first) in
                Bytes.unsafe_set store.bytes k (Char.unsafe_chr (code lor (s lsl 3)));
                Bytes.unsafe_set store.bytes (k + 1) (Char.unsafe_chr quality))
              ~note_indel:(fun pos symbol s ->
                let n = String.length symbol in
                reserve ?budget:memory store (9 + n);
                set_int32 store.bytes store.length (pos - first);
                Bytes.set_int8 store.bytes (store.length + 4) s;
                set_int32 store.bytes (store.length + 5) n;
                Bytes.blit_string symbol 0 store.bytes (store.length + 9) n;
                store.length <- store.length + 9 + n)
              contigs.(i) read m;
            set_int32 store.bytes (at + 12) (store.length - at);
            if store.count = Array.length store.index then begin
              let bigger = Array.make (2 * store.count) 0 in
              Array.blit store.index 0 bigger 0 store.count;
              store.index <- bigger
            end;
            store.index.(store.count) <- at;
            store.count <- store.count + 1;
            incr placements;
            match memory with
            | Some budget when store.length + 8 * store.count >= budget -> spill ()
            | _ -> () in
          (* THE CELLS, FED IN THE REFERENCE'S ORDER; every position before the placement at hand
             is over, and is delivered on the way to it *)
          let ring = { cells = Array.init 256 (fun _ -> empty_cell ()); head = 0; live = 0 }
          and contig = ref (-1) and base = ref 1 in
          let deliver i pos =
            let name, sequence = contigs.(i) in
            f (summary_of name sequence pos (ring_pop ring)) in
          let reach i pos =
            while !contig < i do
              if !contig >= 0 then
                for p = !base to String.length (snd contigs.(!contig)) do
                  deliver !contig p
                done;
              incr contig;
              base := 1
            done;
            while !base < pos do
              deliver i !base;
              incr base
            done in
          let apply bytes at =
            let i = get_int32 bytes at and first = get_int32 bytes (at + 4)
            and span = get_int32 bytes (at + 8) and length = get_int32 bytes (at + 12) in
            reach i first;
            for k = 0 to span - 1 do
              let b = Char.code (Bytes.unsafe_get bytes (at + header_length + 2 * k)) in
              let code = b land 7 and s = b lsr 3 in
              if wanted s then begin
                let cell = ring_get ring k in
                if code = gap_code then
                  cell.gaps <- cell.gaps + 1
                else if code = skip_code then
                  cell.skips <- cell.skips + 1
                else begin
                  cell.counts.(code) <- cell.counts.(code) + 1;
                  let quality =
                    Char.code (Bytes.unsafe_get bytes (at + header_length + 2 * k + 1)) in
                  match cell.quals.(code) with
                  | Some q -> Qualities.add q quality
                  | None ->
                    let q = Qualities.make () in
                    Qualities.add q quality;
                    cell.quals.(code) <- Some q
                end
              end
            done;
            let p = ref (at + header_length + 2 * span) in
            while !p < at + length do
              let offset = get_int32 bytes !p and s = Bytes.get_int8 bytes (!p + 4)
              and n = get_int32 bytes (!p + 5) in
              if wanted s then begin
                let cell = ring_get ring offset and symbol = Bytes.sub_string bytes (!p + 9) n in
                cell.indels <-
                  (match List.assoc_opt symbol cell.indels with
                   | Some m -> (symbol, m + 1) :: List.remove_assoc symbol cell.indels
                   | None -> (symbol, 1) :: cell.indels)
              end;
              p := !p + 9 + n
            done in
          Fun.protect
            ~finally:(fun () -> List.iter (fun path -> try Sys.remove path with _ -> ()) !runs)
            (fun () ->
              Files.Gem.iter ~qualities ?strata ~path (fun read ~placements:_ m -> keep read m) ic;
              if !runs = [] then begin
                if verbose then
                  Printf.eprintf "(%s): Sorting %d %s in memory\n%!" __FUNCTION__ !placements
                    (String.pluralize_int "placement" !placements);
                Array.iter (apply store.bytes) (sorted store)
              end else begin
                (* What is left joins the runs, and the runs are merged: the earliest head among
                   them goes next, each run reading on as its head goes *)
                if store.count > 0 then
                  spill ();
                if verbose then
                  Printf.eprintf "(%s): Merging %d %s from %d %s within a budget of %d bytes\n%!"
                    __FUNCTION__ !placements (String.pluralize_int "placement" !placements)
                    (List.length !runs) (String.pluralize_int "run" (List.length !runs))
                    (Option.get memory);
                let runs =
                  List.map (fun path -> open_in_bin path, ref (Bytes.create 65536), ref true) !runs
                    |> Array.of_list in
                let advance (ic, buf, alive) =
                  match really_input ic !buf 0 header_length with
                  | () ->
                    let length = get_int32 !buf 12 in
                    if Bytes.length !buf < length then begin
                      let bigger = Bytes.create (max length (2 * Bytes.length !buf)) in
                      Bytes.blit !buf 0 bigger 0 header_length;
                      buf := bigger
                    end;
                    really_input ic !buf header_length (length - header_length)
                  | exception End_of_file -> alive := false in
                Fun.protect ~finally:(fun () -> Array.iter (fun (ic, _, _) -> close_in ic) runs)
                  (fun () ->
                    Array.iter advance runs;
                    let rec merge () =
                      let next = ref (-1) in
                      Array.iteri
                        (fun j (_, buf, alive) ->
                          if !alive then
                            if !next < 0 then
                              next := j
                            else begin
                              let (_, b, _) = runs.(!next) in
                              if compare (key !buf 0) (key !b 0) < 0 then
                                next := j
                            end)
                        runs;
                      if !next >= 0 then begin
                        let run = runs.(!next) in
                        let (_, buf, _) = run in
                        apply !buf 0;
                        advance run;
                        merge ()
                      end in
                    merge ())
              end;
              (* Whatever the reads did not reach, to the reference's end *)
              reach (Array.length contigs) 1)
      end
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
          mutable quality: int;
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
    module Qualities:
      sig
        type t
        val make: unit -> t
        val add: t -> int -> unit
        val cardinal: t -> int
        (* Over the qualities that are present, lowest first, each with the
           number of times it was seen.  Empty buckets are skipped *)
        val iter: (int -> int -> unit) -> t -> unit
        (* [into] gains what the other holds: the null distribution of a variant
           is every other genotype taken together *)
        val merge_into: into:t -> t -> unit
        val mean: t -> float
        val variance: t -> float
        (* The mean of what is left once the lowest [fraction] of the
           observations has been dropped *)
        val mean_above_fraction: t -> float -> float
      end
    module Genotype:
      sig
        type kind_t =
          | Base
          | Short_indel
          | Long_indel
        type t = {
          symbol: string;
          kind: kind_t;
          count: int;
          qualities: Qualities.t option
        }
      end
    module Summary:
      sig
        type t = {
          seq: string;
          pos: int;
          reference: char;
          depth: int;
          voting: int;
          gaps: int;
          skips: int;
          genotypes: Genotype.t list
        }
        (* One line per position, the genotypes sorted by symbol, each as
           symbol:kind:count and, for a base, its qualities as quality=times
           pairs -- a form two pileups can be compared in whatever wrote them *)
        val to_string: t -> string
      end
    val empty: t
    val of_line: ?quality_offset:int -> ?line_number:int -> string -> t
    (* The same line counted rather than kept, which is what a variant caller
       wants and is reached without building a read.  [strand] keeps only the
       reads on it *)
    val summarize:
      ?quality_offset:int -> ?strand:Sequences.Types.strand_t -> ?line_number:int ->
      string -> Summary.t
    val to_string: ?quality_offset:int -> t -> string
    (* Over every line of a string, and of a file, the line number going into
       whatever goes wrong *)
    val iter_string: ?quality_offset:int -> (t -> unit) -> string -> unit
    val iter: ?quality_offset:int -> (t -> unit) -> string -> unit
    (* The summaries of every position of the reference, in order, from the GEM
       mapper's own output (gem3-mapper -F MAP) rather than from a pileup: each
       placement of each read is walked against the reference and counted as
       summarize counts a line.  [reference] is the sequences the reads were
       mapped to, by name; [qualities] and [strata] are the reader's, the
       records carrying qualities when the mapper was fed FASTQ, and the first
       that many non-empty strata of each read being counted -- every placement
       the reader delivers votes, whichever copy of a repeat it is on;
       [missing_quality] stands in for records without qualities, which are
       refused otherwise; [strand] keeps only the reads on it, gaps included,
       which a pileup could not tell apart.  What the reads say is checked
       against the reference, so a reference other than the one they were mapped
       to is refused.
       Without [memory] everything the reads say is kept until the input is
       over, at two bytes a call and a cell a position; with it, in bytes, the
       calls are first counted in a pass that keeps one count per position, the
       reference is cut into windows whose cells fit the budget, and each window
       is filled by a pass of its own, so the input, read once more than there
       are windows, must then be a file, named by [path]; [verbose] says how
       many passes it took *)
    module Gem:
      sig
        val iter:
          ?qualities:bool -> ?strata:int -> ?quality_offset:int -> ?missing_quality:int ->
          ?path:string -> ?strand:Sequences.Types.strand_t -> ?memory:int -> ?verbose:bool ->
          reference:(string * string) array -> (Summary.t -> unit) -> in_channel -> unit
      end
  end
)
