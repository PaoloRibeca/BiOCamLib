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
      end
    let base_index = function
      | 'A' -> 0 | 'C' -> 1 | 'G' -> 2 | 'T' -> 3 | 'N' -> 4 | _ -> -1
    let base_of_index = [| 'A'; 'C'; 'G'; 'T'; 'N' |]
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
      let genotypes =
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
          !indels;
        for i = 4 downto 0 do
          if counts.(i) <> 0 then
            List.accum acc
              { Genotype.symbol = String.make 1 base_of_index.(i);
                kind = Genotype.Base;
                count = counts.(i);
                qualities = Some quals.(i) }
        done;
        !acc in
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
  end
)
