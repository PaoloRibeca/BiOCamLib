(*
    Annotations_Common.ml -- (c) 2026 Paolo Ribeca, <paolo.ribeca@gmail.com>

    This file is part of BiOCamLib, the OCaml foundations upon which
    a number of the bioinformatics tools I developed are built.

    Annotations_Common.ml holds what every per-format module needs and
    no single one owns: file and buffer plumbing, the FASTA renderer
    and its width setting, the [Hierarchy] S-expression parser, the
    field codecs that turn a column into a strand, a phase, a score or
    an interval, the [GenBankLocation] AST with its LOCATION-string
    parser, the TSV line iterator, and the [Writer_t] / [Format_t]
    interfaces the format modules satisfy.

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
(* Included rather than opened, so that this module is the base plus the
   scaffolding rather than a separate thing sitting beside it.  Everything
   downstream -- the five format modules, and [Annotations] through them --
   then reaches the base through here, and no hand-kept list of re-exports can
   drift out of step with what the base actually holds.  The extended
   [Hierarchy] below shadows the base's, which is the intended reading.
   Constrained for the same reason as every other include here: it copies the
   base's signature rather than aliasing into it, which is what allows the base
   to be private.  See lib/dune. *)
include (Annotations_Base: module type of Annotations_Base)

(* Read an entire file into memory.  All format readers below
   are string-based (they keep the whole input around for
   topological sorting anyway), so the file-vs-string distinction
   is only an I/O wrapper. *)
let read_file path =
  let ic = open_in path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s
(* Build a [to_string] from a [to_buffer]: the canonical pattern
   shared across all format writers. *)
let to_string_via_buffer to_buffer ann =
  let buf = Buffer.create 256 in
  to_buffer buf ann;
  Buffer.contents buf
(* Render a reference as FASTA.  Shared by the tabular writer, which keeps the
   sequence in a sidecar, and by the GFF3 writer, whose [##FASTA] directive
   appends it to the annotation.  The two want different defaults, so the width
   is per-caller rather than a constant: GFF3 is read by third-party tools that
   expect the conventional wrap, whereas the tabular document is one record per
   line throughout and a wrapped tail would be the only part of it that [awk],
   [cut] and [sort] could not take a line at a time.  A width of zero means
   never wrap. *)
let default_fasta_width = 60
(* Set by a front-end that wants one width everywhere -- AnnoTools'
   [--fasta-width].  [None] leaves each writer its own default. *)
let fasta_width_override = ref None
let set_fasta_width w =
  (match w with
   | Some w when w < 0 ->
     Exception.raise __FUNCTION__ Algorithm
       (Printf.sprintf "Invalid FASTA width %d (expected zero or more)" w)
   | Some _ | None -> ());
  fasta_width_override := w
(* The sequence half of a FASTA record, returned without a trailing newline so
   that a caller can lay the record out itself.  A width of zero, whether the
   caller's or the override's, returns the sequence whole. *)
let wrap_sequence ?(width = default_fasta_width) seq =
  let width = match !fasta_width_override with Some w -> w | None -> width in
  let n = String.length seq in
  if width <= 0 || n <= width then
    seq
  else begin
    let buf = Buffer.create (n + n / width + 1) in
    let i = ref 0 in
    while !i < n do
      if !i > 0 then
        Buffer.add_char buf '\n';
      let w = min width (n - !i) in
      Buffer.add_string buf (String.sub seq !i w);
      i := !i + w
    done;
    Buffer.contents buf
  end
let write_fasta ?(width = default_fasta_width) buf reference =
  Sequences.Reference.iter (fun ~name ~seq ~table:_ ~description ->
    (* The header goes back out as it came in, name and description both: a
       reference read from an ordinary FASTA and written into a [##FASTA]
       section or a tabular sidecar should still say what it said. *)
    Printf.bprintf buf ">%s%s\n" name
      (if description = "" then "" else " " ^ description);
    (* An empty sequence gets a header and nothing else, rather than a blank
       line that would read back as a sequence of length zero. *)
    if seq <> "" then
      Printf.bprintf buf "%s\n" (wrap_sequence ~width seq)) reference

(* Build a [to_file] from a [to_buffer]: ditto. *)
let to_file_via_buffer to_buffer ann path =
  let oc = open_out path in
  let buf = Buffer.create 256 in
  to_buffer buf ann;
  Buffer.output_buffer oc buf;
  close_out oc

(* Hierarchy: the [Annotations_Base] base module plus the
   S-expression parser. *)
module Hierarchy:
  sig
    include module type of Annotations_Base.Hierarchy
    val of_string: string -> t
    val of_file: string -> t
  end
= struct
    include Annotations_Base.Hierarchy
    let of_string s =
      let lexbuf = Lexing.from_string ~with_positions:true s in
      Annotations_Parse.hierarchy Annotations_Lex.hierarchy lexbuf
    let of_file path = of_string (read_file path)
  end

(* The broad GFF3 feature vocabulary.  It lives here rather than in the GFF3
   module because the tabular format borrows it: a tabular document declares
   the hierarchy it was written under, and this is only the fallback for one
   that does not.  Keeping it common is what lets the two formats be read
   independently of each other. *)
let default_gff3_hierarchy =
  Hierarchy.of_string
    "(gene \
        ((mRNA (exon, CDS, five_prime_UTR, three_prime_UTR, \
                start_codon, stop_codon, intron)), \
         (transcript (exon, CDS, five_prime_UTR, three_prime_UTR, \
                      start_codon, stop_codon, intron, \
                      Selenocysteine)), \
         (lncRNA (exon, intron)), \
         (miRNA (exon)), \
         (rRNA (exon)), \
         (tRNA (exon)), \
         (snoRNA (exon)), \
         (snRNA (exon)), \
         (ncRNA (exon)))), \
     pseudogene, region"

let strand_of_field = function
  | "+" -> Some Sequences.Types.forward
  | "-" -> Some Sequences.Types.reverse
  | "." | "?" | "" -> None
  | s ->
    Exception.raise __FUNCTION__ IO_Format
      (Printf.sprintf "Invalid strand %S" s)

let phase_of_field = function
  | "." | "" -> None
  | "0" -> Some 0
  | "1" -> Some 1
  | "2" -> Some 2
  | s ->
    Exception.raise __FUNCTION__ IO_Format
      (Printf.sprintf "Invalid phase %S" s)

(* Column 6 of GFF3 and GTF.  [.] is "no score", which is not the same as a
   score of zero, hence the option. *)
let score_of_field = function
  | "." | "" -> None
  | s ->
    (match float_of_string_opt s with
     | Some f -> Some f
     | None ->
       Exception.raise __FUNCTION__ IO_Format
         (Printf.sprintf "Invalid score %S" s))
(* Written back with %.12g, which is enough to carry every float a reader is
   likely to have produced without printing 17 digits for a round number.  That
   is a readability trade for GFF3 and GTF column 6; a format that claims to
   hold everything the binary archive holds needs [field_of_score_exact]
   below instead. *)
let field_of_score = function
  | None -> "."
  | Some f -> Printf.sprintf "%.12g" f

(* %.17g is the shortest precision that recovers every double exactly.  A score
   that arrived from an archive, or from a column 6 carrying more than twelve
   significant digits, would otherwise be silently rounded on its first crossing
   into a table that is supposed to be lossless -- and the loss is invisible to
   any test that only re-reads what the tabular writer produced, since %.12g of
   an already-rounded value is a fixed point. *)
let field_of_score_exact = function
  | None -> "."
  | Some f -> Printf.sprintf "%.17g" f

(* An interval as a text format writes it.  Everything in the AST is 0-based
   half-open and everything on the wire is 1-based, so this is the whole of the
   boundary -- and it is worth having in one place because the two conventions
   the formats use are not the same, and the difference is easy to get wrong
   silently.
   A run of bases is [lo..hi], both 1-based and inclusive.  A zero-length site
   -- the position between two consecutive bases -- has no bases to number, and
   the formats split on how to say so.  GenBank and the tabular format spell it
   [n^m] with [m = n + 1], where [n] is the 0-based coordinate; GFF3, GTF and
   the feature table have only a 1-based inclusive pair and so spell it as the
   inverted pair [lo..lo-1].  Both come back to the same interval, but through
   different arithmetic, and neither is derivable from the other by eye. *)
module OneBased:
  sig
    type t =
      | Range of int * int
      | Between of int * int
    (* The [lo..hi] / [n^m] spelling, as GenBank, the tabular format and the
       extraction actions write it. *)
    val of_interval: Sequences.Types.simple_interval_t -> t
    val to_interval: t -> Sequences.Types.simple_interval_t
    val to_string: t -> string
    val of_string: string -> t
    (* The plain 1-based inclusive pair, as GFF3, GTF and the feature table
       write it, where a zero-length interval comes out inverted. *)
    val bounds: Sequences.Types.simple_interval_t -> int * int
    val interval_of_bounds: lo:int -> hi:int -> Sequences.Types.simple_interval_t
  end
= struct
    type t =
      | Range of int * int
      | Between of int * int
    let bounds (i: Sequences.Types.simple_interval_t) = i.low + 1, i.low + i.length
    let of_interval (i: Sequences.Types.simple_interval_t) =
      if i.length = 0 then
        (* [low] is quoted as it stands, where [Range] shifts by one: for a
           zero-length interval [low] is the position after 1-based base [low],
           so the two readings coincide and no adjustment is wanted. *)
        Between (i.low, i.low + 1)
      else
        let lo, hi = bounds i in
        Range (lo, hi)
    let interval_of_bounds ~lo ~hi : Sequences.Types.simple_interval_t =
      (* Positions are 1-based, so anything below 1 is not a coordinate.  Left
         unchecked, a [lo] of 0 yields [low = -1], which every writer then
         re-emits happily -- GFF3 [0 500], GenBank [0..500] -- and which only
         surfaces much later, and as an internal error rather than a diagnosis,
         when the reference is finally indexed. *)
      if lo < 1 then
        Exception.raise __FUNCTION__ IO_Format
          (Printf.sprintf "Invalid 1-based coordinate %d (positions start at 1)" lo);
      (* [hi = lo - 1] is the one legal inversion, and is what [bounds] produces
         for a zero-length interval.  It denotes the 0-based half-open interval
         [lo - 1, lo - 1), which is exactly what [GenBankLocation.intervals]
         stores for [Between]. *)
      if hi = lo - 1 then { low = lo - 1; length = 0 }
      else begin
        if hi < lo then
          Exception.raise __FUNCTION__ IO_Format
            (Printf.sprintf "Invalid interval (lo=%d, hi=%d)" lo hi);
        { low = lo - 1; length = hi - lo + 1 }
      end
    let to_interval = function
      | Range (lo, hi) ->
        if hi < lo then
          Exception.raise __FUNCTION__ IO_Format
            (Printf.sprintf "Invalid interval (lo=%d, hi=%d)" lo hi);
        interval_of_bounds ~lo ~hi
      | Between (lo, hi) ->
        (* Only consecutive positions denote a between-bases site.  Accepting
           any [hi] meant [100^999] parsed happily and was then re-emitted as
           [100^101], so a hand-edited file was silently rewritten rather than
           diagnosed -- and hand editing is what the tabular format is for. *)
        if hi <> lo + 1 then
          Exception.raise __FUNCTION__ IO_Format
            (Printf.sprintf "Invalid between-bases site %d^%d (positions must be consecutive)"
               lo hi);
        if lo < 1 then
          Exception.raise __FUNCTION__ IO_Format
            (Printf.sprintf "Invalid 1-based coordinate %d (positions start at 1)" lo);
        { low = lo; length = 0 }
    let to_string = function
      | Range (lo, hi) -> Printf.sprintf "%d..%d" lo hi
      | Between (lo, hi) -> Printf.sprintf "%d^%d" lo hi
    let of_string s =
      let two sep =
        match String.Split.as_list (Str.regexp_string sep) s with
        | [ a; b ] ->
          (match int_of_string_opt a, int_of_string_opt b with
           | Some a, Some b -> Some (a, b)
           | _ -> None)
        | _ -> None in
      match two ".." with
      | Some (lo, hi) -> Range (lo, hi)
      | None ->
        match two "^" with
        | Some (lo, hi) -> Between (lo, hi)
        | None ->
          Exception.raise __FUNCTION__ IO_Format
            (Printf.sprintf "Malformed interval %S (expected lo..hi or lo^hi)" s)
  end

(* Retained under its old name because the readers spell the GFF3/GTF
   convention this way throughout; it is [OneBased.interval_of_bounds]. *)
let interval_of_1_based = OneBased.interval_of_bounds

(* Extended [GenBankLocation]: the base AST module from
   [Annotations_Base] plus the LOCATION-string parser
   ([of_string]) and the resolver from a parsed location to a
   list of half-open intervals tagged by optional remote
   accession, paired with the overall strand inferred from the
   outermost [Complement] node ([intervals]). *)
module GenBankLocation:
  sig
    include module type of Annotations_Base.GenBankLocation
    val of_string: string -> t
    (* Each piece carries its own partiality and strand, because a join can
       disagree with itself on both; the second component is the strand of the
       feature as a whole, and is [None] when the pieces do not agree -- which
       is legal INSDC and is how trans-splicing is spelled. *)
    val intervals:
      t ->
      (string option * Segment.t) list * Sequences.Types.strand_t option
  end
= struct
    include Annotations_Base.GenBankLocation
    let of_string s =
      let lexbuf = Lexing.from_string ~with_positions:true s in
      Annotations_Parse.genbank_location
        Annotations_Lex.genbank_location lexbuf
    let intervals loc =
      let mk_simple low length : Sequences.Types.simple_interval_t =
        { low; length } in
      (* [<] and [>] say the feature runs past what was sequenced, and they
         belong to the piece that carries them: only the first piece of a join
         knows the feature began before the record does.  They were parsed and
         then dropped here, which is what made a partial CDS indistinguishable
         from a complete one -- and a 5'-partial CDS has its first codon
         rewritten to methionine on that belief. *)
      let seg ?(partial_low = false) ?(partial_high = false) strand span =
        Segment.make ~partial_low ~partial_high ?strand span in
      let rec walk strand seq = function
        | Point e ->
          (* Through [interval_of_1_based] rather than [mk_simple], so that a
             point location of 0 is caught by the same check as a range. *)
          [ seq, seg ~partial_low:e.fuzzy_left ~partial_high:e.fuzzy_right
                   strand (interval_of_1_based ~lo:e.pos ~hi:e.pos) ], strand
        | Range (a, b) ->
          (* GenBank spells a between-bases site [lo^hi], so it has no use for
             the inverted pair [interval_of_1_based] tolerates on behalf of the
             formats that cannot -- here [200..199] is simply malformed, and
             letting the shared helper read it as a zero-length feature would
             accept a broken record in silence. *)
          if b.pos < a.pos then
            Exception.raise __FUNCTION__ IO_Format
              (Printf.sprintf "Invalid range (%d..%d): a between-bases site is spelled %d^%d"
                 a.pos b.pos b.pos a.pos);
          [ seq, seg ~partial_low:a.fuzzy_left ~partial_high:b.fuzzy_right
                   strand (interval_of_1_based ~lo:a.pos ~hi:b.pos) ], strand
        | Between (a, _) ->
          (* Zero-length feature between [a] and [a+1]. *)
          [ seq, seg strand (mk_simple a 0) ], strand
        | Complement inner ->
          let flipped =
            match strand with
            | None -> Some Sequences.Types.reverse
            | Some Sequences.Types.Forward _ -> Some Sequences.Types.reverse
            | Some Sequences.Types.Reverse _ -> Some Sequences.Types.forward in
          walk flipped seq inner
        | Join parts
        | Order parts ->
          (* Every part is resolved under the strand in force at this level, and
             the parts no longer have to agree: each piece carries its own, so a
             mixed-strand join -- legal INSDC, and how trans-splicing is spelled
             -- is represented rather than refused.  What the whole feature gets
             is [None] in that case, which is the honest answer to "which strand
             is this on".
             This used to raise, which was right while a feature carried one
             strand for all of its parts; before THAT it kept whichever came
             last, silently reverse-complementing the parts that disagreed. *)
          let acc = ref [] and st = ref strand and seen = ref false
          and mixed = ref false in
          List.iter (fun p ->
            let pieces, s = walk strand seq p in
            if !seen && s <> !st then
              mixed := true;
            st := s;
            seen := true;
            acc := !acc @ pieces) parts;
          let st = if !mixed then ref None else st in
          (* INSDC 3.4.3 gives complement(join(A,B)) and join(complement(B),
             complement(A)) as two spellings of ONE feature, but they arrive
             here in opposite orders: the first stores [A; B], the second
             [B; A].  Everything downstream assumes the first -- [feature_dna]
             concatenates and reverse-complements once, the feature-table writer
             reverses the stored list -- so a distributed complement has to be
             put back into that order, or the two spellings yield different
             proteins and the exons come out of transcription order.
             The test is the parts' strand against the strand already in force:
             they differ exactly when the complement was distributed over the
             parts rather than wrapped around them. *)
          if (not !mixed) && !seen && !st <> strand then
            List.rev !acc, !st
          else
            !acc, !st
        | Remote (acc_name, _, inner) ->
          walk strand (Some acc_name) inner in
      let pieces, overall = walk None None loc in
      (* A piece's strand is [None] when it agrees with the feature's, which is
         what [None] is documented to mean and what keeps the ordinary case
         uncluttered.  The walk cannot know that as it goes -- it sees only the
         strand in force at each level -- so it marks every piece and the ones
         that agree are cleared here.  What is left is exactly the pieces a
         reader has to act on. *)
      List.map
        (fun (seq, (s: Segment.t)) ->
          seq, if s.strand = overall then { s with strand = None } else s)
        pieces,
      overall
  end

(* Iterate over the lines of a TSV-style format (GFF3 / GTF):
   * blank lines are skipped silently;
   * ["##"] directives go to [pragma] (with the body after
     the hashes), if supplied;
   * ["#"] comment lines are skipped silently;
   * everything else is a data row -- [data] is called with the
     1-based line number and the array of tab-separated fields. *)
let iter_tsv_lines ?(pragma = fun _ -> ()) ?(fasta = fun _ -> ()) ~data s =
  let lines = String.split_on_char '\n' s |> Array.of_list in
  let n = Array.length lines in
  let strip raw =
    let m = String.length raw in
    if m > 0 && raw.[m - 1] = '\r' then String.sub raw 0 (m - 1) else raw in
  (* [##FASTA] is a standard GFF3 directive: it ends the annotation and says
     the rest of the file is sequence.  So the walk stops there and hands the
     remainder over whole, rather than trying to read FASTA as rows. *)
  let i = ref 0 and sequence_from = ref (-1) in
  while !i < n && !sequence_from < 0 do
    let line = strip lines.(!i) in
    let lnum = !i + 1 in
    if line = "" then ()
    else if String.length line >= 2 && String.sub line 0 2 = "##" then begin
      let body = String.sub line 2 (String.length line - 2) in
      if String.trim body = "FASTA" then sequence_from := !i + 1 else pragma body
    end
    else if line.[0] = '#' then ()
    else data lnum (String.split_on_char '\t' line |> Array.of_list);
    incr i
  done;
  if !sequence_from >= 0 then
    fasta
      (Array.sub lines !sequence_from (n - !sequence_from)
       |> Array.to_list |> List.map strip |> String.concat "\n")

(* Add features to an annotation in DFS order, dropping the
   [ValueTable] Bloom filter every time the sequence column
   changes between adjacent features.  Used by both GFF3 and
   GTF, which group their input by sequence. *)
let add_dfs_with_seq_bloom ann_ref features =
  let prev_seq = ref None in
  List.iter (fun (path, feature) ->
    let s = feature.Annotation.seq in
    (match !prev_seq with
     | Some p when not (Seq.equal p s) ->
       ValueTable.drop_bloom (Annotation.values !ann_ref)
     | _ -> ());
    prev_seq := Some s;
    ann_ref := Annotation.add !ann_ref ~path feature
  ) features

(* Walk every attribute pair on [feature], resolving each value array back to
   its [string list] form.  Format-specific writers thread the result through
   their own per-pair formatters.
   The order is NOT the order the keys appeared in the source, whatever an
   earlier version of this comment claimed.  [Attributes] is keyed by the integer
   [AttrKey] id, and ids are handed out on first intern across the whole
   annotation, so what comes out is global first-intern order -- which, because
   the GenBank reader folds a [StringMap] to build each feature's qualifiers, is
   near enough alphabetical.  Preserving true per-feature source order would
   take a list rather than a map.  A format that needs a stable, predictable
   order must therefore sort explicitly rather than rely on this. *)
let attribute_pairs ann feature =
  let pairs = ref [] in
  Annotation.attr_iter ann (fun k vs ->
    List.accum pairs (k, vs)) feature;
  List.rev !pairs

(* Common interface implemented by every per-format module
   (GFF3, GTF, GenBank).  [dialects] is a non-empty association
   list of named hierarchies; the head pair is the format's
   default and is also exposed directly as
   [default_hierarchy].  [read] / [read_from_file] install the
   features encoded in their string / file argument into the
   supplied carrier annotation, using the carrier's
   hierarchy for validation; the returned annotation may also
   carry an updated reference (in GenBank's case, populated
   from any ORIGIN block) replacing whatever was on the
   carrier.  [of_string] / [of_file] are convenience
   constructors equivalent to [read] over a fresh annotation
   seeded with the requested hierarchy.  The canonical short
   name of each format lives on [Format.to_string] / its
   inverse [Format.of_string], not in the signature itself. *)
(* The write half of a format, split out so that a format which can only be
   written is expressible.  NCBI's submission feature table is one: it has no
   slot for a source column, no parent link and no annotation metadata, and
   table2asn INFERS the gene/mRNA/CDS relations from coordinate overlap rather
   than reading them, so an annotation cannot be recovered from one.  Without this
   split such a format would have to satisfy [Format_t] by stubbing four
   functions that raise, and its constructor would make [--from-tbl]
   expressible but broken. *)
module type Writer_t = sig
  val to_buffer: Buffer.t -> Annotation.t -> unit
  val to_string: Annotation.t -> string
  val to_file: Annotation.t -> string -> unit
end

(* A format that round-trips: everything a writer has, plus the readers. *)
module type Format_t = sig
  include Writer_t
  val dialects: (string * Hierarchy.t) list
  val default_hierarchy: Hierarchy.t
  val read: Annotation.t -> string -> Annotation.t
  val read_from_file: Annotation.t -> string -> Annotation.t
  val of_string: ?hierarchy:Hierarchy.t -> string -> Annotation.t
  val of_file: ?hierarchy:Hierarchy.t -> string -> Annotation.t
end

