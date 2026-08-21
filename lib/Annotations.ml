(*
    Annotations.ml -- (c) 2026 Paolo Ribeca, <paolo.ribeca@gmail.com>

    This file is part of BiOCamLib, the OCaml foundations upon which
    a number of the bioinformatics tools I developed are built.

    Annotations.ml glues [Annotations_Base] (the format-independent
    AST), [Annotations_Lex] (lexer rules) and [Annotations_Parse]
    (Menhir grammars) into a public API.  Top-level [Hierarchy],
    [Annotation], and [GenBankLocation] modules each extend their
    [Annotations_Base] counterpart (the parsers, binary I/O,
    validation actions, and the LOCATION-string entry points
    live here, not in the base); per-format [GFF3], [GTF], and
    [GenBank] modules are siblings carrying their own default
    hierarchies and string/file readers and writers.  The [Path],
    [Seq], [AttrKey], [AttrMap], [Value], and [ValueTable]
    modules are re-exported verbatim from [Annotations_Base] for
    one-import access.

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
open Annotations_Base

(* Re-export base interning modules under [Annotations] so callers
   need only one import. *)
module Path = Annotations_Base.Path
module Seq = Annotations_Base.Seq
module AttrKey = Annotations_Base.AttrKey
module AttrMap = Annotations_Base.AttrMap
module Value = Annotations_Base.Value
module ValueTable = Annotations_Base.ValueTable

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
(* Render a reference as FASTA, wrapped at the width every FASTA is wrapped at.
   Shared by the tabular writer, which keeps the sequence in a sidecar, and by
   the GFF3 writer, whose [##FASTA] directive appends it to the annotation. *)
let fasta_width = 60
let write_fasta buf reference =
  Sequences.Reference.iter (fun ~name ~seq ~table:_ ->
    Printf.bprintf buf ">%s\n" name;
    let n = String.length seq in
    let i = ref 0 in
    while !i < n do
      let w = min fasta_width (n - !i) in
      Buffer.add_string buf (String.sub seq !i w);
      Buffer.add_char buf '\n';
      i := !i + w
    done) reference

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

(* Annotation: the AST module from [Annotations_Base], extended
   with binary I/O (mirroring [Trees.Splits.{to,of}_binary]: the
   archive is an OCaml [Marshal]-encoded value preceded by a
   version string; the default file suffix is [.Annotation]
   unless the prefix points under [/dev/*]) and validation
   actions.  Each [validate_*] raises on failure and requires a
   reference to be set on the annotation; calling without one
   raises. *)
module Annotation:
  sig
    include module type of Annotations_Base.Annotation
    val to_binary: ?verbose:bool -> t -> string -> unit
    val of_binary: ?verbose:bool -> string -> t
    val to_channel: out_channel -> t -> unit
    val of_channel: in_channel -> t
    (* Feature sequence.  [feature_dna] stitches a feature's
       intervals in the order they are stored, reading each one
       on the forward strand and reverse-complementing the
       whole result once when the feature is on the minus
       strand.  [feature_table] is the /transl_table qualifier
       when the feature carries one, and the reference's
       per-sequence default otherwise.  [feature_protein] drops
       the phase bases from the 5' end of [feature_dna] and
       translates that with [feature_table].  All three require
       a reference to be attached and raise when there is
       none. *)
    val feature_dna: t -> feature_t -> string
    val feature_table: t -> feature_t -> Sequences.Translation.t
    val feature_protein: t -> feature_t -> string
    (* Validation.  Each [validate_*] iterates over the
       annotation register; on every violation it calls the
       supplied [?on_violation] callback with the violating
       feature's path, the feature's id (or [""] if none was
       parsed), and a human-readable message.  The default
       callback raises [Validation_failed] with the same
       payload, which preserves the historical fail-fast
       behaviour for callers that have not opted in.  Passing
       a non-raising callback (e.g.\ one that writes to a
       file) makes the walk run through the whole register and
       collect every violation. *)
    exception Validation_failed of {
      path: string;
      feature_id: string;
      message: string
    }
    type on_violation_t =
      path:string -> feature_id:string -> message:string -> unit
    val validate_sequences_present:
      ?on_violation:on_violation_t -> t -> unit
    val validate_feature_bounds:
      ?on_violation:on_violation_t -> t -> unit
    val validate_translation:
      ?on_violation:on_violation_t -> t -> unit
  end
= struct
    include Annotations_Base.Annotation
    (* *)
    (* Bumped for two changes, neither of which an older archive survives:
       [feature_t] gained its [score] slot, so the record shape differs; and
       sibling lists are now stored most-recent-first, so an archive written in
       insertion order would read back with every level reversed -- which,
       being a shape the type still accepts, would corrupt silently rather than
       fail.  The previous released version is 2026-05-09; no archive carrying
       an intermediate value ever left this branch, so one bump covers both. *)
    let archive_version = "2026-08-21"
    let make_filename_binary = function
      | w when String.length w >= 5 && String.sub w 0 5 = "/dev/" -> w
      | prefix -> prefix ^ ".Annotation"
    let to_channel output ann =
      archive_version |> output_value output;
      output_value output ann
    let of_channel input =
      let version = (input_value input: string) in
      if version <> archive_version then
        Exception.raise_incompatible_archive_version
          __FUNCTION__ version archive_version;
      (input_value input: t)
    let to_binary ?(verbose = false) ann prefix =
      let path = make_filename_binary prefix in
      if verbose then
        Printf.eprintf "(%s): writing annotation to %s\n%!"
          __FUNCTION__ path;
      let oc = open_out path in
      to_channel oc ann;
      close_out oc
    let of_binary ?(verbose = false) prefix =
      let path = make_filename_binary prefix in
      if verbose then
        Printf.eprintf "(%s): reading annotation from %s\n%!"
          __FUNCTION__ path;
      let ic = open_in path in
      let res = of_channel ic in
      close_in ic;
      res
    (* *)
    (* [Initialize] rather than [Algorithm]: asking for a feature's sequence
       when no reference has been attached is an ordinary mistake by the caller,
       not a broken invariant inside the library.  [Exception.handle] prints the
       usage and a plain FATAL line for the first, but treats the second as a
       bug -- dumping a backtrace and inviting the user to report it, for
       something they can simply fix by loading a reference. *)
    let require_reference ann who =
      match reference ann with
      | Some r -> r
      | None ->
        Exception.raise __FUNCTION__ Initialize
          (Printf.sprintf
             "%s: no reference sequence is attached to this annotation -- load one before \
              asking for a feature's sequence" who)
    (* Read each interval on the FORWARD strand and complement the stitched
       result once at the end, rather than reading a minus-strand feature's
       intervals as Reverse.  [Sequences.Reference] keeps the reverse strand in
       its own coordinate frame, so a per-interval Reverse lookup would have to
       flip every position as well; doing it this way keeps the coordinate
       arithmetic in one frame throughout. *)
    let feature_dna ann feature =
      let r = require_reference ann "feature_dna" in
      let name = seq_name ann feature in
      let stitched =
        List.map (fun (i: Sequences.Types.simple_interval_t) ->
          let str_iv: Sequences.Types.stranded_interval_t = {
            low = { name = Sequences.Types.Forward name; position = i.low };
            length = i.length
          } in
          Sequences.Reference.get_sequence r str_iv) feature.intervals
        |> String.concat "" in
      match feature.strand with
      | Some (Sequences.Types.Reverse _) -> Sequences.Lint.rc stitched
      | _ -> stitched
    let feature_table ann feature =
      let r = require_reference ann "feature_table" in
      let name = seq_name ann feature in
      (* The reference records one table per sequence, keyed by a one-base
         interval at the origin; a feature-level /transl_table overrides it. *)
      let per_sequence () =
        try
          Sequences.Reference.get_table r {
            low = { name = Sequences.Types.Forward name; position = 0 };
            length = 1
          }
        with _ -> Sequences.Translation.Table_1 in
      match attr_get ann feature "transl_table" with
      | Some (n :: _) -> (try Sequences.Translation.of_string n with _ -> per_sequence ())
      | _ -> per_sequence ()
    let feature_protein ann feature =
      let dna = feature_dna ann feature in
      let phase = Option.value ~default:0 feature.phase in
      let coding =
        if phase >= String.length dna then ""
        else String.sub dna phase (String.length dna - phase) in
      Sequences.Translation.translate
        ~replace_alternative_start_codons_with_methionine:true ~stop_on_first_stop:true
        (feature_table ann feature) coding
    exception Validation_failed of {
      path: string;
      feature_id: string;
      message: string
    }
    type on_violation_t =
      path:string -> feature_id:string -> message:string -> unit
    let default_on_violation ~path ~feature_id ~message =
      raise (Validation_failed { path; feature_id; message })
    let feature_id_of feature =
      match feature.id with Some s -> s | None -> ""
    let validate_sequences_present
        ?(on_violation = default_on_violation) ann =
      let r = require_reference ann "validate_sequences_present" in
      iter (fun ~path feature ->
        let name = seq_name ann feature in
        if name <> "" then
          try
            let _ = Sequences.Reference.find r
              (Sequences.Types.Forward name) in ()
          with _ ->
            on_violation
              ~path:(Path.to_string (paths ann) path)
              ~feature_id:(feature_id_of feature)
              ~message:(Printf.sprintf
                "sequence %S referenced by feature not in reference" name)
      ) ann
    let validate_feature_bounds
        ?(on_violation = default_on_violation) ann =
      let r = require_reference ann "validate_feature_bounds" in
      iter (fun ~path feature ->
        let name = seq_name ann feature in
        if name <> "" then begin
          let len =
            try Sequences.Reference.length r
                  (Sequences.Types.Forward name)
            with _ -> -1 in
          if len < 0 then ()
          else
            List.iter (fun (i : Sequences.Types.simple_interval_t) ->
              if i.low < 0 || i.low + i.length > len then
                on_violation
                  ~path:(Path.to_string (paths ann) path)
                  ~feature_id:(feature_id_of feature)
                  ~message:(Printf.sprintf
                    "feature on %s [%d, %d) extends past sequence length %d"
                    name i.low (i.low + i.length) len)
            ) feature.intervals
        end
      ) ann
    let validate_translation
        ?(on_violation = default_on_violation) ann =
      (* Resolve the reference here rather than at the first CDS, so that an
         annotation carrying none is reported the same way whether or not it
         happens to contain one. *)
      let _ = require_reference ann "validate_translation" in
      (* GenBank carries the terminating stop in /translation for some entries
         and omits it in others, so compare without it either way. *)
      let strip_trailing_stop s =
        let n = String.length s in
        if n > 0 && s.[n - 1] = '*' then String.sub s 0 (n - 1) else s in
      iter (fun ~path feature ->
        if Path.leaf_category (paths ann) path = "CDS" then
          match attr_get ann feature "translation" with
          | None | Some [] -> ()
          | Some (claimed :: _) ->
            let computed = feature_protein ann feature
            and expected = strip_trailing_stop claimed in
            if computed <> expected then
              on_violation
                ~path:(Path.to_string (paths ann) path)
                ~feature_id:(feature_id_of feature)
                ~message:(Printf.sprintf
                  "CDS on %s [phase=%d, intervals=%d]: claimed translation \
                   does not match computed"
                  (seq_name ann feature) (Option.value ~default:0 feature.phase)
                  (List.length feature.intervals))
      ) ann
  end

open Annotation

(* A criterion picking out a subset of an annotation's features.  Asking an
   annotation what it contains is a library concern rather than a CLI one, so
   that every consumer -- and the test suite -- can do it; AnnoTools adds only
   the command-line spelling of a criterion on top of this.
   A criterion is evaluated afresh against whatever register it is applied to,
   rather than resolved once into a set of features: an [Annotation.t] is
   rebuilt wholesale by every replace-style read, so a captured set would go
   stale against the annotation it was meant to describe. *)
module Selection =
  struct
    type t =
      | All
      | Labels of StringSet.t
      | Regexps of (string * Str.regexp) list
      | Not of t
    let rec to_string = function
      | All -> "everything"
      | Labels s -> Printf.sprintf "labels {%s}" (StringSet.elements s |> String.concat ",")
      | Regexps l ->
        Printf.sprintf "regexps {%s}"
          (List.map (fun (f, _) -> if f = "" then "<label>" else f) l |> String.concat ",")
      | Not t -> Printf.sprintf "not (%s)" (to_string t)
    let label_of feature = Option.value ~default:"" feature.id
    (* Resolve one criterion's field against a feature.  An empty field name
       matches the feature's label; every other name is a structural field
       first and an attribute key otherwise.  An attribute can hold several
       values, hence the list: the field matches when any one of them does. *)
    let field_of ann ~path feature = function
      | "" | "id" | "label" -> [ label_of feature ]
      | "seq" -> [ seq_name ann feature ]
      | "path" -> [ path_to_string path ]
      | "type" -> [ (match List.rev path with leaf :: _ -> leaf | [] -> "") ]
      | "source" -> [ Option.value ~default:"" (feature_source ann feature) ]
      | "strand" ->
        [ (match feature.strand with
           | Some (Sequences.Types.Forward _) -> "+"
           | Some (Sequences.Types.Reverse _) -> "-"
           | None -> ".") ]
      | key -> Option.value ~default:[] (attr_get ann feature key)
    (* Several field-and-regexp criteria are ANDed. *)
    let rec matches ann ~path feature = function
      | All -> true
      | Labels s -> StringSet.mem (label_of feature) s
      | Regexps l ->
        List.for_all
          (fun (f, re) -> List.exists (Str.matches re) (field_of ann ~path feature f)) l
      | Not t -> not (matches ann ~path feature t)
    (* Iterate the features a criterion selects, in register order. *)
    let iter ann selection f =
      iter_paths (fun ~path feature -> if matches ann ~path feature selection then f ~path feature)
        ann
    let count ann selection =
      let n = ref 0 in
      iter ann selection (fun ~path:_ _ -> incr n);
      !n
  end

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

(* GFF3/GTF ranges are 1-based inclusive in the source; the
   AST stores 0-based half-open. *)
let interval_of_1_based ~lo ~hi : Sequences.Types.simple_interval_t =
  (* Positions are 1-based, so anything below 1 is not a coordinate.  Left
     unchecked, a [lo] of 0 yields [low = -1], which every writer then re-emits
     happily -- GFF3 [0 500], GenBank [0..500] -- and which only surfaces much
     later, and as an internal error rather than a diagnosis, when the reference
     is finally indexed. *)
  if lo < 1 then
    Exception.raise __FUNCTION__ IO_Format
      (Printf.sprintf "Invalid 1-based coordinate %d (positions start at 1)" lo);
  (* [hi = lo - 1] is the one legal inversion: it is how a zero-length site --
     the position between two consecutive bases, GenBank's [lo^hi] -- comes back
     from a format that has only a 1-based inclusive pair to spell it with.  It
     denotes the 0-based half-open interval [lo - 1, lo - 1), which is exactly
     what [GenBankLocation.intervals] stores for [Between]. *)
  if hi = lo - 1 then { low = lo - 1; length = 0 }
  else begin
    if hi < lo then
      Exception.raise __FUNCTION__ IO_Format
        (Printf.sprintf "Invalid interval (lo=%d, hi=%d)" lo hi);
    { low = lo - 1; length = hi - lo + 1 }
  end

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
    val intervals:
      t ->
      (string option * Sequences.Types.simple_interval_t) list
      * Sequences.Types.strand_t option
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
      let rec walk strand seq = function
        | Point e ->
          (* Through [interval_of_1_based] rather than [mk_simple], so that a
             point location of 0 is caught by the same check as a range. *)
          [ seq, interval_of_1_based ~lo:e.pos ~hi:e.pos ], strand
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
          [ seq, interval_of_1_based ~lo:a.pos ~hi:b.pos ], strand
        | Between (a, _) ->
          (* Zero-length feature between [a] and [a+1]. *)
          [ seq, mk_simple a 0 ], strand
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
             the parts have to agree.  A feature carries ONE strand, so a
             mixed-strand join -- legal INSDC, and how trans-splicing is spelled
             -- cannot be represented here.  Refusing it is the point: the
             previous version kept whichever strand came last, which silently
             reverse-complemented the parts that disagreed with it. *)
          let acc = ref [] and st = ref strand and seen = ref false in
          List.iter (fun p ->
            let pieces, s = walk strand seq p in
            if !seen && s <> !st then
              Exception.raise __FUNCTION__ IO_Format
                "Mixed-strand join/order is not representable: a feature carries a \
                 single strand, so its parts must all lie on the same one";
            st := s;
            seen := true;
            acc := !acc @ pieces) parts;
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
          if !seen && !st <> strand then List.rev !acc, !st else !acc, !st
        | Remote (acc_name, _, inner) ->
          walk strand (Some acc_name) inner in
      walk None None loc
  end

(* Read an entire file into memory.  All format readers below
   are string-based (they keep the whole input around for
   topological sorting anyway), so the file-vs-string distinction
   is only an I/O wrapper. *)
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
   earlier version of this comment claimed.  [AttrMap] is keyed by the integer
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
   supplied carrier annotation register, using the carrier's
   hierarchy for validation; the returned annotation may also
   carry an updated reference (in GenBank's case, populated
   from any ORIGIN block) replacing whatever was on the
   carrier.  [of_string] / [of_file] are convenience
   constructors equivalent to [read] over a fresh register
   seeded with the requested hierarchy.  The canonical short
   name of each format lives on [Format.to_string] / its
   inverse [Format.of_string], not in the signature itself. *)
(* The write half of a format, split out so that a format which can only be
   written is expressible.  NCBI's submission feature table is one: it has no
   slot for a source column, no parent link and no annotation metadata, and
   table2asn INFERS the gene/mRNA/CDS relations from coordinate overlap rather
   than reading them, so a register cannot be recovered from one.  Without this
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

(* GFF3 satisfies [Format_t] and additionally exposes the
   built-in [gencode_hierarchy] under its own name (also
   reachable via the [dialects] association). *)
module GFF3:
  sig
    include Format_t
    val gencode_hierarchy: Hierarchy.t
  end
= struct
  let default_hierarchy =
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
  (* GENCODE files (and most Ensembl GFF3) collapse every
     transcript biotype into the single type [transcript]
     (with the actual biotype carried as a transcript_type
     attribute), and use
     [stop_codon_redefined_as_selenocysteine] for
     selenocysteines.  Distinct top-level types are gene
     only.  Derived from a survey of
     gencode.v47.basic.annotation.gff3. *)
  let gencode_hierarchy =
    Hierarchy.of_string
      "(gene \
          ((transcript \
             (exon, \
              (CDS (stop_codon_redefined_as_selenocysteine)), \
              five_prime_UTR, three_prime_UTR, \
              start_codon, stop_codon))))"
  let dialects = [
    "standard", default_hierarchy;
    "gencode", gencode_hierarchy
  ]
  (* *)
  let parse_attributes s =
    let lexbuf = Lexing.from_string ~with_positions:true s in
    Annotations_Parse.gff_attribute_list Annotations_Lex.gff_attributes lexbuf
  (* Single GFF3 row -> (id, parent_id option, type, feature).
     The [seq] and [attributes] of [row_feature] are
     pre-interned against the supplied [seqs] / [attr_keys]
     tables so an entire file's worth of "chr1" or "gene_id"
     strings collapse to a handful of ints. *)
  type row_t = {
    row_id: string option;
    row_parent: string option;
    row_type: string;
    row_feature: feature_t
  }
  let parse_row ~seqs ~attr_keys ~values line_no fields =
    if Array.length fields <> 9 then
      Exception.raise __FUNCTION__ IO_Format
        (Printf.sprintf "On line %d: GFF3 row has %d columns, expected 9"
           line_no (Array.length fields));
    let seq = Seq.intern seqs fields.(0)
    and source =
      let s = fields.(1) in
      if s = "" || s = "." then None
      else Some (ValueTable.intern values s)
    and ftype = fields.(2)
    and lo = int_of_string fields.(3)
    and hi = int_of_string fields.(4)
    and score = score_of_field fields.(5)
    and strand = strand_of_field fields.(6)
    and phase = phase_of_field fields.(7)
    and attrs = parse_attributes fields.(8) in
    let attr_map =
      List.fold_left (fun m (k, vs) ->
        let kid = AttrKey.intern attr_keys k in
        let arr =
          Array.of_list
            (List.map (ValueTable.intern values) vs) in
        AttrMap.add kid arr m
      ) AttrMap.empty attrs in
    let id_key = AttrKey.intern attr_keys "ID"
    and parent_key = AttrKey.intern attr_keys "Parent" in
    let first_string m k =
      match AttrMap.find_opt k m with
      | Some arr when Array.length arr > 0 ->
        Some (ValueTable.to_string values arr.(0))
      | _ -> None in
    let id = first_string attr_map id_key
    and parent = first_string attr_map parent_key in
    let feature = {
      seq;
      source;
      intervals = [ interval_of_1_based ~lo ~hi ];
      score;
      strand;
      phase;
      id;
      attributes = attr_map
    } in
    { row_id = id; row_parent = parent; row_type = ftype; row_feature = feature }
  let read_rows ~seqs ~attr_keys ~values s =
    let pragmas = ref [] and rows = ref [] and sequence = ref "" in
    iter_tsv_lines s
      ~pragma:(fun body -> List.accum pragmas body)
      ~fasta:(fun body -> sequence := body)
      ~data:(fun lnum fields ->
        List.accum rows
          (lnum, parse_row ~seqs ~attr_keys ~values lnum fields));
    List.rev !pragmas, List.rev !rows, !sequence
  (* Walk the parent-ID DAG, computing each row's full path
     from root and emitting (path, feature) pairs in DFS
     pre-order suitable for [Annotation.add].  Rows without a
     [Parent] sit at top level; their path is just
     [[row_type]].  Rows with a parent need the parent's path,
     so a topological pass over [Parent=]-edges is required. *)
  let walk_dfs hierarchy rows =
    let root_name = Hierarchy.name hierarchy in
    let by_id = Hashtbl.create 64 in
    List.iter (fun (_, r) ->
      match r.row_id with
      | Some id -> Hashtbl.replace by_id id r
      | None -> ()) rows;
    let path_cache = Hashtbl.create 64 in
    let rec path_of r =
      match r.row_id with
      | Some id when Hashtbl.mem path_cache id ->
        Hashtbl.find path_cache id
      | _ ->
        let p =
          match r.row_parent with
          | None -> [ root_name; r.row_type ]
          | Some pid ->
            let pr =
              try Hashtbl.find by_id pid
              with Not_found ->
                Exception.raise __FUNCTION__ IO_Format
                  (Printf.sprintf
                     "GFF3: feature with Parent=%s but no record \
                      with ID=%s was seen" pid pid) in
            path_of pr @ [ r.row_type ] in
        if not (Hierarchy.validate hierarchy ~path:p) then
          Exception.raise __FUNCTION__ IO_Format
            (Printf.sprintf
               "GFF3: path %s violates the active hierarchy %s"
               (path_to_string p) (Hierarchy.to_string hierarchy));
        (match r.row_id with
         | Some id -> Hashtbl.add path_cache id p
         | None -> ());
        p in
    let kids_of = Hashtbl.create 64 in
    let toplevel = ref [] in
    List.iter (fun (lnum, r) ->
      match r.row_parent with
      | None -> List.accum toplevel (lnum, r)
      | Some pid ->
        let prev =
          try Hashtbl.find kids_of pid with Not_found -> [] in
        Hashtbl.replace kids_of pid ((lnum, r) :: prev)
    ) rows;
    let toplevel = List.rev !toplevel in
    Hashtbl.filter_map_inplace
      (fun _ v -> Some (List.rev v)) kids_of;
    let acc = ref [] in
    let rec emit (_, r) =
      List.accum acc ((path_of r, r.row_feature));
      match r.row_id with
      | None -> ()
      | Some id ->
        (try List.iter emit (Hashtbl.find kids_of id)
         with Not_found -> ()) in
    List.iter emit toplevel;
    List.rev !acc
  (* Carrier-based reader: install the GFF3 features and
     pragmas encoded in [s] into [ann_in], using the carrier's
     hierarchy for validation.  The carrier's interning tables
     are extended in place; pragmas are appended to the
     carrier's metadata map. *)
  let read ann_in s =
    let ann = ref ann_in in
    let hierarchy = Annotation.hierarchy !ann in
    let pragmas, rows, sequence =
      read_rows
        ~seqs:(seqs !ann) ~attr_keys:(attr_keys !ann)
        ~values:(values !ann) s in
    add_dfs_with_seq_bloom ann (walk_dfs hierarchy rows);
    List.iter (fun pragma ->
      match String.index_opt pragma ' ' with
      | None -> ann := add_metadata !ann ~key:pragma ~value:""
      | Some i ->
        let k = String.sub pragma 0 i
        and v = String.sub pragma (i + 1) (String.length pragma - i - 1) in
        ann := add_metadata !ann ~key:k ~value:v
    ) pragmas;
    (* A [##FASTA] section is the annotation's own reference.  Read it with the
       identity linter: what the file says is what it means, and folding an
       IUPAC code to N here would quietly change the sequence. *)
    if sequence <> "" then begin
      let base =
        match Annotation.reference !ann with
        | Some r -> r
        | None -> Sequences.Reference.empty in
      ann :=
        Annotation.set_reference !ann
          (Sequences.Reference.add_from_fasta_string ~linter:Fun.id base sequence)
    end;
    cleanup_values !ann;
    !ann
  let read_from_file ann path = read ann (read_file path)
  let of_string ?(hierarchy = default_hierarchy) s =
    read (create hierarchy) s
  let of_file ?(hierarchy = default_hierarchy) path =
    read_from_file (create hierarchy) path
  (* Output: for each feature, emit one GFF3 row using the
     feature's [seq] / [intervals] / etc., and the
     path-leaf as the type column.  Multi-interval features
     emit one row per interval sharing the [ID]. *)
  (* The bytes that carry structure in column 9.  The lexer percent-decodes on
     the way in, so the writer has to percent-encode on the way out, or a value
     containing one of them changes meaning on the next read: a comma splits it
     into two values, a semicolon into two attributes.
     Space is in the set although GFF3 permits it unencoded, because this
     lexer treats a space as a token separator: encoding it is what makes
     [product=hypothetical protein] survive being written and read again. *)
  let column_9_reserved = ";=&, "
  let attribute_string ann feature =
    let encode = Annotations_Lex.url_encode ~reserved:column_9_reserved in
    let s =
      attribute_pairs ann feature
      |> List.map (fun (k, vs) -> encode k ^ "=" ^ (List.map encode vs |> String.concat ","))
      |> String.concat ";" in
    (* Column 9 is mandatory in GFF3 and uses [.] as the
       attribute-less placeholder; an empty string is not
       valid. *)
    if s = "" then "." else s
  let row_of_feature ann path feature =
    let ftype = match List.rev path with [] -> "" | x :: _ -> x in
    let seq = seq_name ann feature
    and src =
      match feature_source ann feature with
      | Some s -> s | None -> "."
    and score = field_of_score feature.score
    and strand =
      match feature.strand with
      | Some Sequences.Types.Forward _ -> "+"
      | Some Sequences.Types.Reverse _ -> "-"
      | None -> "."
    and attrs = attribute_string ann feature in
    (* Column 8 is per ROW, not per feature: it says how many bases of the first
       codon of THIS row lie in the previous rows.  Stamping the feature's phase
       on every row of a multi-exon CDS is right only for the first.  Intervals
       are stored in transcription order -- the order [feature_dna] splices them
       -- so the running total of coding bases already 5' of each row gives it
       directly, with no strand special case. *)
    let phase_of consumed =
      match feature.phase with
      | None -> "."
      | Some p -> string_of_int (((p - consumed) mod 3 + 3) mod 3) in
    let _, rows =
      List.fold_left (fun (consumed, acc) (ivl: Sequences.Types.simple_interval_t) ->
        let lo = ivl.low + 1
        and hi = ivl.low + ivl.length in
        consumed + ivl.length,
        Printf.sprintf "%s\t%s\t%s\t%d\t%d\t%s\t%s\t%s\t%s"
          seq src ftype lo hi score strand (phase_of consumed) attrs :: acc)
        (0, []) feature.intervals in
    List.rev rows
  let to_buffer buf ann =
    let has_gff_version =
      StringMap.mem "gff-version" (all_metadata ann) in
    if not has_gff_version then
      Buffer.add_string buf "##gff-version 3\n";
    StringMap.iter (fun k vs ->
      List.iter (fun v ->
        Printf.bprintf buf "##%s %s\n" k v
      ) vs
    ) (all_metadata ann);
    iter_paths (fun ~path feature ->
      let rows = row_of_feature ann path feature in
      List.iter (fun r ->
        Buffer.add_string buf r;
        Buffer.add_char buf '\n'
      ) rows
    ) ann;
    (* [##FASTA] is a standard GFF3 directive saying the rest of the file is
       sequence, so a register that has a reference can carry it here rather
       than leaving it to be supplied separately -- which is what makes a
       GenBank record survive a trip through GFF3 whole. *)
    (match reference ann with
     | None -> ()
     | Some r ->
       Buffer.add_string buf "##FASTA\n";
       write_fasta buf r)
  let to_string = to_string_via_buffer to_buffer
  let to_file = to_file_via_buffer to_buffer
end

(* GTF satisfies [Format_t] without any per-module extensions. *)
module GTF: Format_t = struct
  let default_hierarchy =
    Hierarchy.of_string
      "(gene \
         ((transcript \
            (exon, CDS, five_prime_utr, three_prime_utr, \
             start_codon, stop_codon, Selenocysteine))))"
  let dialects = [ "standard", default_hierarchy ]
  let parse_attributes s =
    let lexbuf = Lexing.from_string ~with_positions:true s in
    Annotations_Parse.gtf_attribute_list
      Annotations_Lex.gtf_attributes lexbuf
  type row_t = {
    gtf_seq: string;
    gtf_source: string;
    gtf_type: string;
    gtf_lo: int;
    gtf_hi: int;
    gtf_score: float option;
    gtf_strand: Sequences.Types.strand_t option;
    gtf_phase: int option;
    gtf_attrs: string list StringMap.t;
    gtf_gene_id: string option;
    gtf_tx_id: string option;
    gtf_lnum: int
  }
  let parse_row line_no fields =
    if Array.length fields <> 9 then
      Exception.raise __FUNCTION__ IO_Format
        (Printf.sprintf
           "On line %d: GTF row has %d columns, expected 9"
           line_no (Array.length fields));
    let attrs = parse_attributes fields.(8) in
    let attr_map =
      List.fold_left (fun m (k, vs) ->
        let prev = try StringMap.find k m with Not_found -> [] in
        StringMap.add k (prev @ vs) m
      ) StringMap.empty attrs in
    let lookup1 k =
      match StringMap.find_opt k attr_map with
      | Some (v :: _) -> Some v
      | _ -> None in
    {
      gtf_seq = fields.(0);
      gtf_source = fields.(1);
      gtf_type = fields.(2);
      gtf_lo = int_of_string fields.(3);
      gtf_hi = int_of_string fields.(4);
      gtf_score = score_of_field fields.(5);
      gtf_strand = strand_of_field fields.(6);
      gtf_phase = phase_of_field fields.(7);
      gtf_attrs = attr_map;
      gtf_gene_id = lookup1 "gene_id";
      gtf_tx_id = lookup1 "transcript_id";
      gtf_lnum = line_no
    }
  let read_rows s =
    let rows = ref [] in
    iter_tsv_lines s
      ~data:(fun lnum fields ->
        List.accum rows (parse_row lnum fields));
    List.rev !rows
  let value_array_of_strings values vs =
    Array.of_list (List.map (ValueTable.intern values) vs)
  let intern_source_field values s =
    if s = "" || s = "." then None
    else Some (ValueTable.intern values s)
  (* Build a feature_t from a [row_t], interning into the
     supplied tables. *)
  let feature_of_row ~seqs ~attr_keys ~values r =
    let attr_map =
      StringMap.fold (fun k vs m ->
        let kid = AttrKey.intern attr_keys k in
        AttrMap.add kid (value_array_of_strings values vs) m
      ) r.gtf_attrs AttrMap.empty in
    {
      seq = Seq.intern seqs r.gtf_seq;
      source = intern_source_field values r.gtf_source;
      intervals =
        [ interval_of_1_based ~lo:r.gtf_lo ~hi:r.gtf_hi ];
      score = r.gtf_score;
      strand = r.gtf_strand;
      phase = r.gtf_phase;
      id = None;
      attributes = attr_map
    }
  let synth_feature ~seqs ~attr_keys ~values ~seq ~source
                    ~lo ~hi ~strand ~id ~attrs =
    let attr_map =
      List.fold_left (fun m (k, vs) ->
        let kid = AttrKey.intern attr_keys k in
        AttrMap.add kid (value_array_of_strings values vs) m
      ) AttrMap.empty attrs in
    {
      seq = Seq.intern seqs seq;
      source = intern_source_field values source;
      intervals = [ interval_of_1_based ~lo ~hi ];
      (* A synthesised parent has no score of its own: GTF gives one only to
         the rows actually present in the file. *)
      score = None;
      strand;
      phase = None;
      id = Some id;
      attributes = attr_map
    }
  (* GTF normally has explicit gene/transcript rows from
     Ensembl/GENCODE, but plain GTF (UCSC, pure-CDS) often
     only has exon/CDS rows.  We synthesise gene/transcript
     spans from the union of children intervals when they
     are not present in the source. *)
  let gtf_walk_dfs ~seqs ~attr_keys ~values hierarchy rows =
    let span_of_intervals ivs =
      List.fold_left (fun (lo, hi) (i, j) ->
        (min lo i), (max hi j)
      ) (max_int, min_int) ivs in
    (* Group by (seq, gene_id) and within each by tx_id. *)
    let by_gene = Hashtbl.create 16 in
    let by_tx = Hashtbl.create 64 in
    let gene_order = ref [] in
    let tx_order = ref [] in
    let gene_explicit = Hashtbl.create 16 in
    let tx_explicit = Hashtbl.create 64 in
    List.iter (fun r ->
      let gid =
        match r.gtf_gene_id with
        | Some s -> s
        | None ->
          Exception.raise __FUNCTION__ IO_Format
            (Printf.sprintf
               "On line %d: GTF row missing gene_id"
               r.gtf_lnum) in
      let key_g = r.gtf_seq, gid in
      if not (Hashtbl.mem by_gene key_g) then begin
        Hashtbl.add by_gene key_g [];
        List.accum gene_order key_g
      end;
      (match r.gtf_type with
       | "gene" -> Hashtbl.replace gene_explicit key_g r
       | "transcript" ->
         let tid =
           match r.gtf_tx_id with Some s -> s | None ->
             Exception.raise __FUNCTION__ IO_Format
               (Printf.sprintf
                  "On line %d: transcript row missing transcript_id"
                  r.gtf_lnum) in
         let key_t = r.gtf_seq, gid, tid in
         Hashtbl.replace tx_explicit key_t r
       | _ ->
         match r.gtf_tx_id with
         | None -> ()
         | Some tid ->
           let key_t = r.gtf_seq, gid, tid in
           if not (Hashtbl.mem by_tx key_t) then begin
             Hashtbl.add by_tx key_t [];
             List.accum tx_order key_t
           end;
           Hashtbl.replace by_tx key_t
             ((Hashtbl.find by_tx key_t) @ [r]))
    ) rows;
    let gene_order = List.rev !gene_order in
    let tx_order = List.rev !tx_order in
    let acc = ref [] in
    let emit (path : string list) (feature : feature_t) =
      if not (Hierarchy.validate hierarchy ~path) then
        Exception.raise __FUNCTION__ IO_Format
          (Printf.sprintf
             "GTF: path %s violates hierarchy %s"
             (path_to_string path)
             (Hierarchy.to_string hierarchy));
      List.accum acc (path, feature) in
    List.iter (fun (seq, gid) ->
      let key_g = (seq, gid) in
      let gene_row =
        try Some (Hashtbl.find gene_explicit key_g)
        with Not_found -> None in
      (* gather all transcript keys belonging to this gene,
         in source order. *)
      let txs =
        List.filter (fun (s, g, _) -> s = seq && g = gid)
          tx_order in
      (* compute gene span as union of all sub-features. *)
      let gene_span =
        let collected = ref [] in
        List.iter (fun key_t ->
          List.iter (fun r ->
            List.accum collected (r.gtf_lo, r.gtf_hi)
          ) (try Hashtbl.find by_tx key_t with _ -> [])
        ) txs;
        (match gene_row with
         | Some r -> List.accum collected (r.gtf_lo, r.gtf_hi)
         | None -> ());
        if !collected = [] then 1, 1
        else span_of_intervals !collected in
      let gene_lo, gene_hi = gene_span in
      let strand_of_first =
        match gene_row with
        | Some r -> r.gtf_strand
        | None ->
          (match txs with
           | (_, _, _) as kt :: _ ->
             (match Hashtbl.find by_tx kt with
              | r :: _ -> r.gtf_strand
              | [] -> None)
           | [] -> None) in
      let gene_feature =
        match gene_row with
        | Some r ->
          let f = feature_of_row ~seqs ~attr_keys ~values r in
          { f with id = Some gid }
        | None ->
          synth_feature ~seqs ~attr_keys ~values
            ~seq ~source:"" ~lo:gene_lo ~hi:gene_hi
            ~strand:strand_of_first ~id:gid
            ~attrs:[ "gene_id", [ gid ] ] in
      emit [ implicit_root_name; "gene" ] gene_feature;
      List.iter (fun key_t ->
        let _, _, tid = key_t in
        let tx_row =
          try Some (Hashtbl.find tx_explicit key_t)
          with Not_found -> None in
        let kids =
          try Hashtbl.find by_tx key_t with _ -> [] in
        let tx_span =
          let pts = List.map (fun r ->
            r.gtf_lo, r.gtf_hi) kids in
          let pts =
            match tx_row with
            | Some r -> (r.gtf_lo, r.gtf_hi) :: pts
            | None -> pts in
          if pts = [] then 1, 1 else span_of_intervals pts in
        let tx_lo, tx_hi = tx_span in
        let tx_strand =
          match tx_row, kids with
          | Some r, _ -> r.gtf_strand
          | None, r :: _ -> r.gtf_strand
          | _ -> strand_of_first in
        let tx_feature =
          match tx_row with
          | Some r ->
            let f = feature_of_row ~seqs ~attr_keys ~values r in
            { f with id = Some tid }
          | None ->
            synth_feature ~seqs ~attr_keys ~values
              ~seq ~source:"" ~lo:tx_lo ~hi:tx_hi
              ~strand:tx_strand ~id:tid
              ~attrs:[ "gene_id", [ gid ];
                       "transcript_id", [ tid ] ] in
        emit [ implicit_root_name; "gene"; "transcript" ]
          tx_feature;
        List.iter (fun r ->
          let leaf = r.gtf_type in
          let path =
            [ implicit_root_name; "gene"; "transcript"; leaf ] in
          let feature = feature_of_row ~seqs ~attr_keys ~values r in
          emit path feature
        ) kids
      ) txs
    ) gene_order;
    List.rev !acc
  (* Carrier-based reader: install the GTF features encoded in
     [s] into [ann_in], using the carrier's hierarchy for
     validation and gene/transcript synthesis. *)
  let read ann_in s =
    let ann = ref ann_in in
    let hierarchy = Annotation.hierarchy !ann in
    let rows = read_rows s in
    let dfs =
      gtf_walk_dfs
        ~seqs:(seqs !ann) ~attr_keys:(attr_keys !ann)
        ~values:(values !ann)
        hierarchy rows in
    add_dfs_with_seq_bloom ann dfs;
    cleanup_values !ann;
    !ann
  let read_from_file ann path = read ann (read_file path)
  let of_string ?(hierarchy = default_hierarchy) s =
    read (create hierarchy) s
  let of_file ?(hierarchy = default_hierarchy) path =
    read_from_file (create hierarchy) path
  let attribute_string ann feature =
    attribute_pairs ann feature
    |> List.concat_map (fun (k, vs) ->
      List.map (fun v -> Printf.sprintf "%s %S" k v) vs)
    |> String.concat "; "
  let row_of_feature ann path feature =
    let leaf = match List.rev path with [] -> "" | x :: _ -> x in
    let strand =
      match feature.strand with
      | Some Sequences.Types.Forward _ -> "+"
      | Some Sequences.Types.Reverse _ -> "-"
      | None -> "." in
    (* Per ROW, not per feature -- see the GFF3 writer above. *)
    let phase_of consumed =
      match feature.phase with
      | None -> "."
      | Some p -> string_of_int (((p - consumed) mod 3 + 3) mod 3) in
    let seq = seq_name ann feature in
    let src =
      match feature_source ann feature with
      | Some s -> s | None -> "BiOCamLib" in
    let attrs =
      let s = attribute_string ann feature in
      (* GTF separates each [key "value"] pair with a trailing
         [;], including after the final pair.  When the feature
         has no attributes we emit an empty column 9 rather than
         a lonely [;], which no consumer would parse as a valid
         attribute list. *)
      if s = "" then "" else s ^ ";" in
    let _, rows =
      List.fold_left (fun (consumed, acc) (i: Sequences.Types.simple_interval_t) ->
        let lo = i.low + 1 and hi = i.low + i.length in
        consumed + i.length,
        Printf.sprintf
          "%s\t%s\t%s\t%d\t%d\t%s\t%s\t%s\t%s"
          seq src leaf lo hi (field_of_score feature.score) strand (phase_of consumed) attrs
          :: acc)
        (0, []) feature.intervals in
    List.rev rows
  let to_buffer buf ann =
    iter_paths (fun ~path feature ->
      List.iter (fun r ->
        Buffer.add_string buf r;
        Buffer.add_char buf '\n'
      ) (row_of_feature ann path feature)
    ) ann
  let to_string = to_string_via_buffer to_buffer
  let to_file = to_file_via_buffer to_buffer
end

(* GenBank satisfies [Format_t] and additionally exposes the
   record-list parser, which is useful to callers that want to
   inspect raw GenBank input without going through the full
   [Annotation.t] construction.  The LOCATION-string entry
   point lives on the extended [GenBankLocation] module
   ([GenBankLocation.of_string]). *)
module GenBank:
  sig
    include Format_t
    val parse_records: string -> Annotations_Base.GenBankRecord.t list
  end
= struct
  (* INSDC feature table standard categories, flat under an
     implicit [source]: features in a GenBank record do not
     carry explicit parent links and the format groups them
     only by interval containment.  Users wanting a richer
     hierarchy can swap this out via [?hierarchy]. *)
  let default_hierarchy =
    Hierarchy.of_string
      "(source \
         (gene, mRNA, tRNA, rRNA, ncRNA, misc_RNA, \
          CDS, exon, intron, \
          five_prime_UTR, three_prime_UTR, \
          promoter, regulatory, \
          repeat_region, misc_feature, \
          variation, polyA_signal, polyA_site, \
          primer_bind, protein_bind, \
          oriT, oriC, \
          sig_peptide, mat_peptide, propeptide, \
          stem_loop, terminator))"
  let dialects = [ "standard", default_hierarchy ]
  (* GenBank record framing is handled by the modal [genbank]
     ocamllex rule: it reads the file line by line, classifying
     each non-blank line into one of nine token classes against
     a [Headers | Features | Origin] mode reset on every [//]
     boundary.  The Menhir grammar [genbank_records] then
     stitches feature continuations onto their location and
     qualifier continuations onto their value, returning a list
     of [GenBankRecord.t] records that the driver below lifts
     into the [Annotation.t] AST. *)
  let parse_records s =
    Annotations_Lex.reset_genbank_mode ();
    let lexbuf = Lexing.from_string ~with_positions:true s in
    Annotations_Parse.genbank_records Annotations_Lex.genbank lexbuf
  (* Pull (locus, sequence-length) out of the LOCUS header
     line.  Standard form is "NAME LEN bp ..." where the [bp]
     marker follows the integer length; we tolerate extra
     whitespace and stop at the first integer-then-bp pair. *)
  let locus_and_length headers =
    match List.assoc_opt "LOCUS" headers with
    | None -> "", 0
    | Some v ->
      let parts =
        String.split_on_char ' ' v |> List.filter (fun x -> x <> "") in
      (match parts with
       | name :: rest ->
         let rec find_len = function
           | [] -> 0
           | a :: "bp" :: _ -> (try int_of_string a with _ -> 0)
           | _ :: rest -> find_len rest in
         name, find_len rest
       | [] -> "", 0)
  (* Convert one parsed [GenBankRecord.feature_t] to
     (path, feature) under the active hierarchy, interning
     [seq_name] and attribute keys into the supplied tables.
     Multi-interval (join/order) features fold to a single
     feature with multiple intervals; the strand carries
     through. *)
  (* Aggregate qualifier repeats into per-key lists, then
     freeze to an [AttrMap.t] over interned values.  Used both
     by [feature_to_pair] and by the source-feature synthesis
     in [read] below. *)
  let attrs_of_qualifiers ~attr_keys ~values qualifiers =
    let acc_lists =
      List.fold_left (fun m (k, v) ->
        let prev = try StringMap.find k m with Not_found -> [] in
        StringMap.add k (prev @ [v]) m
      ) StringMap.empty qualifiers in
    let attrs =
      StringMap.fold (fun k vs m ->
        let kid = AttrKey.intern attr_keys k in
        let arr =
          Array.of_list
            (List.map (ValueTable.intern values) vs) in
        AttrMap.add kid arr m
      ) acc_lists AttrMap.empty in
    acc_lists, attrs
  let feature_to_pair ~seqs ~attr_keys ~values
                      hierarchy seq_name
                      (f : Annotations_Base.GenBankRecord.feature_t) =
    let ftype = f.name in
    let path = [ implicit_root_name; "source"; ftype ] in
    if not (Hierarchy.validate hierarchy ~path) then
      Exception.raise __FUNCTION__ IO_Format
        (Printf.sprintf
           "GenBank: feature %S not allowed under hierarchy %s"
           ftype (Hierarchy.to_string hierarchy));
    let loc = GenBankLocation.of_string f.location in
    let pieces, strand = GenBankLocation.intervals loc in
    let intervals = List.map (fun (_, ivl) -> ivl) pieces in
    let acc_lists, attrs =
      attrs_of_qualifiers ~attr_keys ~values f.qualifiers in
    let lookup_str_attr name =
      match StringMap.find_opt name acc_lists with
      | Some (v :: _) -> Some v
      | _ -> None in
    let id =
      match lookup_str_attr "locus_tag" with
      | Some _ as r -> r
      | None -> lookup_str_attr "gene" in
    (* /codon_start is GenBank's spelling of GFF3's phase, counted from 1 rather
       than from 0, so phase = codon_start - 1.  A CDS carrying no /codon_start
       starts in frame, which is phase 0; leaving it None here made every
       out-of-frame CDS fail validate_translation for the wrong reason. *)
    let phase =
      match lookup_str_attr "codon_start" with
      | None -> None
      | Some s ->
        (match int_of_string_opt (String.trim s) with
         | Some n when n >= 1 && n <= 3 -> Some (n - 1)
         | _ ->
           Exception.raise __FUNCTION__ IO_Format
             (Printf.sprintf "GenBank: invalid /codon_start %S on feature %S (expected 1, 2 or 3)"
                s ftype)) in
    let feature = {
      seq = Seq.intern seqs seq_name;
      source = None;
      intervals;
      (* GenBank has no score column: the concept is GFF3's. *)
      score = None;
      strand;
      phase;
      id;
      attributes = attrs
    } in
    (path, feature)
  (* Carrier-based reader: install the GenBank features
     encoded in [s] into [ann_in], using the carrier's
     hierarchy for validation.  Each record's headers go to the
     carrier's metadata (minus the redundant LOCUS line, which
     the writer regenerates).  When at least one record carries
     an ORIGIN block, the resulting reference -- accumulated
     across all such records -- replaces whatever reference was
     previously attached to the carrier. *)
  let read ann_in s =
    let records = parse_records s in
    if records = [] then ann_in
    else
      let ann = ref ann_in in
      let hierarchy = Annotation.hierarchy !ann in
      let ref_acc = ref Sequences.Reference.empty in
      let any_origin = ref false in
      let first_record = ref true in
      List.iter (fun (r : Annotations_Base.GenBankRecord.t) ->
        (* Each GenBank record is one sequence: drop the
           Bloom on every record boundary except the first
           (where it's already empty). *)
        if !first_record then first_record := false
        else ValueTable.drop_bloom (values !ann);
        let locus, seq_length = locus_and_length r.headers in
        let source_path = [ implicit_root_name; "source" ] in
        let real_source =
          List.find_opt
            (fun (gf : Annotations_Base.GenBankRecord.feature_t) ->
               gf.name = "source") r.features in
        if Hierarchy.validate hierarchy ~path:source_path then begin
          (* Build the top-level source feature from the file's
             own [source] feature if there is one (so its
             /organism, /mol_type, /isolate, /host, /db_xref,
             ... qualifiers carry over to the AST), and fall
             back to a bare LOCUS-line-derived span otherwise. *)
          let source_feature =
            match real_source with
            | Some f ->
              let loc = GenBankLocation.of_string f.location in
              let pieces, _ = GenBankLocation.intervals loc in
              let intervals = List.map (fun (_, ivl) -> ivl) pieces in
              let _, attrs =
                attrs_of_qualifiers
                  ~attr_keys:(attr_keys !ann)
                  ~values:(values !ann) f.qualifiers in
              { seq = Seq.intern (seqs !ann) locus;
                source = None;
                intervals;
                score = None;
                strand = None;
                phase = None;
                id = Some locus;
                attributes = attrs }
            | None ->
              let source_iv : Sequences.Types.simple_interval_t = {
                low = 0;
                length = seq_length
              } in
              { seq = Seq.intern (seqs !ann) locus;
                source = None;
                intervals = [ source_iv ];
                score = None;
                strand = None;
                phase = None;
                id = Some locus;
                attributes = AttrMap.empty } in
          ann := add !ann ~path:source_path source_feature
        end;
        (* The LOCUS line is regenerated canonically by the
           writer from [seq] and the feature span, so we
           don't echo it through metadata -- otherwise the
           output would carry two LOCUS lines. *)
        List.iter (fun (k, v) ->
          if k <> "LOCUS" then
            ann := add_metadata !ann ~key:k ~value:v
        ) r.headers;
        List.iter (fun (gf : Annotations_Base.GenBankRecord.feature_t) ->
          if gf.name <> "source" then
            let path, feature =
              feature_to_pair
                ~seqs:(seqs !ann) ~attr_keys:(attr_keys !ann)
                ~values:(values !ann)
                hierarchy locus gf in
            ann := add !ann ~path feature
        ) r.features;
        (match r.origin with
         | None -> ()
         | Some seq ->
           any_origin := true;
           let tmp = Filename.temp_file "gbk_origin_" ".fa" in
           let oc = open_out tmp in
           Printf.fprintf oc ">%s\n%s\n" locus seq;
           close_out oc;
           ref_acc :=
             Sequences.Reference.add_from_fasta !ref_acc tmp;
           (try Sys.remove tmp with _ -> ()))
      ) records;
      cleanup_values !ann;
      if !any_origin then set_reference !ann !ref_acc else !ann
  let read_from_file ann path = read ann (read_file path)
  let of_string ?(hierarchy = default_hierarchy) s =
    read (create hierarchy) s
  let of_file ?(hierarchy = default_hierarchy) path =
    read_from_file (create hierarchy) path
  (* Output: minimal GenBank with a FEATURES section.  Uses
     each feature's [seq] as the LOCUS name and emits the
     standard 21-column qualifier indentation.  Locations
     are reconstructed from [intervals] and [strand].  ORIGIN
     is emitted when a [Sequences.Reference] is attached. *)
  let format_intervals_strand intervals strand =
    let parts =
      List.map (fun (i : Sequences.Types.simple_interval_t) ->
        (* A zero-length site is INSDC's [lo^hi], the position between two
           consecutive bases.  Running it through the ordinary formula would
           emit the reversed range [low+1..low], which this reader rejects and
           no other tool would accept either. *)
        if i.length = 0 then Printf.sprintf "%d^%d" i.low (i.low + 1)
        else Printf.sprintf "%d..%d" (i.low + 1) (i.low + i.length)
      ) intervals in
    let body =
      match parts with
      | [] -> "1"
      | [ p ] -> p
      | _ ->
        Printf.sprintf "join(%s)" (String.concat "," parts) in
    match strand with
    | Some Sequences.Types.Reverse _ ->
      Printf.sprintf "complement(%s)" body
    | _ -> body
  let to_buffer buf ann =
    (* Group features by [seq], preserving source order.
       Each feature is paired with its leaf category so we
       can emit the right name on the FEATURES line. *)
    let by_seq = Hashtbl.create 16 in
    let order = ref [] in
    iter_paths (fun ~path feature ->
      let seq = seq_name ann feature in
      if not (Hashtbl.mem by_seq seq) then
        List.accum order seq;
      let prev =
        try Hashtbl.find by_seq seq with Not_found -> [] in
      let leaf =
        match List.rev path with x :: _ -> x | [] -> "" in
      Hashtbl.replace by_seq seq ((leaf, feature) :: prev)
    ) ann;
    let order = List.rev !order in
    List.iter (fun seq ->
      let feats = List.rev
        (try Hashtbl.find by_seq seq with _ -> []) in
      let total_len =
        List.fold_left (fun acc (_, f) ->
          List.fold_left
            (fun acc (i : Sequences.Types.simple_interval_t) ->
              max acc (i.low + i.length)) acc f.intervals
        ) 0 feats in
      Printf.bprintf buf "LOCUS       %-16s%d bp    DNA\n"
        seq total_len;
      (* A sub-keyword sits in column 3 with its value in column 13, and has to
         follow the keyword it belongs to.  The metadata map is ordered by key,
         so ORGANISM would otherwise come out at column 1 and in alphabetical
         position -- which is not GenBank, and is not what was read in. *)
      let subs_of = function
        | "SOURCE" -> [ "ORGANISM" ]
        | "REFERENCE" -> [ "AUTHORS"; "CONSRTM"; "TITLE"; "JOURNAL"; "PUBMED"; "REMARK" ]
        | _ -> [] in
      let metadata = all_metadata ann in
      let every_sub = List.concat_map subs_of [ "SOURCE"; "REFERENCE" ] in
      let emit ~indented k =
        match StringMap.find_opt k metadata with
        | None -> ()
        | Some vs ->
          List.iter (fun v ->
            if indented then Printf.bprintf buf "  %-10s%s\n" k v
            else Printf.bprintf buf "%-12s%s\n" k v) vs in
      StringMap.iter (fun k _ ->
        if not (List.mem k every_sub) then begin
          emit ~indented:false k;
          List.iter (emit ~indented:true) (subs_of k)
        end) metadata;
      Buffer.add_string buf
        "FEATURES             Location/Qualifiers\n";
      List.iter (fun (leaf, f) ->
        let name = if leaf = "" then "misc_feature" else leaf in
        let location =
          format_intervals_strand f.intervals f.strand in
        Printf.bprintf buf "     %-16s%s\n" name location;
        attr_iter ann (fun k vs ->
          List.iter (fun v ->
            Printf.bprintf buf
              "                     /%s=%S\n" k v
          ) vs
        ) f
      ) feats;
      (match reference ann with
       | Some r ->
         let opt_seq =
           try
             Some (fst (Sequences.Reference.find r
                          (Sequences.Types.Forward seq)))
           with _ -> None in
         (match opt_seq with
          | None -> ()
          | Some s ->
            Buffer.add_string buf "ORIGIN\n";
            let n = String.length s in
            let i = ref 0 in
            while !i < n do
              Printf.bprintf buf "%9d" (!i + 1);
              let row_end = min n (!i + 60) in
              let j = ref !i in
              while !j < row_end do
                if (!j - !i) mod 10 = 0 then
                  Buffer.add_char buf ' ';
                Buffer.add_char buf
                  (Char.lowercase_ascii s.[!j]);
                incr j
              done;
              Buffer.add_char buf '\n';
              i := row_end
            done)
       | None -> ());
      Buffer.add_string buf "//\n"
    ) order
  let to_string = to_string_via_buffer to_buffer
  let to_file = to_file_via_buffer to_buffer
end

(* Tabular: the register's text twin.  Three relations, none of which contains
   a nested syntax:
     <prefix>.AnnotationFeatures.txt    id parent seq path source score strand phase intervals
     <prefix>.AnnotationAttributes.txt  id key value
     <prefix>.AnnotationMetadata.txt    key value
   Every file has fixed arity, so [cat]ting two of them together is meaningful
   and adding one feature with a novel attribute key does not reshape any other
   row.  Attributes are normalised into their own relation rather than packed
   into a column, which is what lets a multi-valued attribute be several rows
   and a valueless one be a row with an empty third field -- neither of which
   GFF3's column 9 can express.
   A feature's id is a content hash chained through its parent, so it is stable
   under insertion and independent of row order: either file can be sorted and
   still [join]ed on the id.
   [to_file] / [of_file] take a PREFIX and use the three files.  The buffer and
   string entry points render the same content as one document with [#!] section
   banners, which is what makes the format usable down a pipe, diffable in one
   piece, and testable without touching the filesystem; a prefix under [/dev/*]
   selects that form too, matching the convention the binary writers use. *)
module Tabular: Format_t = struct
  (* The file itself carries the hierarchy it was written under, so this is only
     the fallback for a document that does not declare one.  A [--dialect] or
     [--hierarchy] override is meaningless here and AnnoTools refuses it rather
     than installing a default over a file that brought its own. *)
  let default_hierarchy = GFF3.default_hierarchy
  let dialects = [ "standard", default_hierarchy ]
  let format_version = "1"
  let hash_recipe = "fnv1a64/1"
  (* FNV-1a over 64 bits, written out rather than reached for.  [Hashtbl.hash]
     would have been wrong twice over: it yields about 30 usable bits, where
     2.2 million features need 64 to keep the collision probability near 1e-7,
     and it is not guaranteed stable across OCaml releases, which for a value
     that goes into a file and has to mean the same thing on the way back is
     fatal. *)
  let hash_of_string s =
    let h = ref 0xcbf29ce484222325L in
    String.iter
      (fun c -> h := Int64.mul (Int64.logxor !h (Int64.of_int (Char.code c))) 0x100000001b3L) s;
    Printf.sprintf "%016Lx" !h
  (* Field encoding.  [%] always goes, so that a literal one cannot be read back
     as an escape; control bytes go, since a tab or a newline would end the
     field or the row; [>] goes in a path segment, so that the [->] separator
     can never occur inside one.  A field that would come out as exactly [.] is
     encoded too, because [.] is the absent marker. *)
  let encode ?(reserved = "") s =
    if s = "." then "%2E" else Annotations_Lex.url_encode ~reserved s
  let decode = Annotations_Lex.url_decode
  let encode_opt ?reserved = function
    | None -> "."
    | Some s -> encode ?reserved s
  let decode_opt = function
    | "." -> None
    | s -> Some (decode s)
  (* Intervals, in the INSDC spelling: 1-based inclusive, comma-joined, in the
     order stored rather than sorted, with a zero-length site as [lo^hi]. *)
  let intervals_to_field intervals =
    match intervals with
    | [] -> "."
    | _ ->
      List.map (fun (i: Sequences.Types.simple_interval_t) ->
        if i.length = 0 then Printf.sprintf "%d^%d" i.low (i.low + 1)
        else Printf.sprintf "%d..%d" (i.low + 1) (i.low + i.length)) intervals
      |> String.concat ","
  let intervals_of_field = function
    | "." | "" -> []
    | s ->
      String.Split.on_char_as_list ',' s
      |> List.map (fun piece ->
        let two sep =
          match String.Split.as_list (Str.regexp_string sep) piece with
          | [ a; b ] ->
            (match int_of_string_opt a, int_of_string_opt b with
             | Some a, Some b -> Some (a, b)
             | _ -> None)
          | _ -> None in
        match two ".." with
        | Some (lo, hi) -> interval_of_1_based ~lo ~hi
        | None ->
          (match two "^" with
           (* Only consecutive positions denote a between-bases site.  Accepting
              any [hi] meant [100^999] parsed happily and was then re-emitted as
              [100^101], so a hand-edited file was silently rewritten rather
              than diagnosed -- and hand editing is what this format is for. *)
           | Some (lo, hi) when hi = lo + 1 ->
             if lo < 1 then
               Exception.raise __FUNCTION__ IO_Format
                 (Printf.sprintf "Invalid 1-based coordinate %d (positions start at 1)" lo);
             { Sequences.Types.low = lo; length = 0 }
           | Some (lo, hi) ->
             Exception.raise __FUNCTION__ IO_Format
               (Printf.sprintf
                  "Invalid between-bases site %d^%d: the two positions must be consecutive" lo hi)
           | None ->
             Exception.raise __FUNCTION__ IO_Format
               (Printf.sprintf "Invalid interval %S (expected lo..hi or lo^hi)" piece)))
  let strand_to_field = function
    | Some (Sequences.Types.Forward _) -> "+"
    | Some (Sequences.Types.Reverse _) -> "-"
    | None -> "."
  let phase_to_field = function
    | None -> "."
    | Some n -> string_of_int n
  (* The path is the category chain WITHOUT the implicit root, which is stripped
     on output and restored on input -- the same treatment [Hierarchy.to_string]
     gives it, and for the same reason: it is redundant on every row. *)
  let path_to_field path =
    match path with
    | root :: rest when root = implicit_root_name ->
      List.map (encode ~reserved:">") rest |> String.concat "->"
    | _ -> List.map (encode ~reserved:">") path |> String.concat "->"
  let path_of_field s =
    implicit_root_name
    :: (if s = "" || s = "." then []
        else String.Split.as_list (Str.regexp_string "->") s |> List.map decode)
  (* The format's own metadata keys live in a [!] namespace so that they cannot
     collide with an annotation's; a real key that happens to start with [!] is
     encoded on the way out. *)
  let own_key k = "!" ^ k
  (* A leading [!] would collide with that namespace, and a leading [#] would
     let a metadata row impersonate a section banner in the one-document form --
     a key of [#!features] would end the metadata table and start a second
     features one.  Both are escaped; [url_decode] restores them. *)
  let encode_metadata_key k =
    if k = "" then ""
    else
      let rest = encode (String.sub k 1 (String.length k - 1)) in
      match k.[0] with
      | '!' -> "%21" ^ rest
      (* A leading [#] would make the row look like a table header, and a
         leading [>] like the start of the reference. *)
      | '#' -> "%23" ^ rest
      | '>' -> "%3E" ^ rest
      | _ -> encode k
  (* The per-sequence translation table, recorded only when it is not the
     standard one, so an ordinary annotation carries no such rows at all. *)
  let table_key name = own_key ("table:" ^ name)
  (* [id] is the content hash and the join key; [feature_id] is the feature's
     OWN identifier, which is not always derivable from an attribute -- the
     GenBank reader names a record's source feature after its LOCUS -- and so
     needs a column of its own or it is simply lost.  The name matches the
     column --validate-report already uses for the same thing. *)
  (* A table opens with ONE line naming its columns, prefixed with [#].  That
     single rule replaces a banner plus a bare header row, and it is enough on
     its own: the three headers differ, so in the one-document form the header
     IS the table's identity and nothing else has to be agreed on.  A [>] line
     opens the reference, which is plain FASTA. *)
  let features_header =
    "#id\tparent\tseq\tpath\tfeature_id\tsource\tscore\tstrand\tphase\tintervals"
  let attributes_header = "#id\tkey\tvalue"
  let metadata_header = "#key\tvalue"
  let features_suffix = ".AnnotationFeatures.txt"
  let attributes_suffix = ".AnnotationAttributes.txt"
  let metadata_suffix = ".AnnotationMetadata.txt"
  let reference_suffix = ".AnnotationReference.fasta"
  let single_document prefix = String.length prefix >= 5 && String.sub prefix 0 5 = "/dev/"
  (* A prefix under [/dev/*] selects the one-document form, as it does for the
     binary writers.  So does an ordinary path that turns out to BE a document:
     someone handed a file rather than a prefix, and refusing it because its
     name lacks a suffix we invented would be unhelpful. *)
  let looks_like_document path =
    match open_in path with
    | exception _ -> false
    | ic ->
      (* [read] tolerates a preamble before the first banner, so the sniff has
         to skip the same thing rather than decide on line one -- otherwise a
         document carrying a comment header is taken for a prefix and the reader
         then goes looking for files that do not exist.  The first [#!] line
         settles it either way; anything that is not blank, not a comment and
         not a banner means this is not a document.  The line cap stops the
         sniff reading an arbitrary large file that merely begins with
         comments.
         [Sys_error] is caught alongside [End_of_file] because [open_in] on a
         directory succeeds on Linux and it is the READ that fails: without
         this the exception escaped a function whose whole job is to answer
         yes or no, and the channel leaked with it. *)
      let rec scan n =
        if n = 0 then false
        else
          match input_line ic with
          | exception (End_of_file | Sys_error _) -> false
          | line ->
            let trimmed = String.trim line in
            if trimmed = "" then scan (n - 1)
            else
              trimmed = metadata_header || trimmed = features_header
              || trimmed = attributes_header in
      let verdict = scan 100 in
      close_in_noerr ic;
      verdict
  (* Rendering.  Every feature is visited once, in DFS pre-order, and its id is
     computed from its identity chained through its parent's id -- so an id
     depends on where a feature sits, not merely on what it says, which is what
     separates the identical exons that alternative transcripts share. *)
  type rendered_t = {
    r_features: Buffer.t;
    r_attributes: Buffer.t;
    r_metadata: Buffer.t;
    (* Empty when no reference is attached, which is the GFF3-shaped case. *)
    r_reference: Buffer.t
  }
  let render ann =
    let r = {
      r_features = Buffer.create 4096;
      r_attributes = Buffer.create 4096;
      r_metadata = Buffer.create 256;
      r_reference = Buffer.create 4096
    } in
    (* The reference goes out as FASTA rather than into a table: a sequence is
       not tabular data, and a 30 kbp cell would be the same category error as
       packing attributes into one column.  As FASTA it is also readable by
       every other tool it might be handed to, and comes back in through the
       reader this library already has. *)
    Printf.bprintf r.r_metadata "%s\n" metadata_header;
    (match reference ann with
     | None -> ()
     | Some reference ->
       Sequences.Reference.iter (fun ~name ~seq:_ ~table ->
         if table <> Sequences.Translation.Table_1 then
           Printf.bprintf r.r_metadata "%s\t%s\n" (table_key name)
             (Sequences.Translation.to_string table)) reference;
       write_fasta r.r_reference reference);
    Printf.bprintf r.r_metadata "%s\t%s\n" (own_key "format-version") format_version;
    Printf.bprintf r.r_metadata "%s\t%s\n" (own_key "hash-recipe") hash_recipe;
    Printf.bprintf r.r_metadata "%s\t%s\n" (own_key "hierarchy")
      (encode (Hierarchy.to_string (hierarchy ann)));
    StringMap.iter (fun k vs ->
      List.iter (fun v ->
        Printf.bprintf r.r_metadata "%s\t%s\n" (encode_metadata_key k) (encode v)) vs)
      (all_metadata ann);
    Buffer.add_string r.r_features features_header;
    Buffer.add_char r.r_features '\n';
    Buffer.add_string r.r_attributes attributes_header;
    Buffer.add_char r.r_attributes '\n';
    (* [id_of_path] remembers the id of the most recent feature seen at each
       path prefix, which in DFS pre-order is exactly the parent of whatever
       comes next at the level below. *)
    let id_of_path = Hashtbl.create 64
    and seen = Hashtbl.create 64
    and duplicates = Hashtbl.create 64 in
    iter_paths (fun ~path feature ->
      let depth = List.length path in
      let parent =
        if depth <= 2 then ""
        else
          match Hashtbl.find_opt id_of_path (List.filteri (fun i _ -> i < depth - 1) path) with
          | Some id -> id
          | None ->
            Exception.raise __FUNCTION__ Algorithm
              (Printf.sprintf "No parent id for %s: the walk is not in DFS order"
                 (path_to_string path)) in
      let category = match List.rev path with leaf :: _ -> leaf | [] -> "" in
      let intervals = intervals_to_field feature.intervals in
      let strand = strand_to_field feature.strand
      and phase = phase_to_field feature.phase in
      (* The feature's own identifier belongs in its identity, not in its
         payload: two alternative transcripts of one gene can agree on every
         structural field -- same span, same strand, same parent -- and differ
         only by their ID, and chaining through the parent alone would then give
         them, and every exon beneath them, the same id.
         Features that carry no identifier at all (a GenBank feature with
         neither /locus_tag nor /gene) can still be structurally identical
         siblings, so a counter distinguishes those.  It is keyed by the whole
         identity rather than by the parent, so it only ever moves off zero for
         an actual duplicate: adding an unrelated sibling does not renumber
         anything. *)
      let base =
        String.concat "\000"
          [ parent; Option.value ~default:"" feature.id; seq_name ann feature; category;
            intervals; strand; phase ] in
      let ordinal =
        match Hashtbl.find_opt duplicates base with
        | None -> Hashtbl.replace duplicates base 1; 0
        | Some n -> Hashtbl.replace duplicates base (n + 1); n in
      let key = Printf.sprintf "%s\000#%d" base ordinal in
      let id = hash_of_string key in
      (* Two features sharing an id would silently merge their attributes on the
         way back in, so the writer checks rather than trusting the arithmetic:
         everything is in memory here and the check costs one hashtable. *)
      (match Hashtbl.find_opt seen id with
       | Some prev when prev <> key ->
         Exception.raise __FUNCTION__ Algorithm
           (Printf.sprintf "Hash collision on %s: two distinct features share id %s" hash_recipe id)
       | _ -> Hashtbl.replace seen id key);
      Hashtbl.replace id_of_path path id;
      Printf.bprintf r.r_features "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n"
        id (if parent = "" then "." else parent) (encode (seq_name ann feature))
        (path_to_field path) (encode_opt feature.id) (encode_opt (feature_source ann feature))
        (field_of_score_exact feature.score) strand phase intervals;
      (* Sorted, because AttrMap order is an interning artefact: a format whose
         point is to be diffable cannot inherit an order nobody controls. *)
      let pairs = ref [] in
      attr_iter ann (fun k vs -> List.accum pairs (k, vs)) feature;
      List.sort (fun (a, _) (b, _) -> compare a b) !pairs
        |> List.iter (fun (k, vs) ->
          match vs with
          (* No values at all is not the same as one empty value, but the row
             shape cannot say both -- [<id> <key> <empty>] already means the
             latter, which is how a valueless qualifier travels.  An attribute
             with an empty value ARRAY is a degenerate state no reader produces
             (only [attr_set ~values:[]] can), and it means the same as the
             attribute being absent, so that is how it is written. *)
          | [] -> ()
          | _ ->
            List.iter
              (fun v ->
                Printf.bprintf r.r_attributes "%s\t%s\t%s\n" id (encode k) (encode v)) vs)) ann;
    r
  let to_buffer buf ann =
    let r = render ann in
    (* Each table already opens with its own header line, so they simply
       follow one another; the reference, being FASTA, goes last. *)
    Buffer.add_buffer buf r.r_metadata;
    Buffer.add_buffer buf r.r_features;
    Buffer.add_buffer buf r.r_attributes;
    Buffer.add_buffer buf r.r_reference
  let to_string = to_string_via_buffer to_buffer
  let to_file ann prefix =
    if single_document prefix then to_file_via_buffer to_buffer ann prefix
    else begin
      let r = render ann in
      List.iter (fun (suffix, buf) ->
        (* No reference means no FASTA file, rather than an empty one. *)
        if suffix <> reference_suffix || Buffer.length buf > 0 then begin
          let oc = open_out (prefix ^ suffix) in
          Buffer.output_buffer oc buf;
          close_out oc
        end)
        [ features_suffix, r.r_features;
          attributes_suffix, r.r_attributes;
          metadata_suffix, r.r_metadata;
          reference_suffix, r.r_reference ]
    end
  (* Reading.  Rows are collected first and the forest rebuilt from the parent
     column afterwards, so neither file has to arrive in any particular order:
     both may be sorted, which is most of the point of a tabular format. *)
  (* Split into lines, dropping a trailing CR the way [iter_tsv_lines] already
     does for GFF3 and GTF.  Without it a CRLF file carries the CR into the last
     field of every row: in the attributes table that is the value itself, which
     is then interned and re-emitted downstream as [ID=gene1%0D] -- silent
     corruption rather than a diagnosis. *)
  let lines_of s =
    String.Split.on_char_as_list '\n' s
    |> List.map (fun l ->
      let n = String.length l in
      if n > 0 && l.[n - 1] = '\r' then String.sub l 0 (n - 1) else l)
  (* Split one table's text into rows.  A leading [#] line is the header this
     table was recognised by and is checked; its absence is an error rather than
     something to guess around.  Only a genuinely EMPTY line is filler --
     trimming first would discard a metadata row whose key and value are both
     empty, which a bare [##] pragma in a GFF3 file produces. *)
  let split_rows header what s =
    let lines = lines_of s |> List.filter (fun l -> l <> "") in
    match lines with
    | [] -> []
    | first :: rest when String.trim first = header ->
      List.map (fun l -> String.Split.on_char_as_array '\t' l) rest
    | first :: _ ->
      Exception.raise __FUNCTION__ IO_Format
        (Printf.sprintf "Malformed %s table: expected header %S, found %S" what header
           (String.trim first))
  let field row i what n =
    if Array.length row <> n then
      Exception.raise __FUNCTION__ IO_Format
        (Printf.sprintf "Malformed %s row: expected %d fields, found %d" what n
           (Array.length row));
    row.(i)
  let read_tables ann_in ~features ~attributes ~metadata ~reference_fasta =
    (* Metadata first: it carries the hierarchy the rest has to validate
       against. *)
    let file_hierarchy = ref None and plain_metadata = ref []
    and tables = ref StringMap.empty in
    let table_prefix = own_key "table:" in
    List.iter (fun row ->
      let k = field row 0 "metadata" 2 and v = field row 1 "metadata" 2 in
      let is_table =
        String.length k > String.length table_prefix
        && String.sub k 0 (String.length table_prefix) = table_prefix in
      match k with
      | "!hierarchy" -> file_hierarchy := Some (Hierarchy.of_string (decode v))
      | "!format-version" ->
        if decode v <> format_version then
          Exception.raise __FUNCTION__ IO_Format
            (Printf.sprintf "Unsupported tabular format version %S (this is %S)" (decode v)
               format_version)
      | "!hash-recipe" -> ()
      | _ when is_table ->
        let name =
          decode (String.sub k (String.length table_prefix)
                    (String.length k - String.length table_prefix)) in
        tables := StringMap.add name (Sequences.Translation.of_string (decode v)) !tables
      | _ -> List.accum plain_metadata (decode k, decode v))
      (split_rows metadata_header "metadata" metadata);
    let is_empty = fold (fun ~path:_ _ _ -> false) true ann_in in
    let ann =
      match !file_hierarchy with
      | Some h when is_empty ->
        (* The register is rebuilt around the hierarchy the file declares, so
           everything on the carrier that is NOT the hierarchy has to be carried
           across -- its reference, and its metadata, which is register state in
           exactly the same way and was previously dropped without a word.  The
           carrier's entries go in first, so that the file's append after them,
           which is the order every other add-mode read already gives. *)
        let fresh = create h in
        let fresh =
          match reference ann_in with
          | Some r -> set_reference fresh r
          | None -> fresh in
        ref
          (StringMap.fold
             (fun k vs acc ->
               List.fold_left (fun acc v -> add_metadata acc ~key:k ~value:v) acc vs)
             (all_metadata ann_in) fresh)
      | Some h when Hierarchy.to_string h <> Hierarchy.to_string (hierarchy ann_in) ->
        Exception.raise __FUNCTION__ IO_Format
          "Tabular input declares a hierarchy that differs from the register's; \
           load it into an empty register instead"
      | _ -> ref ann_in in
    (* Attributes, grouped by feature id.  Values keep the order they appear in,
       which is what makes a multi-valued attribute round-trip. *)
    let attrs_of = Hashtbl.create 64 in
    List.iter (fun row ->
      let id = field row 0 "attributes" 3
      and k = decode (field row 1 "attributes" 3)
      and v = decode (field row 2 "attributes" 3) in
      let prev = try Hashtbl.find attrs_of id with Not_found -> [] in
      Hashtbl.replace attrs_of id ((k, v) :: prev))
      (split_rows attributes_header "attributes" attributes);
    let rows = split_rows features_header "features" features in
    let by_id = Hashtbl.create 64 and children = Hashtbl.create 64 and roots = ref [] in
    List.iter (fun row ->
      let id = field row 0 "features" 10 and parent = field row 1 "features" 10 in
      if Hashtbl.mem by_id id then
        Exception.raise __FUNCTION__ IO_Format
          (Printf.sprintf "Duplicate feature id %S in tabular input" id);
      Hashtbl.replace by_id id row;
      if parent = "." || parent = "" then List.accum roots id
      else begin
        let prev = try Hashtbl.find children parent with Not_found -> [] in
        Hashtbl.replace children parent (id :: prev)
      end) rows;
    (* An attributes row naming a feature that is not in the features table
       would attach to nothing and be dropped without a word. *)
    Hashtbl.iter (fun id _ ->
      if not (Hashtbl.mem by_id id) then
        Exception.raise __FUNCTION__ IO_Format
          (Printf.sprintf
             "The attributes table names feature %S, which the features table does not contain" id))
      attrs_of;
    (* Reconstruct DFS pre-order from the parent links.  Annotation.add wants
       each internal path segment to be the most recent node at the previous
       level, which is exactly what a depth-first walk of this forest gives it,
       whatever order the rows arrived in. *)
    let ordered = ref [] and reached = Hashtbl.create 64 in
    let rec walk parent_path id =
      (* A feature reached twice means the parent links are not a forest.  The
         completeness check below would not catch it on its own, since the count
         could still come out right. *)
      if Hashtbl.mem reached id then
        Exception.raise __FUNCTION__ IO_Format
          (Printf.sprintf "Feature %S is reachable twice: the parent links are not a forest" id);
      Hashtbl.replace reached id ();
      let row = Hashtbl.find by_id id in
      let path = path_of_field (field row 3 "features" 10) in
      (* The parent column and the path column describe one forest twice, and
         [Annotation.add] places a feature by its path alone.  If they disagree
         the file has no single meaning -- which of the two descriptions wins
         would depend on row order, and row order carrying no meaning is the
         whole point of the format. *)
      (match List.rev path with
       | _ :: rev_prefix when List.rev rev_prefix = parent_path -> ()
       | _ ->
         Exception.raise __FUNCTION__ IO_Format
           (Printf.sprintf
              "Feature %S has path %S, which does not sit directly below %S, the path of the \
               feature its parent column names"
              id (path_to_field path) (path_to_field parent_path)));
      let attrs =
        try List.rev (Hashtbl.find attrs_of id) with Not_found -> [] in
      let attr_map =
        List.fold_left (fun m (k, v) ->
          let kid = AttrKey.intern (attr_keys !ann) k in
          let existing = match AttrMap.find_opt kid m with Some a -> Array.to_list a | None -> [] in
          AttrMap.add kid
            (Array.of_list (existing @ [ ValueTable.intern (values !ann) v ])) m)
          AttrMap.empty attrs in
      let feature = {
        seq = Seq.intern (seqs !ann) (decode (field row 2 "features" 10));
        source =
          Option.map (ValueTable.intern (values !ann))
            (decode_opt (field row 5 "features" 10));
        intervals = intervals_of_field (field row 9 "features" 10);
        score = score_of_field (field row 6 "features" 10);
        strand = strand_of_field (field row 7 "features" 10);
        phase = phase_of_field (field row 8 "features" 10);
        id = decode_opt (field row 4 "features" 10);
        attributes = attr_map
      } in
      List.accum ordered (path, feature);
      match Hashtbl.find_opt children id with
      | None -> ()
      | Some kids -> List.iter (walk path) (List.rev kids) in
    List.iter (walk [ implicit_root_name ]) (List.rev !roots);
    (* Every row has to have been reached.  A row whose parent column names an
       id that is not in the table, and a cycle among the parent links, are both
       invisible to the walk -- it simply never arrives -- so without this check
       such a feature would be dropped in silence. *)
    Hashtbl.iter (fun id _ ->
      if not (Hashtbl.mem reached id) then
        Exception.raise __FUNCTION__ IO_Format
          (Printf.sprintf
             "Feature %S is unreachable: its parent is not in the features table, or the \
              parent links contain a cycle" id)) by_id;
    add_dfs_with_seq_bloom ann (List.rev !ordered);
    List.iter (fun (k, v) -> ann := add_metadata !ann ~key:k ~value:v) (List.rev !plain_metadata);
    (* The reference travels as FASTA beside the tables.  Load it with the
       identity linter: what was written is what the register held, and folding
       an IUPAC code to N on the way back in would corrupt what a round trip is
       supposed to preserve.  Sequences with no recorded table get the standard
       one, which is why only the others are written out. *)
    if reference_fasta <> "" then begin
      let base =
        match Annotation.reference !ann with
        | Some r -> r
        | None -> Sequences.Reference.empty in
      ann :=
        Annotation.set_reference !ann
          (Sequences.Reference.add_from_fasta_string ~linter:Fun.id ~tables:!tables base
             reference_fasta)
    end;
    cleanup_values !ann;
    !ann
  (* One document.  A [#] line opens a table and names it -- the three headers
     differ, so nothing beyond the header itself has to be agreed on -- and a
     [>] line opens the reference, which is plain FASTA and runs to the end
     unless another [#] header follows it.  A row before any header, or a header
     that names no known table, is an error rather than something to skip. *)
  let read ann_in s =
    let sections = Hashtbl.create 4 in
    let current = ref None and buf = Buffer.create 1024 in
    let flush () =
      Option.iter (fun name -> Hashtbl.replace sections name (Buffer.contents buf)) !current in
    let open_section name line =
      flush ();
      Buffer.clear buf;
      current := Some name;
      Buffer.add_string buf line;
      Buffer.add_char buf '\n' in
    List.iter (fun line ->
      if line = "" then ()
      else if line.[0] = '#' then begin
        let trimmed = String.trim line in
        if trimmed = features_header then open_section "features" line
        else if trimmed = attributes_header then open_section "attributes" line
        else if trimmed = metadata_header then open_section "metadata" line
        else
          Exception.raise __FUNCTION__ IO_Format
            (Printf.sprintf "Tabular input: %S names no known table" trimmed)
      end else if line.[0] = '>' && !current <> Some "reference" then begin
        flush ();
        Buffer.clear buf;
        current := Some "reference";
        Buffer.add_string buf line;
        Buffer.add_char buf '\n'
      end else begin
        if !current = None then
          Exception.raise __FUNCTION__ IO_Format
            (Printf.sprintf "Tabular input: row %S appears before any table header" line);
        Buffer.add_string buf line;
        Buffer.add_char buf '\n'
      end) (lines_of s);
    flush ();
    let section name =
      match Hashtbl.find_opt sections name with
      | Some s -> s
      | None ->
        Exception.raise __FUNCTION__ IO_Format
          (Printf.sprintf "Tabular input has no %S table" name) in
    read_tables ann_in ~features:(section "features") ~attributes:(section "attributes")
      ~metadata:(section "metadata")
      ~reference_fasta:(Option.value ~default:"" (Hashtbl.find_opt sections "reference"))
  let read_from_file ann prefix =
    if single_document prefix || looks_like_document prefix then read ann (read_file prefix)
    else
      read_tables ann
        ~features:(read_file (prefix ^ features_suffix))
        ~attributes:(read_file (prefix ^ attributes_suffix))
        ~metadata:(read_file (prefix ^ metadata_suffix))
        (* A register with no reference writes no FASTA, so its absence is
           ordinary rather than an error. *)
        ~reference_fasta:
          (if Sys.file_exists (prefix ^ reference_suffix) then read_file (prefix ^ reference_suffix)
           else "")
  let of_string ?(hierarchy = default_hierarchy) s = read (create hierarchy) s
  let of_file ?(hierarchy = default_hierarchy) prefix = read_from_file (create hierarchy) prefix
end

(* NCBI's submission feature table, the [.tbl] that table2asn consumes.  It is
   write-only by nature rather than by omission: it has no slot for GFF3's
   source column, no parent link and no annotation metadata, and table2asn
   INFERS the gene/mRNA/CDS relations from coordinate overlap and shared
   identifiers instead of reading them.  Inference is not encoding, so a
   register cannot be recovered from one, and the format therefore satisfies
   [Writer_t] rather than [Format_t].
   It is also a stanza format wearing a TSV costume -- a block header, coordinate
   lines, and tab-indented qualifier continuation lines -- so it is NOT the
   awk-per-record shape the tabular format above provides.  The two exist for
   different jobs. *)
module Tbl: Writer_t = struct
  let to_buffer buf ann =
    (* One [>Feature] block per sequence, its features in register order. *)
    let by_seq = Hashtbl.create 16 and order = ref [] in
    iter_paths (fun ~path feature ->
      let seq = seq_name ann feature in
      if not (Hashtbl.mem by_seq seq) then List.accum order seq;
      let prev = try Hashtbl.find by_seq seq with Not_found -> [] in
      Hashtbl.replace by_seq seq ((path, feature) :: prev)) ann;
    List.iter (fun seq ->
      Printf.bprintf buf ">Feature %s\n" seq;
      (try Hashtbl.find by_seq seq with Not_found -> [])
        |> List.rev
        |> List.iter (fun (path, feature) ->
          let category = match List.rev path with leaf :: _ -> leaf | [] -> "" in
          let reverse =
            match feature.strand with
            | Some (Sequences.Types.Reverse _) -> true
            | _ -> false in
          (* There is no strand column: the minus strand is spelled by giving a
             range stop-first.  Intervals also run 5' to 3' ALONG THE FEATURE
             rather than along the sequence, so a minus-strand feature lists
             them in the reverse of the genomic order they are stored in --
             which is also the correct reading of complement(join(A,B)), namely
             B' followed by A'. *)
          let ivs = if reverse then List.rev feature.intervals else feature.intervals in
          List.iteri (fun i (iv: Sequences.Types.simple_interval_t) ->
            if iv.length = 0 then
              Exception.raise __FUNCTION__ IO_Format
                (Printf.sprintf
                   "Feature %S on %S is zero-length, which a feature table has no way to spell"
                   category seq);
            let lo = iv.low + 1 and hi = iv.low + iv.length in
            let a, b = if reverse then hi, lo else lo, hi in
            if i = 0 then Printf.bprintf buf "%d\t%d\t%s\n" a b category
            else Printf.bprintf buf "%d\t%d\n" a b) ivs;
          (* Qualifiers sit on their own continuation lines, indented by three
             empty columns.  A valueless one simply omits the fifth. *)
          let has_codon_start = ref false in
          attr_iter ann (fun k vs ->
            if k = "codon_start" then has_codon_start := true;
            match vs with
            | [] -> Printf.bprintf buf "\t\t\t%s\n" k
            | _ ->
              List.iter (fun v ->
                if v = "" then Printf.bprintf buf "\t\t\t%s\n" k
                else Printf.bprintf buf "\t\t\t%s\t%s\n" k v) vs) feature;
          (* A phase that arrived in a GFF3 column rather than as a qualifier
             still has to travel, and /codon_start is the only slot for it.
             It is 1-based against the 0-based phase. *)
          (match feature.phase with
           | Some n when not !has_codon_start ->
             Printf.bprintf buf "\t\t\tcodon_start\t%d\n" (n + 1)
           | _ -> ()))) (List.rev !order)
  let to_string = to_string_via_buffer to_buffer
  let to_file = to_file_via_buffer to_buffer
end

(* A serialisable handle on the three formats, used by the
   [AnnoTools] CLI and by any caller that wants to dispatch on
   format at runtime.  The constructor names mirror the module
   names but live in their own namespace ([Format.GFF3] vs
   [GFF3]), so the two never clash. *)
module Format = struct
  type t = GFF3 | GTF | GenBank | Tabular
  let all = [ GFF3; GTF; GenBank; Tabular ]
  let module_of: t -> (module Format_t) = function
    | GFF3 -> (module GFF3)
    | GTF -> (module GTF)
    | GenBank -> (module GenBank)
    | Tabular -> (module Tabular)
  let to_string = function
    | GFF3 -> "gff3"
    | GTF -> "gtf"
    | GenBank -> "genbank"
    | Tabular -> "tsv"
  (* Match the canonical name (lower-cased) plus a small set of
     the informal spellings users tend to type on the command
     line. *)
  let of_string s =
    match String.lowercase_ascii s with
    | "gff3" | "gff" -> GFF3
    | "gtf" -> GTF
    | "genbank" | "gb" -> GenBank
    | "tsv" | "tabular" | "table" -> Tabular
    | _ ->
      Exception.raise __FUNCTION__ Initialize
        (Printf.sprintf "Unknown annotation format %S (have: %s)"
           s (String.concat ", " (List.map to_string all)))
  (* Resolve a dialect name against the format's
     [Format_t.dialects] association list, raising if the name
     is unknown.  Comparison is case-insensitive on the dialect
     key, which lets the CLI accept both [gencode] and
     [Gencode]. *)
  let dialect_of f name =
    let module F = (val module_of f) in
    let q = String.lowercase_ascii name in
    match List.find_opt
      (fun (k, _) -> String.lowercase_ascii k = q) F.dialects with
    | Some (_, h) -> h
    | None ->
      Exception.raise __FUNCTION__ Initialize
        (Printf.sprintf "Unknown dialect %S for format %s (have: %s)"
           name (to_string f)
           (String.concat ", " (List.map fst F.dialects)))
end

(* A serialisable handle on everything that can be WRITTEN, which is every
   format plus the write-only feature table.  Keeping it distinct from
   [Format.t] is what stops [--from-tbl] being expressible: the reading side of
   the CLI dispatches over [Format.t] and the writing side over this, so the
   type system records which formats can do which. *)
module Writer = struct
  type t =
    | Format of Format.t
    | Tbl
  let all = List.map (fun f -> Format f) Format.all @ [ Tbl ]
  let module_of: t -> (module Writer_t) = function
    | Format f ->
      (* [Format_t] includes [Writer_t], so a format's module is already a
         writer; this only narrows the packed signature. *)
      let module F = (val Format.module_of f) in
      (module F: Writer_t)
    | Tbl -> (module Tbl)
  let to_string = function
    | Format f -> Format.to_string f
    | Tbl -> "tbl"
  let of_string s =
    match String.lowercase_ascii s with
    | "tbl" | "feature-table" | "featuretable" -> Tbl
    | _ -> Format (Format.of_string s)
end

