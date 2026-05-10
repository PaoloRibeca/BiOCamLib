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
    val validate_sequences_present: t -> unit
    val validate_feature_bounds: t -> unit
    val validate_translation: t -> unit
  end
= struct
    include Annotations_Base.Annotation
    (* *)
    let archive_version = "2026-05-09"
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
    let require_reference ann who =
      match reference ann with
      | Some r -> r
      | None ->
        Exception.raise __FUNCTION__ Algorithm
          (Printf.sprintf "%s: no reference set on the annotation"
             who)
    let validate_sequences_present ann =
      let r = require_reference ann "validate_sequences_present" in
      let missing = ref StringSet.empty in
      iter (fun ~path:_ feature ->
        let name = seq_name ann feature in
        if name <> "" then
          (try
             let _ = Sequences.Reference.find r
               (Sequences.Types.Forward name) in ()
           with _ ->
             missing := StringSet.add name !missing)
      ) ann;
      if not (StringSet.is_empty !missing) then
        Exception.raise __FUNCTION__ Algorithm
          (Printf.sprintf
             "Annotation references sequence(s) not in reference: %s"
             (String.concat ", "
                (StringSet.elements !missing)))
    let validate_feature_bounds ann =
      let r = require_reference ann "validate_feature_bounds" in
      iter (fun ~path:_ feature ->
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
                Exception.raise __FUNCTION__ Algorithm
                  (Printf.sprintf
                     "Feature on %s [%d, %d) extends past sequence \
                      length %d"
                     name i.low (i.low + i.length) len)
            ) feature.intervals
        end
      ) ann
    let validate_translation ann =
      let r = require_reference ann "validate_translation" in
      let strip_trailing_stop s =
        let n = String.length s in
        if n > 0 && s.[n - 1] = '*'
        then String.sub s 0 (n - 1) else s in
      iter (fun ~path feature ->
        let leaf = Path.leaf_category (paths ann) path in
        if leaf = "CDS" then
          match attr_get ann feature "translation" with
          | None | Some [] -> ()
          | Some (claimed :: _) ->
            let name = seq_name ann feature in
            (* Stitch joined intervals on the forward strand,
               reverse-complement if the feature is on the
               minus strand, then drop [phase] bases at the
               5' end before translating. *)
            let stitched =
              List.map (fun (i : Sequences.Types.simple_interval_t) ->
                let str_iv : Sequences.Types.stranded_interval_t = {
                  low = {
                    name = Sequences.Types.Forward name;
                    position = i.low
                  };
                  length = i.length
                } in
                Sequences.Reference.get_sequence r str_iv
              ) feature.intervals
              |> String.concat "" in
            let stitched =
              match feature.strand with
              | Some Sequences.Types.Reverse _ ->
                Sequences.Lint.rc stitched
              | _ -> stitched in
            let phase =
              match feature.phase with Some n -> n | None -> 0 in
            let coding =
              if phase >= String.length stitched then ""
              else String.sub stitched phase
                     (String.length stitched - phase) in
            (* Pick the per-feature translation table from the
               GenBank /transl_table qualifier when present;
               fall back to the per-sequence default. *)
            let table =
              match attr_get ann feature "transl_table" with
              | Some (n :: _) ->
                (try Sequences.Translation.of_string n
                 with _ ->
                   try Sequences.Reference.get_table r {
                     low = {
                       name = Sequences.Types.Forward name;
                       position = 0 };
                     length = 1 }
                   with _ -> Sequences.Translation.Table_1)
              | _ ->
                try Sequences.Reference.get_table r {
                  low = {
                    name = Sequences.Types.Forward name;
                    position = 0 };
                  length = 1 }
                with _ -> Sequences.Translation.Table_1 in
            let products =
              Sequences.Translation.get_translations
                ~replace_alternative_start_codons_with_methionine:true
                ~only_largest_product:true ~min_length:0
                table coding in
            let computed =
              if Array.length products = 0 then ""
              else
                let _, p = products.(0) in
                strip_trailing_stop p in
            let claimed = strip_trailing_stop claimed in
            if computed <> claimed then
              Exception.raise __FUNCTION__ Algorithm
                (Printf.sprintf
                   "CDS on %s [phase=%d, intervals=%d]: claimed \
                    translation does not match computed."
                   name phase (List.length feature.intervals))
      ) ann
  end

open Annotation

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

(* GFF3/GTF ranges are 1-based inclusive in the source; the
   AST stores 0-based half-open. *)
let interval_of_1_based ~lo ~hi : Sequences.Types.simple_interval_t =
  if hi < lo then
    Exception.raise __FUNCTION__ IO_Format
      (Printf.sprintf "Invalid interval (lo=%d, hi=%d)" lo hi);
  { low = lo - 1; length = hi - lo + 1 }

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
          [ seq, mk_simple (e.pos - 1) 1 ], strand
        | Range (a, b) ->
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
          let acc = ref [] and st = ref strand in
          List.iter (fun p ->
            let pieces, s = walk strand seq p in
            st := s;
            acc := !acc @ pieces) parts;
          !acc, !st
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
let iter_tsv_lines ?(pragma = fun _ -> ()) ~data s =
  let lines = String.split_on_char '\n' s |> Array.of_list in
  Array.iteri (fun i raw ->
    let lnum = i + 1 in
    let line =
      let n = String.length raw in
      if n > 0 && raw.[n - 1] = '\r'
      then String.sub raw 0 (n - 1) else raw in
    if line = "" then ()
    else if String.length line >= 2 && String.sub line 0 2 = "##"
    then pragma (String.sub line 2 (String.length line - 2))
    else if line.[0] = '#' then ()
    else
      let fields =
        String.split_on_char '\t' line |> Array.of_list in
      data lnum fields
  ) lines

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

(* Walk every attribute pair on [feature] in source order,
   resolving each value array back to its [string list] form.
   Format-specific writers thread the result through their own
   per-pair formatters. *)
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
module type Format_t = sig
  val dialects: (string * Hierarchy.t) list
  val default_hierarchy: Hierarchy.t
  val read: Annotation.t -> string -> Annotation.t
  val read_from_file: Annotation.t -> string -> Annotation.t
  val of_string: ?hierarchy:Hierarchy.t -> string -> Annotation.t
  val of_file: ?hierarchy:Hierarchy.t -> string -> Annotation.t
  val to_buffer: Buffer.t -> Annotation.t -> unit
  val to_string: Annotation.t -> string
  val to_file: Annotation.t -> string -> unit
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
      strand;
      phase;
      id;
      attributes = attr_map
    } in
    { row_id = id; row_parent = parent; row_type = ftype; row_feature = feature }
  let read_rows ~seqs ~attr_keys ~values s =
    let pragmas = ref [] and rows = ref [] in
    iter_tsv_lines s
      ~pragma:(fun body -> List.accum pragmas body)
      ~data:(fun lnum fields ->
        List.accum rows
          (lnum, parse_row ~seqs ~attr_keys ~values lnum fields));
    List.rev !pragmas, List.rev !rows
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
    let pragmas, rows =
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
  let attribute_string ann feature =
    attribute_pairs ann feature
    |> List.map (fun (k, vs) -> k ^ "=" ^ String.concat "," vs)
    |> String.concat ";"
  let row_of_feature ann path feature =
    let ftype = match List.rev path with [] -> "" | x :: _ -> x in
    let seq = seq_name ann feature
    and src =
      match feature_source ann feature with
      | Some s -> s | None -> "."
    and score = "."
    and strand =
      match feature.strand with
      | Some Sequences.Types.Forward _ -> "+"
      | Some Sequences.Types.Reverse _ -> "-"
      | None -> "."
    and phase =
      match feature.phase with
      | None -> "." | Some n -> string_of_int n
    and attrs = attribute_string ann feature in
    List.map (fun (ivl : Sequences.Types.simple_interval_t) ->
      let lo = ivl.low + 1
      and hi = ivl.low + ivl.length in
      Printf.sprintf "%s\t%s\t%s\t%d\t%d\t%s\t%s\t%s\t%s"
        seq src ftype lo hi score strand phase attrs
    ) feature.intervals
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
    ) ann
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
    let phase =
      match feature.phase with
      | None -> "." | Some n -> string_of_int n in
    let seq = seq_name ann feature in
    let src =
      match feature_source ann feature with
      | Some s -> s | None -> "BiOCamLib" in
    List.map (fun (i : Sequences.Types.simple_interval_t) ->
      let lo = i.low + 1 and hi = i.low + i.length in
      Printf.sprintf
        "%s\t%s\t%s\t%d\t%d\t.\t%s\t%s\t%s;"
        seq src leaf lo hi strand phase
        (attribute_string ann feature)
    ) feature.intervals
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
    (* Aggregate qualifier repeats into per-key lists, then
       freeze to [Value.t array] so each value is interned. *)
    let acc_lists =
      List.fold_left (fun m (k, v) ->
        let prev = try StringMap.find k m with Not_found -> [] in
        StringMap.add k (prev @ [v]) m
      ) StringMap.empty f.qualifiers in
    let attrs =
      StringMap.fold (fun k vs m ->
        let kid = AttrKey.intern attr_keys k in
        let arr =
          Array.of_list
            (List.map (ValueTable.intern values) vs) in
        AttrMap.add kid arr m
      ) acc_lists AttrMap.empty in
    let lookup_str_attr name =
      match StringMap.find_opt name acc_lists with
      | Some (v :: _) -> Some v
      | _ -> None in
    let id =
      match lookup_str_attr "locus_tag" with
      | Some _ as r -> r
      | None -> lookup_str_attr "gene" in
    let feature = {
      seq = Seq.intern seqs seq_name;
      source = None;
      intervals;
      strand;
      phase = None;
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
        if Hierarchy.validate hierarchy ~path:source_path then begin
          let source_iv : Sequences.Types.simple_interval_t = {
            low = 0;
            length = seq_length
          } in
          let source_feature = {
            seq = Seq.intern (seqs !ann) locus;
            source = None;
            intervals = [ source_iv ];
            strand = None;
            phase = None;
            id = Some locus;
            attributes = AttrMap.empty
          } in
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
        let lo = i.low + 1 and hi = i.low + i.length in
        Printf.sprintf "%d..%d" lo hi
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
      StringMap.iter (fun k vs ->
        List.iter (fun v ->
          Printf.bprintf buf "%-12s%s\n" k v
        ) vs
      ) (all_metadata ann);
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

(* A serialisable handle on the three formats, used by the
   [Annotools] CLI and by any caller that wants to dispatch on
   format at runtime.  The constructor names mirror the module
   names but live in their own namespace ([Format.GFF3] vs
   [GFF3]), so the two never clash. *)
module Format = struct
  type t = GFF3 | GTF | GenBank
  let all = [ GFF3; GTF; GenBank ]
  let module_of: t -> (module Format_t) = function
    | GFF3 -> (module GFF3)
    | GTF -> (module GTF)
    | GenBank -> (module GenBank)
  let to_string = function
    | GFF3 -> "gff3"
    | GTF -> "gtf"
    | GenBank -> "genbank"
  (* Match the canonical name (lower-cased) plus a small set of
     the informal spellings users tend to type on the command
     line. *)
  let of_string s =
    match String.lowercase_ascii s with
    | "gff3" | "gff" -> GFF3
    | "gtf" -> GTF
    | "genbank" | "gb" -> GenBank
    | _ ->
      Exception.raise __FUNCTION__ Algorithm
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
      Exception.raise __FUNCTION__ Algorithm
        (Printf.sprintf "Unknown dialect %S for format %s (have: %s)"
           name (to_string f)
           (String.concat ", " (List.map fst F.dialects)))
end

