(*
    AnnoTools.ml -- (c) 2026 Paolo Ribeca, <paolo.ribeca@gmail.com>.

    AnnoTools manipulates a single in-memory annotation register
    via a CLI-driven action stream, mirroring the structure of
    KPopCountDB / KPopTwistDB.  Supported formats: GFF3, GTF,
    GenBank.  References are loaded from multi-FASTA.  Validation
    actions check that the loaded sequences and annotation are
    self-consistent.

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

open BiOCamLib
open Better

module A = Annotations

module Mode = struct
  type t = Replace | Add
  let of_string = function
    | "replace" | "REPLACE" -> Replace
    | "add" | "ADD" -> Add
    | s ->
      Exception.raise __FUNCTION__ Initialize
        (Printf.sprintf "Unknown mode %S (expected replace|add)" s)
end

module Sequence_kind = struct
  type t = DNA | Protein
  let of_string = function
    | "dna" | "DNA" | "nucleotide" -> DNA
    | "protein" | "PROTEIN" | "aa" -> Protein
    | s ->
      Exception.raise_unrecognized_initializer __FUNCTION__ "sequence kind" s
  let to_string = function
    | DNA -> "dna"
    | Protein -> "protein"
end

type to_do_t =
  | Empty
  | Of_binary of string
  | To_binary of string
  | Annotation_op of Mode.t * A.Format.t * string
  | Reference_op of Mode.t * string
  | Set_hierarchy of A.Format.t * string
  | To_format of A.Writer.t * string
  | Validate_sequences_present
  | Validate_feature_bounds
  | Validate_translation
  | Validate_all
  | Validate_report of string
  | Selection_from_labels of StringSet.t
  | Selection_from_regexps of (string * Str.regexp) list
  | Selection_negate
  | Selection_print
  | Selection_clear
  | Extract of Sequence_kind.t * string
  | Summary

module Defaults = struct
  let verbose = false
end

module Parameters = struct
  let program = ref []
  let verbose = ref Defaults.verbose
end

let info = {
  Tools.Argv.name = "AnnoTools";
  version = "3";
  date = "21-Aug-2026"
} and authors = [
  "2026", "Paolo Ribeca", "paolo.ribeca@gmail.com"
]

let () =
  let module TA = Tools.Argv in
  try
    TA.set_header (info, authors, [ Info.info ]);
    TA.set_synopsis "[ACTIONS]";
    (* Same selector syntax as KPopCountDB: comma-separated <field>~<regexp>
       criteria, all of which must match. *)
    let parse_regexp_selector option s =
      List.map (fun l ->
        let res = String.Split.on_char_as_list '~' l in
        if List.length res <> 2 then begin
          TA.usage ();
          List.length res
            |> Printf.sprintf
                 "Option '%s': Wrong number of fields in criterion (expected 2, found %d)"
                 option
            |> TA.parse_error (* parse_error exits the program *)
        end;
        List.nth res 0, List.nth res 1 |> Str.regexp)
        (String.Split.on_char_as_list ',' s) in
    (* The tabular format carries the hierarchy it was written under, so pinning
       one for it would either be ignored or would install a default over a file
       that brought its own.  Refuse rather than accept a switch that does
       nothing. *)
    let overridable option fmt =
      if fmt = A.Format.Tabular then begin
        TA.usage ();
        Printf.sprintf
          "Option '%s': format '%s' carries its own hierarchy, so an override is meaningless"
          option (A.Format.to_string fmt)
        |> TA.parse_error
      end;
      fmt in
    TA.parse [
      TA.make_separator_multiline
        [ "Actions.";
          "They are executed delayed and in order of specification." ];
      TA.make_separator_multiline
        [ "";
          "Operations on the annotation register:" ];
      [ "-0"; "--empty" ],
        None,
        [ "load an empty annotation into the register" ],
        TA.Optional,
        (fun _ -> Empty |> List.accum Parameters.program);
      [ "-i"; "--input" ],
        Some "<binary_file_prefix>",
        [ "load into the register the annotation present in the";
          " specified binary file (extension '.Annotation' is";
          " appended unless the path is under '/dev/*')" ],
        TA.Optional,
        (fun _ ->
          Of_binary (TA.get_parameter ()) |> List.accum Parameters.program);
      [ "-o"; "--output" ],
        Some "<binary_file_prefix>",
        [ "write the current register to the specified binary file";
          " (extension '.Annotation' is appended unless under '/dev/*')" ],
        TA.Optional,
        (fun _ ->
          To_binary (TA.get_parameter ()) |> List.accum Parameters.program);
      TA.make_separator_multiline
        [ "";
          "Hierarchy.";
          "Override the active hierarchy for a given format.  The";
          "override is sticky: every subsequent input operation in";
          "that format uses it until another '--hierarchy' or";
          "'--dialect' replaces it.  Reverting to the format's";
          "default is just '--dialect <fmt> standard'." ];
      [ "--hierarchy" ],
        Some "<gff3|gtf|genbank> <S-expression>",
        [ "set the hierarchy to use for subsequent input operations";
          " in the named format" ],
        TA.Optional,
        (fun _ ->
          let fmt = TA.get_parameter () |> A.Format.of_string |> overridable "--hierarchy" in
          let s = TA.get_parameter () in
          Set_hierarchy (fmt, s) |> List.accum Parameters.program);
      [ "--dialect" ],
        Some "<gff3|gtf|genbank> <name>",
        [ "switch subsequent input operations in the named format";
          " to one of its built-in dialects.  Currently only GFF3";
          " ships more than one dialect ('standard' and";
          " 'gencode')." ],
        TA.Optional,
        (fun _ ->
          let fmt = TA.get_parameter () |> A.Format.of_string |> overridable "--dialect" in
          let h = TA.get_parameter () |> A.Format.dialect_of fmt in
          Set_hierarchy (fmt, A.Hierarchy.to_string h)
          |> List.accum Parameters.program);
      TA.make_separator_multiline
        [ "";
          "Annotation input.";
          "Long form: action mode + format + path.";
          "Short forms: '--from-gff3', '--from-gtf', '--from-genbank'";
          "default to 'replace'." ];
      [ "-a"; "--annotation" ],
        Some "<replace|add> <gff3|gtf|genbank|tsv> <file_or_prefix>",
        [ "merge or replace the register from the named format.";
          "Every format but 'tsv' takes a FILE; 'tsv' takes a";
          "PREFIX, since it is a collection of files.";
          "When the format is GenBank and the input carries an";
          "ORIGIN section, or GFF3 and it carries a '##FASTA'";
          "section, the reference sequence is replaced as well." ],
        TA.Optional,
        (fun _ ->
          let mode = TA.get_parameter () |> Mode.of_string in
          let fmt = TA.get_parameter () |> A.Format.of_string in
          let p = TA.get_parameter () in
          Annotation_op (mode, fmt, p) |> List.accum Parameters.program);
      [ "--from-gff3" ],
        Some "<file>",
        [ "shorthand for '--annotation replace gff3 <file>'" ],
        TA.Optional,
        (fun _ ->
          Annotation_op (Mode.Replace, A.Format.GFF3, TA.get_parameter ())
          |> List.accum Parameters.program);
      [ "--from-gtf" ],
        Some "<file>",
        [ "shorthand for '--annotation replace gtf <file>'" ],
        TA.Optional,
        (fun _ ->
          Annotation_op (Mode.Replace, A.Format.GTF, TA.get_parameter ())
          |> List.accum Parameters.program);
      [ "--from-tsv"; "--from-tabular" ],
        Some "<prefix>",
        [ "shorthand for '--annotation replace tsv <prefix>'.";
          "Reads the '.AnnotationFeatures.txt',";
          "'.AnnotationAttributes.txt' and '.AnnotationMetadata.txt'";
          "tables written from that prefix, together with";
          "'.AnnotationReference.fasta' when one is beside them.";
          "A path under '/dev/*', or an ordinary file that turns";
          "out to be a whole tabular document, is read as one";
          "document instead" ],
        TA.Optional,
        (fun _ ->
          Annotation_op (Mode.Replace, A.Format.Tabular, TA.get_parameter ())
          |> List.accum Parameters.program);
      [ "--from-genbank" ],
        Some "<file>",
        [ "shorthand for '--annotation replace genbank <file>'" ],
        TA.Optional,
        (fun _ ->
          Annotation_op (Mode.Replace, A.Format.GenBank, TA.get_parameter ())
          |> List.accum Parameters.program);
      TA.make_separator_multiline
        [ "";
          "Reference (multi-FASTA) input.";
          "Long form takes the same mode keyword as --annotation.";
          "Short form '--from-fasta' defaults to 'replace'." ];
      [ "-r"; "--reference" ],
        Some "<replace|add> <file>",
        [ "merge or replace the register's reference from <file>" ],
        TA.Optional,
        (fun _ ->
          let mode = TA.get_parameter () |> Mode.of_string in
          let p = TA.get_parameter () in
          Reference_op (mode, p) |> List.accum Parameters.program);
      [ "--from-fasta" ],
        Some "<file>",
        [ "shorthand for '--reference replace <file>'" ],
        TA.Optional,
        (fun _ ->
          Reference_op (Mode.Replace, TA.get_parameter ())
          |> List.accum Parameters.program);
      TA.make_separator_multiline
        [ "";
          "Validation.";
          "Each check stops at the first violation, exits non-zero,";
          "and points the user at '--validate-report <file>' for the";
          "full list.  All require a reference to be set." ];
      [ "--validate-sequences-present" ],
        None,
        [ "every sequence referenced by an annotation feature must";
          " also exist in the reference" ],
        TA.Optional,
        (fun _ ->
          Validate_sequences_present
          |> List.accum Parameters.program);
      [ "--validate-feature-bounds" ],
        None,
        [ "every feature interval must lie within the corresponding";
          " sequence's length" ],
        TA.Optional,
        (fun _ ->
          Validate_feature_bounds |> List.accum Parameters.program);
      [ "--validate-translation" ],
        None,
        [ "translated CDS features must agree with their";
          " /translation= qualifier (currently a structural";
          " sub-check; codon-by-codon comparison is a follow-up)" ],
        TA.Optional,
        (fun _ ->
          Validate_translation |> List.accum Parameters.program);
      [ "--validate" ],
        None,
        [ "run every validation in turn" ],
        TA.Optional,
        (fun _ -> Validate_all |> List.accum Parameters.program);
      [ "--validate-report" ],
        Some "<file>",
        [ "run every validation against the current register but";
          " do not stop at the first violation: walk the whole";
          " register, write a tab-separated report with one row";
          " per violation (columns: check, path, feature_id,";
          " message) to <file>, and exit non-zero if any violation";
          " was found." ],
        TA.Optional,
        (fun _ ->
          Validate_report (TA.get_parameter ())
          |> List.accum Parameters.program);
      TA.make_separator_multiline
        [ "";
          "Actions involving the selection register.";
          "The selection restricts '--selection-print' and the";
          "'--extract-*' actions to the features it matches.  The";
          "'--to-*' writers and '-o' always write the whole register,";
          "because a feature whose parent is not selected would be";
          "emitted without it.  The selection is sticky, and starts";
          "out matching everything.";
          "";
          "To pull every mature peptide out of a GenBank record:";
          "  AnnoTools --from-genbank in.gb \\";
          "            -R 'type~^mat_peptide$' \\";
          "            --extract-protein peptides.faa";
          "Add '-v' to see how many features each selection matched." ];
      [ "-L"; "--labels"; "--selection-from-labels" ],
        Some "<feature_id>[','...','<feature_id>]",
        [ "put into the selection register the features carrying the";
          "given identifiers.  The match is EXACT, not a regexp: for";
          "patterns use '-R' with the 'id' field.";
          "";
          "A feature's identifier comes from its source format:";
          "  GFF3     the 'ID=' attribute";
          "  GenBank  '/locus_tag', or '/gene' when there is none";
          "  GTF      only the gene and transcript levels, from";
          "           'gene_id' and 'transcript_id'";
          "";
          "Many features have NO identifier -- a GenBank mat_peptide,";
          "and every row of a GTF file, since there only the";
          "synthesised gene and transcript parents get one.  '-L'";
          "can never match those; select them with '-R' on 'type' or";
          "'path' instead.";
          "";
          "'--selection-print' lists them, which is how to find out";
          "what to pass.  Its first column is the identifier when the";
          "feature has one and a positional stand-in of the form";
          "'<seq>:<type>:<location>' when it does not -- the latter is";
          "a label, not an identifier, and '-L' will not match it.";
          "";
          "Examples:";
          "  -L b0011              one feature, by locus tag";
          "  -L b0011,b0012,b0013  three of them";
          "  -L ENSG00000141510    a GFF3 feature by its ID=" ],
        TA.Optional,
        (fun _ ->
          Selection_from_labels
            (TA.get_parameter () |> String.Split.on_char_as_list ',' |> StringSet.of_list)
          |> List.accum Parameters.program);
      [ "-R"; "--regexps"; "--selection-from-regexps" ],
        Some "<field>'~'<regexp>[','...','<field>'~'<regexp>]",
        [ "put into the selection register the features whose named";
          "fields match the given regexps.  Criteria separated by ','";
          "must ALL match.";
          "";
          "<field> is one of:";
          "  type    the feature's own category, e.g. CDS, mat_peptide";
          "  path    its whole category chain, e.g. source->CDS";
          "  seq     the sequence it lies on";
          "  strand  '+', '-' or '.'";
          "  id      its identifier ('label', and the empty field";
          "          name, are synonyms)";
          "  source  the provenance in GFF3 column 2";
          "Any other name is read as an ATTRIBUTE, matching when any";
          "one of that attribute's values does -- so 'gene~dnaA'";
          "selects on the /gene qualifier.  Those seven names are";
          "therefore reserved: an attribute sharing one of them";
          "cannot be selected on.";
          "";
          "<regexp> is UNANCHORED, so 'type~gene' also matches";
          "'pseudogene'.  Anchor it with '^...$' when that matters.";
          "";
          "Examples:";
          "  -R 'type~^mat_peptide$'    every mature peptide";
          "  -R 'type~^CDS$,gene~^thr'  CDSs whose /gene starts 'thr'";
          "  -R 'seq~^chr1$'            everything on chr1";
          "  -R '~b0011'                the feature whose id is b0011" ],
        TA.Optional,
        (fun _ ->
          Selection_from_regexps (TA.get_parameter () |> parse_regexp_selector "-R")
          |> List.accum Parameters.program);
      [ "--selection-negate" ],
        None,
        [ "negate the current selection" ],
        TA.Optional,
        (fun _ -> Selection_negate |> List.accum Parameters.program);
      [ "--selection-print" ],
        None,
        [ "print the features currently selected, one per line, to";
          " standard output" ],
        TA.Optional,
        (fun _ -> Selection_print |> List.accum Parameters.program);
      [ "--selection-clear" ],
        None,
        [ "reset the selection register so that it matches everything" ],
        TA.Optional,
        (fun _ -> Selection_clear |> List.accum Parameters.program);
      TA.make_separator_multiline
        [ "";
          "Sequence extraction.";
          "Emit the sequence denoted by each selected feature as";
          "FASTA.  A feature's intervals are spliced in the order";
          "they are stored and the result is reverse-complemented";
          "when the feature is on the minus strand; a protein is that";
          "sequence with the phase bases dropped from its 5' end,";
          "translated with the feature's '/transl_table' when it";
          "carries one.  Requires a reference to have been loaded." ];
      [ "--extract" ],
        Some "<dna|protein> <file>",
        [ "write the sequence of every selected feature to <file>" ],
        TA.Optional,
        (fun _ ->
          let kind = TA.get_parameter () |> Sequence_kind.of_string in
          let path = TA.get_parameter () in
          Extract (kind, path) |> List.accum Parameters.program);
      [ "--extract-dna" ],
        Some "<file>",
        [ "shorthand for '--extract dna <file>'" ],
        TA.Optional,
        (fun _ ->
          Extract (Sequence_kind.DNA, TA.get_parameter ())
          |> List.accum Parameters.program);
      [ "--extract-protein" ],
        Some "<file>",
        [ "shorthand for '--extract protein <file>'" ],
        TA.Optional,
        (fun _ ->
          Extract (Sequence_kind.Protein, TA.get_parameter ())
          |> List.accum Parameters.program);
      [ "--summary" ],
        None,
        [ "print a one-line summary of the current register to stderr" ],
        TA.Optional,
        (fun _ -> Summary |> List.accum Parameters.program);
      TA.make_separator_multiline
        [ "";
          "Annotation output." ];
      [ "--to" ],
        Some "<gff3|gtf|genbank|tsv|tbl> <file_or_prefix>",
        [ "write the register in the named format.  Every format but";
          "'tsv' takes a FILE; 'tsv' takes a PREFIX, since it writes";
          "a collection of files.";
          "'tbl' is NCBI's submission feature table, which is";
          "write-only: it encodes no hierarchy and no metadata, so";
          "nothing can be read back from it" ],
        TA.Optional,
        (fun _ ->
          let fmt = TA.get_parameter () |> A.Writer.of_string in
          let p = TA.get_parameter () in
          To_format (fmt, p) |> List.accum Parameters.program);
      [ "--to-gff3" ],
        Some "<file>",
        [ "shorthand for '--to gff3 <file>'" ],
        TA.Optional,
        (fun _ ->
          To_format (A.Writer.Format A.Format.GFF3, TA.get_parameter ())
          |> List.accum Parameters.program);
      [ "--to-gtf" ],
        Some "<file>",
        [ "shorthand for '--to gtf <file>'" ],
        TA.Optional,
        (fun _ ->
          To_format (A.Writer.Format A.Format.GTF, TA.get_parameter ())
          |> List.accum Parameters.program);
      [ "--to-tsv"; "--to-tabular" ],
        Some "<prefix>",
        [ "shorthand for '--to tsv <prefix>'.  Writes a COLLECTION";
          "of files: the '.AnnotationFeatures.txt',";
          "'.AnnotationAttributes.txt' and '.AnnotationMetadata.txt'";
          "tables, plus '.AnnotationReference.fasta' when the";
          "register carries a sequence.";
          "A prefix under '/dev/*' writes all of it to that one path";
          "instead, as a single document" ],
        TA.Optional,
        (fun _ ->
          To_format (A.Writer.Format A.Format.Tabular, TA.get_parameter ())
          |> List.accum Parameters.program);
      [ "--to-tbl"; "--to-feature-table" ],
        Some "<file>",
        [ "shorthand for '--to tbl <file>'" ],
        TA.Optional,
        (fun _ ->
          To_format (A.Writer.Tbl, TA.get_parameter ())
          |> List.accum Parameters.program);
      [ "--to-genbank" ],
        Some "<file>",
        [ "shorthand for '--to genbank <file>'" ],
        TA.Optional,
        (fun _ ->
          To_format (A.Writer.Format A.Format.GenBank, TA.get_parameter ())
          |> List.accum Parameters.program);
      TA.make_separator_multiline
        [ "Miscellaneous options."; "They are set immediately." ];
      [ "--fasta-width" ],
        Some "<non_negative_integer>",
        [ "wrap sequence lines at this width wherever FASTA is emitted";
          " ('--to-gff3', '--to-tsv' and the '--extract-*' actions),";
          " with '0' meaning one line per sequence.  Without this option";
          " each format keeps its own convention: GFF3 wraps its";
          " '##FASTA' section at 60, while the tabular format and the";
          " extraction actions emit one line per sequence, so that every";
          " line of their output is a whole record" ],
        TA.Default (Fun.const "each format's own convention"),
        (fun _ -> A.set_fasta_width (Some (TA.get_parameter_int_non_neg ())));
      [ "-v"; "--verbose" ],
        None,
        [ "set verbose execution" ],
        TA.Default (Fun.const "quiet execution"),
        (fun _ -> Parameters.verbose := true);
      [ "-V"; "--version" ],
        None,
        [ "print version and exit" ],
        TA.Optional,
        (fun _ -> Printf.printf "%s\n%!" info.version; exit 0);
      [ "--markdown" ], None, [], TA.Optional,
        (fun _ -> TA.markdown (); exit 0);
      [ "-x"; "--print-exception-backtrace" ], None, [], TA.Optional,
        (fun _ -> Printexc.record_backtrace true);
      [ "-h"; "--help" ],
        None,
        [ "print syntax and exit" ],
        TA.Optional,
        (fun _ -> TA.usage (); exit 0)
    ];
    let program = List.rev !Parameters.program in
    if program = [] then begin
      TA.usage ();
      exit 0
    end;
    if !Parameters.verbose then
      TA.header ();
    (* Register: empty annotation under the GFF3 default
       hierarchy.  The hierarchy is replaced wholesale on every
       replace-style read, so the initial choice is moot.
       Per-format hierarchy overrides live in a single
       [Hashtbl] keyed by [A.Format.t]; an entry stays in place
       until '--hierarchy' or '--dialect' replaces it, so a
       single command-line directive applies to every later
       input operation in that format. *)
    let current = ref (A.Annotation.create A.GFF3.default_hierarchy) in
    let hierarchy_overrides = Hashtbl.create 8 in
    let hierarchy_of fmt =
      match Hashtbl.find_opt hierarchy_overrides fmt with
      | Some h -> h
      | None ->
        let module F = (val A.Format.module_of fmt) in
        F.default_hierarchy in
    let read_format mode fmt path =
      let module F = (val A.Format.module_of fmt) in
      let target =
        match mode with
        | Mode.Replace -> A.Annotation.create (hierarchy_of fmt)
        | Mode.Add -> !current in
      current := F.read_from_file target path
    in
    let read_reference mode path =
      let base =
        match mode with
        | Mode.Replace -> Sequences.Reference.empty
        | Mode.Add ->
          (match A.Annotation.reference !current with
           | Some r -> r
           | None -> Sequences.Reference.empty) in
      (* Upper-case, and change nothing else.  The default linter is
         [Lint.dnaize], which folds every non-ACGT byte to N -- so an IUPAC
         ambiguity code in the reference would be destroyed before any feature
         sequence was ever read out of it.  Case still has to be normalised,
         because the codon tables are upper-case only and a soft-masked genome
         would otherwise translate to X throughout. *)
      let r = Sequences.Reference.add_from_fasta ~linter:String.uppercase_ascii base path in
      current := A.Annotation.set_reference !current r in
    let summary () =
      let n_feat = ref 0 in
      A.Annotation.iter (fun ~path:_ _ -> incr n_feat) !current;
      Printf.eprintf
        "(%s): hierarchy_root=%s features=%d distinct_paths=%d \
         reference=%s\n%!"
        info.Tools.Argv.name
        (A.Hierarchy.name (A.Annotation.hierarchy !current))
        !n_feat
        (A.Path.Table.cardinal (A.Annotation.paths !current))
        (match A.Annotation.reference !current with
         | None -> "(none)"
         | Some _ -> "(loaded)") in
    let selection = ref A.Selection.All in
    (* 1-based inclusive and comma-joined, the spelling GenBank uses for a
       LOCATION; a zero-length site is written lo^hi rather than as the reversed
       range a naive lo+1..lo+length would produce. *)
    let location_of feature =
      List.map (fun i -> A.OneBased.(of_interval i |> to_string))
        feature.A.Annotation.intervals
      |> String.concat "," in
    (* A feature need not carry an id: GenBank derives one from /locus_tag when
       there is one and has none otherwise.  Fall back to sequence, category and
       span, which separates everything except two features of the same category
       occupying the same span on the same sequence. *)
    let name_of ann ~path feature =
      match feature.A.Annotation.id with
      | Some id when id <> "" -> id
      | _ ->
        Printf.sprintf "%s:%s:%s" (A.Annotation.seq_name ann feature)
          (match List.rev path with leaf :: _ -> leaf | [] -> "")
          (location_of feature) in
    let iter_selected f = A.Selection.iter !current !selection f in
    (* Under -v, say what the selection register now holds every time it
       changes.  A selector is easy to get subtly wrong -- an unanchored regexp,
       a field name that is silently read as an attribute -- and the first sign
       is usually an output that is empty or far too large, by which point the
       action that produced it has already run. *)
    let report_selection () =
      if !Parameters.verbose then begin
        let matched = A.Selection.count !current !selection
        and total = A.Selection.count !current A.Selection.All in
        Printf.eprintf "(%s): selection matches %d of %d %s (%s)\n%!" info.Tools.Argv.name
          matched total (String.pluralize_int "feature" total)
          (A.Selection.to_string !selection)
      end in
    List.iter (function
      | Empty ->
        current := A.Annotation.create A.GFF3.default_hierarchy
      | Of_binary prefix ->
        current := A.Annotation.of_binary ~verbose:!Parameters.verbose prefix
      | To_binary prefix ->
        Exception.catch_unexpected_end_of_output __FUNCTION__
          (fun () ->
            A.Annotation.to_binary ~verbose:!Parameters.verbose !current prefix)
      | Annotation_op (mode, fmt, path) ->
        read_format mode fmt path;
        (* The selection is a criterion, not a fixed set, so what it matches
           moves when the register does. *)
        if !selection <> A.Selection.All then report_selection ()
      | Reference_op (mode, path) ->
        read_reference mode path
      | Set_hierarchy (fmt, s) ->
        Hashtbl.replace hierarchy_overrides fmt
          (A.Hierarchy.of_string s)
      | To_format (fmt, path) ->
        Exception.catch_unexpected_end_of_output __FUNCTION__
          (fun () ->
            let module F = (val A.Writer.module_of fmt) in
            F.to_file !current path)
      | Validate_sequences_present ->
        A.Annotation.validate_sequences_present !current
      | Validate_feature_bounds ->
        A.Annotation.validate_feature_bounds !current
      | Validate_translation ->
        A.Annotation.validate_translation !current
      | Validate_all ->
        A.Annotation.validate_sequences_present !current;
        A.Annotation.validate_feature_bounds !current;
        A.Annotation.validate_translation !current
      | Validate_report path ->
        (* Run all three checks against the register with a
           non-raising callback that writes one TSV row per
           violation to [path].  Count violations as we go and
           report the total to stderr at the end; exit non-zero
           if any was found. *)
        let oc = open_out path in
        (* Same header convention as every other tabular output here. *)
        output_string oc "#check\t#path\t#feature_id\t#message\n";
        let total = ref 0 in
        let mk_callback check =
          fun ~path:p ~feature_id:fid ~message:m ->
            incr total;
            Printf.fprintf oc "%s\t%s\t%s\t%s\n" check p fid m in
        A.Annotation.validate_sequences_present
          ~on_violation:(mk_callback "sequences_present") !current;
        A.Annotation.validate_feature_bounds
          ~on_violation:(mk_callback "feature_bounds") !current;
        A.Annotation.validate_translation
          ~on_violation:(mk_callback "translation") !current;
        close_out oc;
        if !total = 0 then
          Printf.eprintf "(%s): Validation OK; report at %S is empty.\n%!"
            info.Tools.Argv.name path
        else begin
          Printf.eprintf
            "(%s): Validation failed: %d violation(s); see %S.\n%!"
            info.Tools.Argv.name !total path;
          exit 1
        end
      | Selection_from_labels s -> selection := A.Selection.Labels s; report_selection ()
      | Selection_from_regexps l -> selection := A.Selection.Regexps l; report_selection ()
      | Selection_negate -> selection := A.Selection.Not !selection; report_selection ()
      | Selection_clear -> selection := A.Selection.All; report_selection ()
      | Selection_print ->
        Exception.catch_unexpected_end_of_output __FUNCTION__
          (fun () ->
            let n = ref 0 in
            iter_selected (fun ~path feature ->
              incr n;
              Printf.printf "%s\t%s\t%s\t%s\n" (name_of !current ~path feature)
                (A.Annotation.path_to_string path) (A.Annotation.seq_name !current feature)
                (location_of feature));
            (* Data goes to stdout and the count to stderr, so flush before the
               count or the two interleave out of order on a terminal. *)
            flush stdout;
            if !Parameters.verbose then
              Printf.eprintf "(%s): %d %s selected by %s\n%!" info.Tools.Argv.name !n
                (String.pluralize_int "feature" !n) (A.Selection.to_string !selection))
      | Extract (kind, path) ->
        Exception.catch_unexpected_end_of_output __FUNCTION__
          (fun () ->
            let oc = open_out path in
            let n = ref 0 in
            iter_selected (fun ~path:p feature ->
              incr n;
              let sequence =
                match kind with
                | Sequence_kind.DNA -> A.Annotation.feature_dna !current feature
                | Sequence_kind.Protein -> A.Annotation.feature_protein !current feature in
              (* One line per sequence unless '--fasta-width' says otherwise:
                 an extracted feature is usually on its way down a pipe. *)
              Printf.fprintf oc ">%s path=%s seq=%s location=%s\n%s\n"
                (name_of !current ~path:p feature) (A.Annotation.path_to_string p)
                (A.Annotation.seq_name !current feature) (location_of feature)
                (A.wrap_sequence ~width:0 sequence));
            close_out oc;
            if !Parameters.verbose then
              Printf.eprintf "(%s): wrote %d %s %s to %s\n%!" info.Tools.Argv.name !n
                (Sequence_kind.to_string kind) (String.pluralize_int "sequence" !n) path)
      | Summary -> summary ()
    ) program
  with
  | A.Annotation.Validation_failed { path; feature_id; message } ->
    Printf.eprintf "(%s): Validation failed: %s\n%!"
      info.Tools.Argv.name message;
    if path <> "" || feature_id <> "" then
      Printf.eprintf "(%s):   at path=%s feature_id=%S\n%!"
        info.Tools.Argv.name path feature_id;
    Printf.eprintf
      "(%s): Re-run with '--validate-report <file>' to write every \
       violation to a tab-separated file.\n%!" info.Tools.Argv.name;
    exit 1
  | e ->
    Exception.handle __FUNCTION__ TA.usage (fun () ->
      Printf.peprintf
        "(%s): This should not have happened - please contact <paolo.ribeca@gmail.com>\n%!"
        __FUNCTION__;
      Printf.peprintf
        "(%s): You might also wish to rerun me with option -x to get a full backtrace.\n%!"
        __FUNCTION__
    ) e

