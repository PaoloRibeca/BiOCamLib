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
      Exception.raise __FUNCTION__ Algorithm
        (Printf.sprintf "Unknown mode %S (expected replace|add)" s)
end

type to_do_t =
  | Empty
  | Of_binary of string
  | To_binary of string
  | Annotation_op of Mode.t * A.Format.t * string
  | Reference_op of Mode.t * string
  | Set_hierarchy of A.Format.t * string
  | To_format of A.Format.t * string
  | Validate_sequences_present
  | Validate_feature_bounds
  | Validate_translation
  | Validate_all
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
  version = "1";
  date = "09-May-2026"
} and authors = [
  "2026", "Paolo Ribeca", "paolo.ribeca@gmail.com"
]

let () =
  let module TA = Tools.Argv in
  TA.set_header (info, authors, [ Info.info ]);
  TA.set_synopsis "[ACTIONS]";
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
        let fmt = TA.get_parameter () |> A.Format.of_string in
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
        let fmt = TA.get_parameter () |> A.Format.of_string in
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
      Some "<replace|add> <gff3|gtf|genbank> <file>",
      [ "merge or replace the register from <file> in the named";
        " format.  When the format is GenBank and the input";
        " carries an ORIGIN section, the reference sequence is";
        " replaced as well." ],
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
        "Each check raises and exits non-zero on the first";
        "violation.  All require a reference to be set." ];
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
    [ "--summary" ],
      None,
      [ "print a one-line summary of the current register to stderr" ],
      TA.Optional,
      (fun _ -> Summary |> List.accum Parameters.program);
    TA.make_separator_multiline
      [ "";
        "Annotation output." ];
    [ "--to" ],
      Some "<gff3|gtf|genbank> <file>",
      [ "write the register to <file> in the named format" ],
      TA.Optional,
      (fun _ ->
        let fmt = TA.get_parameter () |> A.Format.of_string in
        let p = TA.get_parameter () in
        To_format (fmt, p) |> List.accum Parameters.program);
    [ "--to-gff3" ],
      Some "<file>",
      [ "shorthand for '--to gff3 <file>'" ],
      TA.Optional,
      (fun _ ->
        To_format (A.Format.GFF3, TA.get_parameter ())
        |> List.accum Parameters.program);
    [ "--to-gtf" ],
      Some "<file>",
      [ "shorthand for '--to gtf <file>'" ],
      TA.Optional,
      (fun _ ->
        To_format (A.Format.GTF, TA.get_parameter ())
        |> List.accum Parameters.program);
    [ "--to-genbank" ],
      Some "<file>",
      [ "shorthand for '--to genbank <file>'" ],
      TA.Optional,
      (fun _ ->
        To_format (A.Format.GenBank, TA.get_parameter ())
        |> List.accum Parameters.program);
    TA.make_separator_multiline
      [ "Miscellaneous options."; "They are set immediately." ];
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
    let r = Sequences.Reference.add_from_fasta base path in
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
      (Annotations_Base.Path.Table.cardinal (A.Annotation.paths !current))
      (match A.Annotation.reference !current with
       | None -> "(none)"
       | Some _ -> "(loaded)") in
  try
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
        read_format mode fmt path
      | Reference_op (mode, path) ->
        read_reference mode path
      | Set_hierarchy (fmt, s) ->
        Hashtbl.replace hierarchy_overrides fmt
          (A.Hierarchy.of_string s)
      | To_format (fmt, path) ->
        Exception.catch_unexpected_end_of_output __FUNCTION__
          (fun () ->
            let module F = (val A.Format.module_of fmt) in
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
      | Summary -> summary ()
    ) program
  with e ->
    Exception.handle __FUNCTION__ TA.usage (fun () ->
      Printf.peprintf
        "(%s): Run with -x to print a full backtrace.\n%!"
        __FUNCTION__
    ) e

