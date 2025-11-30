(*
    FASTools.ml -- (c) 2022-2025 Paolo Ribeca, <paolo.ribeca@gmail.com>

    FASTools allows to perform a number of essential manipulations on
    FASTA and FASTQ files.

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

type working_mode_t =
  | Compact
  | Expand
  | RevCom
  | UnQuals
  | Match of Str.regexp
  | Rename of Str.regexp * string
and to_do_t =
  | SetWorkingMode of working_mode_t
  | SetLinter of Sequences.Lint.string_t Sequences.Lint.String.t
  | SetLinterKeepLowercase of bool
  | SetLinterKeepDashes of bool
  | ProcessInput of Files.Reads.t
  | SetOutput of string
  | SetOutputPE of string * string

module Defaults =
  struct
    let working_mode = Compact
    let linter = Sequences.Lint.String.of_string "none"
    let linter_keep_lowercase = false
    let linter_keep_dashes = false
    let flush = false
    (*let threads = Processes.Parallel.get_nproc ()*)
    let verbose = false
  end

module Parameters =
  struct
    let program = ref []
    let flush = ref Defaults.flush
    (*let threads = ref Defaults.threads*)
    let verbose = ref Defaults.verbose
  end

let info = {
  Tools.Argv.name = "FASTools";
  version = "13";
  date = "30-Nov-2025"
} and authors = [
  "2022-2025", "Paolo Ribeca", "paolo.ribeca@gmail.com"
]

let () =
  let module TA = Tools.Argv in
  TA.set_header (info, authors, [ Info.info ]);
  TA.set_synopsis "[OPTIONS]";
  TA.parse [
    TA.make_separator_multiline [ "Working mode."; "Executed delayed in order of specification, default='compact'." ];
    [ "compact"; "-c"; "--compact" ],
      None,
      [ "put each FASTA/FASTQ record on one tab-separated line";
        " (default mode)" ],
      TA.Optional,
      (fun _ -> SetWorkingMode Compact |> List.accum Parameters.program);
    [ "expand"; "-e"; "--expand" ],
      None,
      [ "split each tab-separated line into one or more FASTA/FASTQ records" ],
      TA.Optional,
      (fun _ -> SetWorkingMode Expand |> List.accum Parameters.program);
    [ "revcom"; "-r"; "--revcom" ],
      None,
      [ "reverse-complement sequences (and reverse qualities if present)";
        "in FASTA/FASTQ records or tab-separated lines" ],
      TA.Optional,
      (fun _ -> SetWorkingMode RevCom |> List.accum Parameters.program);
    [ "dropq"; "-d"; "--dropq" ],
      None,
      [ "drop qualities in FASTA/FASTQ records or tab-separated lines" ],
      TA.Optional,
      (fun _ -> SetWorkingMode UnQuals |> List.accum Parameters.program);
    [ "match"; "-m"; "--match" ],
      Some "<regexp>",
      [ "select sequence names matching the specified regexp";
        "in FASTA/FASTQ records or tab-separated lines.";
        "The regexp must be defined according to <https://ocaml.org/api/Str.html>.";
        "For paired-end files, the pair matches when at least one name matches." ],
      TA.Optional,
      (fun _ -> SetWorkingMode (Match (TA.get_parameter () |> Str.regexp)) |> List.accum Parameters.program);
    [ "rename"; "-R"; "--rename" ],
      Some "<regexp> <replacement>",
      [ "replace with the provided pattern all the instances of the specified";
        "regexp occurring in the sequence names of FASTA/FASTQ records";
        "or tab-separated lines.";
        "The regexp must be defined according to <https://ocaml.org/api/Str.html>.";
        "Replacement expressions can contain identifiers \\1 ... \\9 for the groups";
        "matched by the regular expression; \\0 represents the full match" ],
      TA.Optional,
      (fun _ ->
        let regexp = TA.get_parameter () |> Str.regexp in
        SetWorkingMode (Rename (regexp, TA.get_parameter ())) |> List.accum Parameters.program);
    TA.make_separator_multiline [ "Input/Output."; "Executed delayed in order of specification, default='-F'." ];
    [ "-f"; "--fasta" ],
      Some "<fasta_file_name>",
      [ "process FASTA input file containing sequences" ],
      TA.Optional,
      (fun _ -> ProcessInput (FASTA (TA.get_parameter ())) |> List.accum Parameters.program);
    [ "-F" ],
      None,
      [ "process FASTA sequences from standard input" ],
      TA.Optional,
      (fun _ -> ProcessInput (FASTA "/dev/stdin") |> List.accum Parameters.program);
    [ "-s"; "--single-end" ],
      Some "<fastq_file_name>",
      [ "process FASTQ input file containing single-end sequencing reads" ],
      TA.Optional,
      (fun _ -> ProcessInput (SingleEndFASTQ (TA.get_parameter ())) |> List.accum Parameters.program);
    [ "-S" ],
      None,
      [ "process single-end FASTQ sequencing reads from standard input" ],
      TA.Optional,
      (fun _ -> ProcessInput (SingleEndFASTQ "/dev/stdin") |> List.accum Parameters.program);
    [ "-p"; "--paired-end" ],
      Some "<fastq_file_name1> <fastq_file_name2>",
      [ "process FASTQ input files containing paired-end sequencing reads" ],
      TA.Optional,
      (fun _ ->
        let name1 = TA.get_parameter () in
        let name2 = TA.get_parameter () in
        ProcessInput (PairedEndFASTQ (name1, name2)) |> List.accum Parameters.program);
    [ "-P" ],
      None,
      [ "process interleaved FASTQ sequencing reads from standard input" ],
      TA.Optional,
      (fun _ -> ProcessInput (InterleavedFASTQ "/dev/stdin") |> List.accum Parameters.program);
    [ "-t"; "--tabular" ],
      Some "<tabular_file_name>",
      [ "process input file containing FAST[A|Q] records as tab-separated lines" ],
      TA.Optional,
      (fun _ -> ProcessInput (Tabular (TA.get_parameter ())) |> List.accum Parameters.program);
    [ "-T" ],
      None,
      [ "process FAST[A|Q] records in tabular form from standard input" ],
      TA.Optional,
      (fun _ -> ProcessInput (Tabular "/dev/stdin") |> List.accum Parameters.program);
    [ "-l"; "--linter" ],
      Some "'none'|'DNA'|'dna'|'protein'",
      [ "sets linter for sequence.";
        "All non-base (for DNA) or non-AA (for protein) characters";
        " are converted to unknowns" ],
      TA.Default (Sequences.Lint.String.to_string Defaults.linter |> Fun.const),
      (fun _ -> SetLinter (TA.get_parameter () |> Sequences.Lint.String.of_string) |> List.accum Parameters.program);
    [ "--linter-keep-lowercase" ],
      Some "'true'|'false'",
      [ "sets whether the linter should keep lowercase DNA/protein characters";
        " appearing in sequences rather than capitalise them" ],
      TA.Default (string_of_bool Defaults.linter_keep_lowercase |> Fun.const),
      (fun _ -> SetLinterKeepLowercase (TA.get_parameter_boolean ()) |> List.accum Parameters.program);
    [ "--linter-keep-dashes" ],
      Some "'true'|'false'",
      [ "sets whether the linter should keep dashes appearing in sequences";
        " rather than convert them to unknowns" ],
      TA.Default (string_of_bool Defaults.linter_keep_dashes |> Fun.const),
      (fun _ -> SetLinterKeepDashes (TA.get_parameter_boolean ()) |> List.accum Parameters.program);
    [ "-o"; "--output" ],
      Some "<output_file_name>",
      [ "set the name of the output file.";
        "Files are kept open, and it is possible to switch between them";
        " by repeatedly using this option.";
        "Use '/dev/stdout' for standard output" ],
      TA.Default (Fun.const "/dev/stdout"),
      (fun _ -> SetOutput (TA.get_parameter ()) |> List.accum Parameters.program);
    [ "-O"; "--paired-end-output" ],
      Some "<output_file_name_1> <output_file_name_2>",
      [ "set the names of paired-end FASTQ output files.";
        "Files are kept open, and it is possible to switch between them";
        " by repeatedly using this option.";
        "Use '/dev/stdout' for standard output" ],
      TA.Default (Fun.const "/dev/stdout"),
      (fun _ ->
        let output1 = TA.get_parameter () in
        let output2 = TA.get_parameter () in
        SetOutputPE (output1, output2) |> List.accum Parameters.program);
    [ "--flush"; "--flush-output" ],
      None,
      [ "flush output after each record (global option)" ],
      TA.Default (Fun.const "do not flush"),
      (fun _ -> Parameters.flush := true);
    TA.make_separator "Miscellaneous";
(*
    [ "-t"; "-T"; "--threads" ],
      Some "<computing_threads>",
      [ "number of concurrent computing threads to be spawned";
        " (default automatically detected from your configuration)" ],
      TA.Default (string_of_int !Parameters.threads |> Fun.const),
      (fun _ -> Parameters.threads := TA.get_parameter_int_pos ());
*)
    [ "-v"; "--verbose" ],
      None,
      [ "set verbose execution (global option)" ],
      TA.Default (Fun.const "quiet execution"),
      (fun _ -> Parameters.verbose := true);
    [ "-V"; "--version" ],
      None,
      [ "print version and exit" ],
      TA.Optional,
      (fun _ -> Printf.printf "%s\n%!" info.version; exit 0);
    (* Hidden option to emit help in markdown format *)
    [ "--markdown" ], None, [], TA.Optional, (fun _ -> TA.markdown (); exit 0);
    [ "-h"; "--help" ],
      None,
      [ "print syntax and exit" ],
      TA.Optional,
      (fun _ -> TA.usage (); exit 1)
  ];
  if !Parameters.verbose then
    TA.header ();
  let working_mode = ref Defaults.working_mode
  and linter_keep_lowercase = ref Defaults.linter_keep_lowercase
  and linter_keep_dashes = ref Defaults.linter_keep_dashes
  and linter = ref Defaults.linter in
  let get_linter () =
    Sequences.Lint.String.lint !linter ~keep_lowercase:!linter_keep_lowercase ~keep_dashes:!linter_keep_dashes
  and outputs = StringMap.singleton "/dev/stdout" stdout |> ref in
  let get_output_stream fname output =
    match StringMap.find_opt fname !outputs with
    | Some o ->
      output := o
    | None ->
      let o = open_out fname in
      outputs := StringMap.add fname o !outputs;
      output := o
  and output1 = ref stdout and output2 = ref stdout in
  let output_fast_record ?(rc = false) (_, segm, { Files.Base.Read.tag; seq; qua }) =
    let seq, qua =
      if rc then
        Sequences.Lint.rc seq, String.rev qua
      else
        seq, qua
    and output =
      match segm with
      | 0 -> !output1
      | 1 -> !output2
      | _ -> assert false in
    if qua = "" then begin
      output_char output '>';
      output_string output tag;
      output_char output '\n';
      output_string output seq;
      output_char output '\n'
    end else begin
      output_char output '@';
      output_string output tag;
      output_char output '\n';
      output_string output seq;
      output_string output "\n+\n";
      output_string output qua;
      output_char output '\n'
    end;
    if !Parameters.flush then
      flush output
  and output_tabular_record ?(pe = false) ?(rc = false) (_, segm, { Files.Base.Read.tag; seq; qua }) =
    let seq, qua =
      if rc then
        Sequences.Lint.rc seq, String.rev qua
      else
        seq, qua
    and output =
      match segm with
      | 0 -> !output1
      | 1 -> !output2
      | _ -> assert false in
    output_string output tag;
    output_char output '\t';
    output_string output seq;
    if qua <> "" then begin
      output_char output '\t';
      output_string output qua
    end;
    if pe && !output1 = !output2 && segm = 0 then
      output_char output '\t'
    else begin
      output_char output '\n';
      if !Parameters.flush then
        flush output
    end in
  let output_record ?(pe = false) ?(rc = false) = function
    | Files.Reads.FASTA _ | SingleEndFASTQ _ | PairedEndFASTQ _ | InterleavedFASTQ _ ->
      output_fast_record ~rc (* There is no PE switch for FAST output *)
    | Tabular _ ->
      output_tabular_record ~pe ~rc
  and has_at_least_one_input = ref false in
  List.iter
    (function
      | ProcessInput _ ->
        has_at_least_one_input := true
      | _ ->
        ())
    !Parameters.program;
  if not !has_at_least_one_input then
    ProcessInput (FASTA "/dev/stdin") |> List.accum Parameters.program;
  List.iter
    (function
      | SetWorkingMode mode ->
        working_mode := mode
      | SetLinter l ->
        linter := l
      | SetLinterKeepLowercase b ->
        linter_keep_lowercase := b
      | SetLinterKeepDashes b ->
        linter_keep_dashes := b
      | ProcessInput input ->
        begin match !working_mode with
        | Compact -> (* We convert records to tabular form *)
          Files.Reads.iter_se_pe ~linter:(get_linter ()) ~verbose:!Parameters.verbose
            output_tabular_record
            (fun record1 record2 ->
              let output_record = output_tabular_record ~pe:true in
              output_record record1;
              output_record record2)
            input
        | Expand -> (* We convert records to FAST* form *)
          Files.Reads.iter_se_pe ~linter:(get_linter ()) ~verbose:!Parameters.verbose
            output_fast_record
            (fun record1 record2 ->
              output_fast_record record1; (* There is no PE switch for FAST output *)
              output_fast_record record2)
            input
        | RevCom ->
          Files.Reads.iter_se_pe ~linter:(get_linter ()) ~verbose:!Parameters.verbose
            (output_record ~rc:true input)
            (fun record1 record2 ->
              let output_record = output_record ~rc:true ~pe:true input in
              output_record record1;
              output_record record2)
            input
        | UnQuals ->
          Files.Reads.iter_se_pe ~linter:(get_linter ()) ~verbose:!Parameters.verbose
            (fun (id, segm, read) ->
              (output_record input) (id, segm, { read with qua = "" }))
            (fun (id1, segm1, read1) (id2, segm2, read2) ->
              let output_record = output_record ~pe:true input in
              output_record (id1, segm1, { read1 with qua = "" });
              output_record (id2, segm2, { read2 with qua = "" }))
            input
        | Match regexp ->
          Files.Reads.iter_se_pe ~linter:(get_linter ()) ~verbose:!Parameters.verbose
            (fun ((_, _, { tag; _ }) as record) ->
              if Str.matches regexp tag then
                output_record input record)
            (fun (_, _, { tag = tag1; _ } as record1) (_, _, { tag = tag2; _ } as record2) ->
              if Str.matches regexp tag1 || Str.matches regexp tag2 then begin
                let output_record = output_record ~pe:true input in
                output_record record1;
                output_record record2
              end)
            input
        | Rename (regexp, replacement) ->
          Files.Reads.iter_se_pe ~linter:(get_linter ()) ~verbose:!Parameters.verbose
            (fun (id, segm, ({ tag; _ } as read)) ->
              output_record input (id, segm, { read with tag = Str.global_replace regexp replacement tag }))
            (fun (id1, segm1, ({ tag = tag1; _ } as read1)) (id2, segm2, ({ tag = tag2; _ } as read2)) ->
              let output_record = output_record ~pe:true input in
              output_record (id1, segm1, { read1 with tag = Str.global_replace regexp replacement tag1 });
              output_record (id2, segm2, { read2 with tag = Str.global_replace regexp replacement tag2 }))
            input
        end
      | SetOutput fname ->
        get_output_stream fname output1
      | SetOutputPE (fname1, fname2) ->
        get_output_stream fname1 output1;
        get_output_stream fname2 output2)
    (List.rev !Parameters.program)

