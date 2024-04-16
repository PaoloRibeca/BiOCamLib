(*
    FASTools.ml -- (c) 2022-2024 Paolo Ribeca, <paolo.ribeca@gmail.com>

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

type linter_t =
  | None
  | DNA
  | Protein
and working_mode_t =
  | Compact
  | Expand
  | Match of Str.regexp
  | RevCom
  | UnQuals
and to_do_t =
  | SetWorkingMode of working_mode_t
  | SetLinter of linter_t
  | SetLinterKeepLowercase of bool
  | SetLinterKeepDashes of bool
  | ProcessInput of Files.Type.t
  | SetOutput of string
  | SetOutputPE of string * string

module Parameters =
  struct
    let program = ref []
    let flush = ref false
    (*let threads = ref Tools.Parallel.get_nproc ()*)
    let verbose = ref false
  end

let info = {
  Tools.Argv.name = "FASTools";
  version = "9";
  date = "16-Apr-2024"
} and authors = [
  "2022-2024", "Paolo Ribeca", "paolo.ribeca@gmail.com"
]

let () =
  let module TA = Tools.Argv in
  TA.set_header (info, authors, [ Info.info ]);
  TA.set_synopsis "[OPTIONS]";
  TA.parse [
    TA.make_separator_multiline [ "Working mode."; "Executed delayed in order of specification, default='compact'." ];
    [ "compact"; "-c"; "--compact" ],
      None,
      [ "put each FASTA/FASTQ record on one tab-separated line" ],
      TA.Optional,
      (fun _ -> SetWorkingMode Compact |> List.accum Parameters.program);
    [ "expand"; "-e"; "--expand" ],
      None,
      [ "split each tab-separated line into one or more FASTA/FASTQ records" ],
      TA.Optional,
      (fun _ -> SetWorkingMode Expand |> List.accum Parameters.program);
    [ "match"; "-m"; "--match" ],
      Some "<regexp>",
      [ "select sequence names matching the specified regexp";
        "in FASTA/FASTQ records or tab-separated lines.";
        "The regexp must be defined according to <https://ocaml.org/api/Str.html>.";
        "For paired-end files, the pair matches when at least one name matches." ],
      TA.Optional,
      (fun _ -> SetWorkingMode (Match (TA.get_parameter () |> Str.regexp)) |> List.accum Parameters.program);
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
      TA.Default (fun () -> "none"),
      (fun _ ->
        SetLinter begin
          match TA.get_parameter () with
          | "none" -> None
          | "DNA" | "dna" -> DNA
          | "protein" -> Protein
          | w ->
            Printf.sprintf "Unknown linter '%s'" w |> TA.parse_error;
            assert false (* Just to please the compiler *)
        end |> List.accum Parameters.program);
    [ "--linter-keep-lowercase" ],
      Some "'true'|'false'",
      [ "sets whether the linter should keep lowercase DNA/protein characters";
        " appearing in sequences rather than capitalise them" ],
      TA.Default (fun () -> "false"),
      (fun _ -> SetLinterKeepLowercase (TA.get_parameter_boolean ()) |> List.accum Parameters.program);
    [ "--linter-keep-dashes" ],
      Some "'true'|'false'",
      [ "sets whether the linter should keep dashes appearing in sequences";
        " rather than convert them to unknowns" ],
      TA.Default (fun () -> "false"),
      (fun _ -> SetLinterKeepDashes (TA.get_parameter_boolean ()) |> List.accum Parameters.program);
    [ "-o"; "--output" ],
      Some "<output_file_name>",
      [ "set the name of the output file.";
        "Files are kept open, and it is possible to switch between them";
        " by repeatedly using this option.";
        "Use '/dev/stdout' for standard output" ],
      TA.Default (fun () -> "/dev/stdout"),
      (fun _ -> SetOutput (TA.get_parameter ()) |> List.accum Parameters.program);
    [ "-O"; "--paired-end-output" ],
      Some "<output_file_name_1> <output_file_name_2>",
      [ "set the names of paired-end FASTQ output files.";
        "Files are kept open, and it is possible to switch between them";
        " by repeatedly using this option.";
        "Use '/dev/stdout' for standard output" ],
      TA.Default (fun () -> "/dev/stdout"),
      (fun _ ->
        let output_1 = TA.get_parameter () in
        let output_2 = TA.get_parameter () in
        SetOutputPE (output_1, output_2) |> List.accum Parameters.program);
    [ "--flush"; "--flush-output" ],
      None,
      [ "flush output after each record (global option)" ],
      TA.Default (fun () -> "do not flush"),
      (fun _ -> Parameters.flush := true);
    TA.make_separator "Miscellaneous";
(*
    [ "-t"; "-T"; "--threads" ],
      Some "<computing_threads>",
      [ "number of concurrent computing threads to be spawned";
        " (default automatically detected from your configuration)" ],
      TA.Default (fun () -> string_of_int !Parameters.threads),
      (fun _ -> Parameters.threads := TA.get_parameter_int_pos ());
*)
    [ "-v"; "--verbose" ],
      None,
      [ "set verbose execution (global option)" ],
      TA.Default (fun () -> string_of_bool !Parameters.verbose),
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
  let working_mode = ref Compact
  and linter_keep_lowercase = ref false and linter_keep_dashes = ref false
  and linter = ref None and linter_f = ref Sequences.Lint.none
  and outputs = StringMap.singleton "/dev/stdout" stdout |> ref in
  let [@warning "-5"] set_linter_f () =
    match !linter with
    | None ->
      linter_f := Sequences.Lint.none
    | DNA ->
      linter_f := Sequences.Lint.dnaize ~keep_lowercase:!linter_keep_lowercase ~keep_dashes:!linter_keep_dashes
    | Protein ->
      linter_f := Sequences.Lint.proteinize ~keep_lowercase:!linter_keep_lowercase ~keep_dashes:!linter_keep_dashes
  and get_output_stream fname output =
    match StringMap.find_opt fname !outputs with
    | Some o ->
      output := o
    | None ->
      let o = open_out fname in
      outputs := StringMap.add fname o !outputs;
      output := o
  and output_1 = ref stdout and output_2 = ref stdout in
  let output_fast_record ?(rc = false) _ segm { Files.ReadsIterate.tag; seq; qua } =
    let seq, qua =
      if rc then
        Sequences.Lint.rc seq, String.rev qua
      else
        seq, qua
    and output =
      match segm with
      | 0 -> !output_1
      | 1 -> !output_2
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
  and output_tabular_record ?(pe = false) ?(rc = false) _ segm { Files.ReadsIterate.tag; seq; qua } =
    let seq, qua =
      if rc then
        Sequences.Lint.rc seq, String.rev qua
      else
        seq, qua
    and output =
      match segm with
      | 0 -> !output_1
      | 1 -> !output_2
      | _ -> assert false in
    output_string output tag;
    output_char output '\t';
    output_string output seq;
    if qua <> "" then begin
      output_char output '\t';
      output_string output qua
    end;
    if pe && !output_1 = !output_2 && segm = 0 then
      output_char output '\t'
    else begin
      output_char output '\n';
      if !Parameters.flush then
        flush output
    end in
  let has_at_least_one_input = ref false in
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
        linter := l;
        set_linter_f ()
      | SetLinterKeepLowercase b ->
        linter_keep_lowercase := b;
        set_linter_f ()
      | SetLinterKeepDashes b ->
        linter_keep_dashes := b;
        set_linter_f ()
      | ProcessInput input ->
        begin match !working_mode, input with
        | Compact, PairedEndFASTQ _ | Compact, InterleavedFASTQ _ | Compact, Tabular _ ->
          Files.ReadsIterate.add_from_files Files.ReadsIterate.empty input |>
            Files.ReadsIterate.iter ~linter:!linter_f ~verbose:!Parameters.verbose (output_tabular_record ~pe:true)
        | Compact, _ ->
          Files.ReadsIterate.add_from_files Files.ReadsIterate.empty input |>
            Files.ReadsIterate.iter ~linter:!linter_f ~verbose:!Parameters.verbose output_tabular_record
        | Expand, _ ->
          Files.ReadsIterate.add_from_files Files.ReadsIterate.empty input |>
            Files.ReadsIterate.iter ~linter:!linter_f ~verbose:!Parameters.verbose output_fast_record
        | Match regexp, FASTA _
        | Match regexp, SingleEndFASTQ _ ->
          Files.ReadsIterate.add_from_files Files.ReadsIterate.empty input |>
            Files.ReadsIterate.iter ~linter:!linter_f ~verbose:!Parameters.verbose
              (fun i segm ({ tag; _ } as record) ->
                if Str.matches regexp tag then
                  output_fast_record i segm record)
        | Match regexp, PairedEndFASTQ (file_1, file_2) ->
          Files.FASTQ.iter_pe ~linter:!linter_f ~verbose:!Parameters.verbose
            (fun i tag_1 seq_1 qua_1 tag_2 seq_2 qua_2 ->
              if Str.matches regexp tag_1 || Str.matches regexp tag_2 then begin
                output_fast_record i 0 { tag = tag_1; seq = seq_1; qua = qua_1 };
                output_fast_record i 1 { tag = tag_2; seq = seq_2; qua = qua_2 }
              end)
            file_1 file_2
        | Match regexp, InterleavedFASTQ file ->
          Files.FASTQ.iter_il ~linter:!linter_f ~verbose:!Parameters.verbose
            (fun i tag_1 seq_1 qua_1 tag_2 seq_2 qua_2 ->
              if Str.matches regexp tag_1 || Str.matches regexp tag_2 then begin
                output_fast_record i 0 { tag = tag_1; seq = seq_1; qua = qua_1 };
                output_fast_record i 1 { tag = tag_2; seq = seq_2; qua = qua_2 }
              end)
            file
        | Match regexp, Tabular file ->
          Files.Tabular.iter ~linter:!linter_f ~verbose:!Parameters.verbose
            (fun i tag seq qua ->
              if Str.matches regexp tag then
                output_fast_record i 0 { tag; seq; qua })
            (fun i tag_1 seq_1 qua_1 tag_2 seq_2 qua_2 ->
              if Str.matches regexp tag_1 || Str.matches regexp tag_2 then begin
                output_fast_record i 0 { tag = tag_1; seq = seq_1; qua = qua_1 };
                output_fast_record i 1 { tag = tag_2; seq = seq_2; qua = qua_2 }
              end)
            file
        | RevCom, FASTA _
        | RevCom, SingleEndFASTQ _
        | RevCom, PairedEndFASTQ _
        | RevCom, InterleavedFASTQ _ ->
          Files.ReadsIterate.add_from_files Files.ReadsIterate.empty input |>
            Files.ReadsIterate.iter ~linter:!linter_f ~verbose:!Parameters.verbose (output_fast_record ~rc:true)
        | RevCom, Tabular _ ->
          Files.ReadsIterate.add_from_files Files.ReadsIterate.empty input |>
            Files.ReadsIterate.iter ~linter:!linter_f ~verbose:!Parameters.verbose (output_tabular_record ~rc:true)
        | UnQuals, FASTA _
        | UnQuals, SingleEndFASTQ _ ->
          Files.ReadsIterate.add_from_files Files.ReadsIterate.empty input |>
            Files.ReadsIterate.iter ~linter:!linter_f ~verbose:!Parameters.verbose
              (fun i segm record ->
                output_fast_record i segm { record with qua = "" })
        | UnQuals, PairedEndFASTQ (file_1, file_2) ->
          Files.FASTQ.iter_pe ~linter:!linter_f ~verbose:!Parameters.verbose
            (fun i tag_1 seq_1 _ tag_2 seq_2 _ ->
              output_fast_record i 0 { tag = tag_1; seq = seq_1; qua = "" };
              output_fast_record i 1 { tag = tag_2; seq = seq_2; qua = "" })
            file_1 file_2
        | UnQuals, InterleavedFASTQ file ->
          Files.FASTQ.iter_il ~linter:!linter_f ~verbose:!Parameters.verbose
            (fun i tag_1 seq_1 _ tag_2 seq_2 _ ->
              output_fast_record i 0 { tag = tag_1; seq = seq_1; qua = "" };
              output_fast_record i 1 { tag = tag_2; seq = seq_2; qua = "" })
            file
        | UnQuals, Tabular file ->
          Files.Tabular.iter ~linter:!linter_f ~verbose:!Parameters.verbose
            (fun i tag seq _ ->
              output_fast_record i 0 { tag; seq; qua = "" })
            (fun i tag_1 seq_1 _ tag_2 seq_2 _ ->
              output_fast_record i 0 { tag = tag_1; seq = seq_1; qua = "" };
              output_fast_record i 1 { tag = tag_2; seq = seq_2; qua = "" })
            file
        end
      | SetOutput fname ->
        get_output_stream fname output_1
      | SetOutputPE (fname_1, fname_2) ->
        get_output_stream fname_1 output_1;
        get_output_stream fname_2 output_2)
    (List.rev !Parameters.program)

