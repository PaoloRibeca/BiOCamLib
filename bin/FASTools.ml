(*
    FASTools.ml -- (c) 2022 Paolo Ribeca, <paolo.ribeca@gmail.com>

    FASTools allows to perform a number of basic manipulations on
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

module StringMap = Tools.StringMap

type linter_t =
  | None
  | DNA
  | Protein
and working_mode_t =
  | Compact
  | Expand
  | RevCom
  | Match of Str.regexp
and to_do_t =
  | SetWorkingMode of working_mode_t
  | SetLinter of linter_t
  | SetLinterKeepDashes of bool
  | ProcessInput of KMer.ReadFiles.file_t
  | SetOutput of string
  | SetOutputPE of string * string

module Defaults =
  struct
(*
    let threads = Tools.Parallel.get_nproc ()
*)
    let verbose = false
  end

module Parameters =
  struct
    let program = ref []
    (*let threads = ref Defaults.threads*)
    let verbose = ref Defaults.verbose
  end

let version = "0.2"

let header =
  Printf.sprintf begin
    "This is the FASTools program (version %s)\n%!" ^^
    " (c) 2022 Paolo Ribeca, <paolo.ribeca@gmail.com>\n%!"
  end version

let _ =
  let module TA = Tools.Argv in
  TA.set_header header;
  TA.set_synopsis "[OPTIONS]";
  TA.parse [
    TA.make_separator "Working mode (paired-end files are automatically interleaved, default='compact')";
    [ "compact"; "-c"; "--compact" ],
      None,
      [ "put each FASTA/FASTQ record on one tab-separated line" ],
      TA.Optional,
      (fun _ -> SetWorkingMode Compact |> Tools.List.accum Parameters.program);
    [ "expand"; "-e"; "--expand" ],
      None,
      [ "split each tab-separated line into a FASTA/FASTQ record" ],
      TA.Optional,
      (fun _ -> SetWorkingMode Expand |> Tools.List.accum Parameters.program);
    [ "revcom"; "-r"; "--revcom" ],
      None,
      [ "reverse-complement sequences in FASTA/FASTQ records or tab-separated lines" ],
      TA.Optional,
      (fun _ -> SetWorkingMode RevCom |> Tools.List.accum Parameters.program);
    [ "match"; "-m"; "--match" ],
      Some "<regexp>",
      [ "select matching sequence names in FASTA/FASTQ records or tab-separated lines.";
        "For paired-end files, the pair matches when at least one name matches" ],
      TA.Optional,
      (fun _ -> SetWorkingMode (Match (TA.get_parameter () |> Str.regexp)) |> Tools.List.accum Parameters.program);
    TA.make_separator "Input/Output";
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
        end |> Tools.List.accum Parameters.program);
    [ "--linter-keep-dashes" ],
      Some "<bool>",
      [ "sets whether the linter should keep dashes appearing in sequences";
        " or convert them to unknowns" ],
      TA.Default (fun () -> "false"),
      (fun _ -> SetLinterKeepDashes (TA.get_parameter_boolean ()) |> Tools.List.accum Parameters.program);
    [ "-f"; "--fasta" ],
      Some "<fasta_file_name>",
      [ "FASTA input file containing sequences" ],
      TA.Optional,
      (fun _ -> ProcessInput (KMer.ReadFiles.FASTA (TA.get_parameter ())) |> Tools.List.accum Parameters.program);
    [ "-s"; "--single-end" ],
      Some "<fastq_file_name>",
      [ "FASTQ input file containing single-end sequencing reads" ],
      TA.Optional,
      (fun _ ->
        ProcessInput (KMer.ReadFiles.SingleEndFASTQ (TA.get_parameter ())) |> Tools.List.accum Parameters.program);
    [ "-p"; "--paired-end" ],
      Some "<fastq_file_name1> <fastq_file_name2>",
      [ "FASTQ input files containing paired-end sequencing reads" ],
      TA.Optional,
      (fun _ ->
        let name1 = TA.get_parameter () in
        let name2 = TA.get_parameter () in
        ProcessInput (KMer.ReadFiles.PairedEndFASTQ (name1, name2)) |> Tools.List.accum Parameters.program);
    [ "-t"; "--tabular" ],
      Some "<tabular_file_name>",
      [ "tabular input file containing FASTA/FASTQ records as tab-separated lines" ],
      TA.Optional,
      (fun _ -> ProcessInput (KMer.ReadFiles.Tabular (TA.get_parameter ())) |> Tools.List.accum Parameters.program);
    [ "-o"; "--output" ],
      Some "<output_file_name>",
      [ "set the name of the output file.";
        "Files are kept open, and it is possible to switch between them";
        " by repeatedly using this option.";
        "Use <stdout> for standard output" ],
      TA.Default (fun () -> "<stdout>"),
      (fun _ -> SetOutput (TA.get_parameter ()) |> Tools.List.accum Parameters.program);
    [ "-O"; "--paired-end-output" ],
      Some "<output_file_name_1> <output_file_name_2>",
      [ "set the names of paired-end FASTQ output files.";
        "Files are kept open, and it is possible to switch between them";
        " by repeatedly using this option.";
        "Use <stdout> for standard output" ],
      TA.Default (fun () -> "<stdout>"),
      (fun _ ->
        let output_1 = TA.get_parameter () in
        let output_2 = TA.get_parameter () in
        SetOutputPE (output_1, output_2) |> Tools.List.accum Parameters.program);
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
      [ "set verbose execution" ],
      TA.Default (fun () -> string_of_bool !Parameters.verbose),
      (fun _ -> Parameters.verbose := true);
    (* Hidden option to emit help in markdown format *)
    [ "--markdown" ], None, [], TA.Optional, (fun _ -> TA.markdown (); exit 0);
    [ "-h"; "--help" ],
      None,
      [ "print syntax and exit" ],
      TA.Optional,
      (fun _ -> TA.usage (); exit 1)
  ];
  let working_mode = ref Compact
  and linter = ref None and linter_keep_dashes = ref false and linter_f = ref Sequences.Lint.none
  and outputs = StringMap.singleton "<stdout>" stdout |> ref in
  let [@warning "-5"] set_linter_f () =
    match !linter with
    | None -> linter_f := Sequences.Lint.none
    | DNA -> linter_f := Sequences.Lint.dnaize ~keep_dashes:!linter_keep_dashes
    | Protein -> linter_f := Sequences.Lint.proteinize ~keep_dashes:!linter_keep_dashes
  and get_output_stream fname output =
    match StringMap.find_opt fname !outputs with
    | Some o ->
      output := o
    | None ->
      let o = open_out fname in
      outputs := StringMap.add fname o !outputs;
      output := o
  and output_1 = ref stdout and output_2 = ref stdout in
  let output_fast_record ?(rc = false) _ segm { KMer.ReadFiles.tag; seq; qua } =
    let seq =
      if rc then
        Sequences.Lint.rc seq
      else
        seq
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
    end
  and output_tabular_record ?(pe = false) ?(rc = false) _ segm { KMer.ReadFiles.tag; seq; qua } =
    let seq =
      if rc then
        Sequences.Lint.rc seq
      else
        seq
    and output =
      match segm with
      | 0 -> output_1
      | 1 -> output_2
      | _ -> assert false in
    output_string !output tag;
    output_char !output '\t';
    output_string !output seq;
    if qua <> "" then begin
      output_char !output '\t';
      output_string !output qua
    end;
    if not pe || segm = 1 then
      output_char !output '\n'
    else
      output_char !output '\t' in
  List.iter
    (function
      | SetWorkingMode mode ->
        working_mode := mode
      | SetLinter l ->
        linter := l;
        set_linter_f ()
      | SetLinterKeepDashes b ->
        linter_keep_dashes := b;
        set_linter_f ()
      | ProcessInput input ->
        begin match !working_mode, input with
        | Compact, KMer.ReadFiles.PairedEndFASTQ _ ->
          KMer.ReadFiles.add_from_files KMer.ReadFiles.empty input |>
            KMer.ReadFiles.iter ~linter:!linter_f ~verbose:!Parameters.verbose (output_tabular_record ~pe:true)
        | Compact, _ ->
          KMer.ReadFiles.add_from_files KMer.ReadFiles.empty input |>
            KMer.ReadFiles.iter ~linter:!linter_f ~verbose:!Parameters.verbose output_tabular_record
        | Expand, _ ->
          KMer.ReadFiles.add_from_files KMer.ReadFiles.empty input |>
            KMer.ReadFiles.iter ~linter:!linter_f ~verbose:!Parameters.verbose output_fast_record
        | RevCom, KMer.ReadFiles.FASTA _
        | RevCom, KMer.ReadFiles.SingleEndFASTQ _
        | RevCom, KMer.ReadFiles.PairedEndFASTQ _ ->
          KMer.ReadFiles.add_from_files KMer.ReadFiles.empty input |>
            KMer.ReadFiles.iter ~linter:!linter_f ~verbose:!Parameters.verbose (output_fast_record ~rc:true)
        | RevCom, KMer.ReadFiles.Tabular _ ->
          KMer.ReadFiles.add_from_files KMer.ReadFiles.empty input |>
            KMer.ReadFiles.iter ~linter:!linter_f ~verbose:!Parameters.verbose (output_tabular_record ~rc:true)
        | Match regexp, KMer.ReadFiles.FASTA _
        | Match regexp, KMer.ReadFiles.SingleEndFASTQ _ ->
          KMer.ReadFiles.add_from_files KMer.ReadFiles.empty input |>
            KMer.ReadFiles.iter ~linter:!linter_f ~verbose:!Parameters.verbose
              (fun i segm ({ tag; _ } as record) ->
                if Str.string_match regexp tag 0 then
                  output_fast_record i segm record)
        | Match regexp, KMer.ReadFiles.PairedEndFASTQ (file_1, file_2) ->
          Sequences.FASTQ.iter_pe ~linter:!linter_f ~verbose:!Parameters.verbose
            (fun i tag_1 seq_1 qua_1 tag_2 seq_2 qua_2 ->
              if Str.string_match regexp tag_1 0 || Str.string_match regexp tag_2 0 then begin
                output_fast_record i 0 { tag = tag_1; seq = seq_1; qua = qua_1 };
                output_fast_record i 1 { tag = tag_2; seq = seq_2; qua = qua_2 }
              end)
            file_1 file_2
        | Match regexp, KMer.ReadFiles.Tabular file ->
          Sequences.Tabular.iter ~linter:!linter_f ~verbose:!Parameters.verbose
            (fun i tag seq qua ->
              if Str.string_match regexp tag 0 then
                output_fast_record i 0 { tag; seq; qua })
            (fun i tag_1 seq_1 qua_1 tag_2 seq_2 qua_2 ->
              if Str.string_match regexp tag_1 0 || Str.string_match regexp tag_2 0 then begin
                output_fast_record i 0 { tag = tag_1; seq = seq_1; qua = qua_1 };
                output_fast_record i 1 { tag = tag_2; seq = seq_2; qua = qua_2 }
              end)
            file
        end
      | SetOutput fname ->
        get_output_stream fname output_1
      | SetOutputPE (fname_1, fname_2) ->
        get_output_stream fname_1 output_1;
        get_output_stream fname_2 output_2)
    (List.rev !Parameters.program)

