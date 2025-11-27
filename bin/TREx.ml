(*
    TREx.ml -- (c) 2023-2024 Paolo Ribeca, <paolo.ribeca@gmail.com>

    TREx finds exact tandem repeats for all the sequences present
    in an input FASTA file.

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

module TandemRepeatExplorer =
  struct
    module PrefixMultimap = Tools.Multimap (ComparableString) (ComparableInt)
    module IntervalMultimap =
      Tools.Multimap (MakeComparable (struct type t = int * int end)) (ComparableString)
    let iter ?(maximum_repeat_length = max_int) ?(minimum_locus_length = 0) ?(verbose = false) f s =
      let maximum_repeat_length = (String.length s + 1) / 2 |> min maximum_repeat_length
      and k = ref 0 and old = ref PrefixMultimap.empty in
      if verbose then
        Printf.eprintf "%s\r(%s): Initializing...%!" String.TermIO.clear __FUNCTION__;
      let l = String.length s in
      for i = 0 to l - 1 do
        old := PrefixMultimap.add "" i !old
      done;
      let res = ref IntervalMultimap.empty in
      while !k < maximum_repeat_length && PrefixMultimap.cardinal !old > 0 do
        incr k;
        if verbose then
          Printf.eprintf "%s\r(%s): Processing k=%d%!" String.TermIO.clear __FUNCTION__ !k;
        let red_k = !k - 1 and max_i = l - !k and current = ref PrefixMultimap.empty and count = ref 0 in
        PrefixMultimap.iter_set
          (fun k_mer set ->
            let card = IntSet.cardinal set in
            count := !count + card;
            if card > 1 then
              IntSet.iter
                (fun i ->
                  if i <= max_i then begin
                    let k_mer = k_mer ^ string_of_char s.[i + red_k] in
                    current := PrefixMultimap.add k_mer i !current
                  end)
                set)
          !old;
        old := !current;
        if verbose then
          Printf.eprintf " (%d left)...%!" !count;
        let k = !k and k_res = ref IntMap.empty in
        PrefixMultimap.iter_set
          (fun k_mer set ->
            (*Printf.eprintf "lo_set=%d k-mer='%s'\n%!" (Tools.IntSet.min_elt set) k_mer;*)
            let set = ref set in
            (* We extract all the series from the set,
                always starting from the smallest element left *)
            while IntSet.cardinal !set > 0 do
              let lo = IntSet.min_elt !set in
              let hi = ref lo in
              while IntSet.mem !hi !set do
                set := IntSet.remove !hi !set;
                hi := !hi + k
              done;
              let hi = !hi - k in
              if hi > lo then begin
                (* Here hi is the highest position at which a repeat
                  with length k and sequence k_mer starts *)
                k_res := IntMap.add lo (k_mer, hi + k - 1) !k_res
              end
            done)
          !old;
        (* We merge together overlapping consecutive series of k-mers *)
        let k_mer = ref "" and lo = ref (-1) and prev_lo = ref (-2) and hi = ref (-1) in
        let process_it () =
          (*if verbose && !k_mer <> "" then
            Printf.eprintf "%s(%s): Processing [%d,%d]='%s'... %s\n%!"
              Tools.String.TermIO.clear __FUNCTION__ !lo !hi !k_mer begin
                if !hi - !lo + 1 >= minimum_locus_length then
                  "added"
                else
                  "discarded"
              end;*)
          if !k_mer <> "" && !hi - !lo + 1 >= minimum_locus_length then
            res := IntervalMultimap.add (!lo, !hi) !k_mer !res in
        IntMap.iter
          (fun _lo (_k_mer, _hi) ->
            if _lo <> !prev_lo + 1 then begin
              process_it ();
              k_mer := _k_mer;
              lo := _lo;
              prev_lo := _lo;
              hi := _hi
            end else begin
              prev_lo := _lo;
              hi := max !hi _hi
            end)
          !k_res;
        process_it ()
      done;
      if verbose then
        Printf.eprintf "%s\r%!" String.TermIO.clear;
      let curr_lo = ref (-1) and curr_hi = ref (-1) and curr_k_mer = ref "" in
      let process_it () =
        if !curr_hi <> -1 then
          f !curr_k_mer (!curr_lo + 1) (!curr_hi + 1) in
      IntervalMultimap.iter_set
        (fun (lo, hi) set ->
          (* The interval must not be entirely contained into an already open one *)
          if hi > !curr_hi then begin
            process_it ();
            curr_lo := lo;
            curr_hi := hi;
            curr_k_mer := StringSet.min_elt set
          end else
            if verbose then
              Printf.eprintf "%s(%s): Discarding [%d,%d]='%s' (contained in [%d,%d]='%s')\n%!"
                String.TermIO.clear __FUNCTION__ (lo + 1) (hi + 1) (StringSet.min_elt set)
                (!curr_lo + 1) (!curr_hi + 1) !curr_k_mer)
        !res;
      process_it ()
  end

module Defaults =
  struct
    let maximum_repeat_length = max_int
    let minimum_locus_length = 0
    let linter = Sequences.Lint.String.of_string "DNA"
    let linter_keep_lowercase = false
    let linter_keep_dashes = false
    let verbose = false
  end

module Parameters =
  struct
    let maximum_repeat_length = ref Defaults.maximum_repeat_length
    let minimum_locus_length = ref Defaults.minimum_locus_length
    let linter = ref Defaults.linter
    let linter_keep_lowercase = ref Defaults.linter_keep_lowercase
    let linter_keep_dashes = ref Defaults.linter_keep_dashes
    let verbose = ref Defaults.verbose
  end

let info = {
  Tools.Argv.name = "TREx";
  version = "4";
  date = "16-Apr-2024"
} and authors = [
  "2023-2024", "Paolo Ribeca", "paolo.ribeca@gmail.com"
]

let () =
  let module TA = Tools.Argv in
  TA.set_header (info, authors, [ Info.info ]);
  TA.set_synopsis "[OPTIONS]";
  TA.parse [
    TA.make_separator "Input/Output";
    [ "-l"; "--linter" ],
      Some "'none'|'DNA'|'dna'|'protein'",
      [ "sets linter for sequence.";
        "All non-base (for DNA) or non-AA (for protein) characters";
        " are converted to unknowns" ],
      TA.Default (Sequences.Lint.String.to_string Defaults.linter |> Fun.const),
      (fun _ -> Parameters.linter := TA.get_parameter () |> Sequences.Lint.String.of_string);
    [ "--linter-keep-lowercase" ],
      Some "<bool>",
      [ "sets whether the linter should keep lowercase DNA/protein characters";
        " appearing in sequences rather than capitalise them" ],
      TA.Default (string_of_bool Defaults.linter_keep_lowercase |> Fun.const),
      (fun _ -> Parameters.linter_keep_lowercase := TA.get_parameter_boolean ());
    [ "--linter-keep-dashes" ],
      Some "<bool>",
      [ "sets whether the linter should keep dashes appearing in sequences";
        " rather than convert them to unknowns" ],
      TA.Default (string_of_bool Defaults.linter_keep_dashes |> Fun.const),
      (fun _ -> Parameters.linter_keep_dashes := TA.get_parameter_boolean ());
    TA.make_separator "Algorithm";
    [ "-M"; "--maximum_repeat_length" ],
      Some "<non_negative_integer>",
      [ "maximum unit length for a tandem repeat to be considered" ],
      TA.Default (string_of_int Defaults.maximum_repeat_length |> Fun.const),
      (fun _ -> Parameters.maximum_repeat_length := TA.get_parameter_int_non_neg ());
    [ "-m"; "--minimum_locus_length" ],
      Some "<non_negative_integer>",
      [ "minimum locus length for a tandem repeat to be considered" ],
      TA.Default (string_of_int Defaults.minimum_locus_length |> Fun.const),
      (fun _ -> Parameters.minimum_locus_length := TA.get_parameter_int_non_neg ());
    TA.make_separator "Miscellaneous";
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
  let linter_f =
    Sequences.Lint.String.lint !Parameters.linter
      ~keep_lowercase:!Parameters.linter_keep_lowercase ~keep_dashes:!Parameters.linter_keep_dashes in
  Files.FASTA.iter ~linter:linter_f ~verbose:!Parameters.verbose
    (fun (_, _, { tag; seq; _ }) ->
      TandemRepeatExplorer.iter
        ~maximum_repeat_length:!Parameters.maximum_repeat_length
        ~minimum_locus_length:!Parameters.minimum_locus_length ~verbose:!Parameters.verbose
        (fun k_mer lo hi ->
          let len_locus = hi - lo + 1 and len_repeat = String.length k_mer in
          let periods = float_of_int len_locus /. float_of_int len_repeat in
          Printf.printf "%s\t%d\t%d\t%d\t%d\t%.6g\t%s\n%!" tag lo hi len_locus len_repeat periods k_mer)
        seq)
    "/dev/stdin"

