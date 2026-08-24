(*
    Tests_Consensus.ml -- (c) 2026 Paolo Ribeca, <paolo.ribeca@gmail.com>

    This file is part of BiOCamLib, the OCaml foundations upon which
    a number of the bioinformatics tools I developed are built.

    Tests_Consensus.ml exercises the consensus caller: the two
    alignment-tidying primitives it is built on, and the column-and-
    window vote itself.  Inputs are built inline, small enough that
    the expected answer can be worked out by hand from the rules
    rather than copied from a run.

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

(* Helpers. *)

let dashes n = String.make n '-'

let show_sides (s, first, last) = Printf.sprintf "%S %d %d" s first last

(* Side dashes.  The two indices are the first and last character of what is
   left, zero-based and inclusive, so an all-dash line reports a first past the
   end and a last before the beginning -- an empty range rather than a nonsense
   one, which is what the callers below rely on to add nothing to coverage. *)

let test_side_dashes () =
  Testing.section "Alignment side dashes" (fun () ->
    Testing.check_string "dashes on both sides become spaces"
      ~expected:"\"  ACGT  \" 2 5"
      (show_sides (Consensus.Alignment.replace_side_dashes "--ACGT--"));
    Testing.check_string "the replacement character is the caller's"
      ~expected:"\"NNACGTNN\" 2 5"
      (show_sides (Consensus.Alignment.replace_side_dashes ~replacement:'N' "--ACGT--"));
    Testing.check_string "interior dashes are left alone"
      ~expected:"\"AC--GT\" 0 5"
      (show_sides (Consensus.Alignment.replace_side_dashes "AC--GT"));
    Testing.check_string "a line with no dashes is untouched"
      ~expected:"\"ACGT\" 0 3"
      (show_sides (Consensus.Alignment.replace_side_dashes "ACGT"));
    (* First past last, which is the empty range every caller reads it as.
       The exact pair falls out of the leading pass having already rewritten
       those bytes, so the trailing pass finds nothing left to walk back over. *)
    Testing.check_string "an all-dash line reports an empty range"
      ~expected:"\"    \" 4 3"
      (show_sides (Consensus.Alignment.replace_side_dashes "----"));
    Testing.check_string "and so does an empty one"
      ~expected:"\"\" 0 -1"
      (show_sides (Consensus.Alignment.replace_side_dashes "")))

(* Tips.  A terminal block is erased when the gap separating it from the rest
   is at least [tip_gap_multiplier] times as long as the block, and only while
   the total erased stays within [max_tip_threshold] per cent of the non-gap
   characters -- so a long arm is kept however isolated it looks. *)

let test_remove_tips () =
  Testing.section "Alignment tips" (fun () ->
    Testing.check_string "a line without gaps is returned as it stands"
      ~expected:"ACGTACGTACGT"
      (Consensus.Alignment.remove_tips "ACGTACGTACGT");
    (* Block of 2 behind a gap of 10: 10 >= 2.5 * 2, and 2 is within 30% of the
       18 non-gap characters, so the block goes. *)
    Testing.check_string "a short block behind a long gap is erased"
      ~expected:(dashes 12 ^ "ACGTACGTACGTACGT")
      (Consensus.Alignment.remove_tips ("AC" ^ dashes 10 ^ "ACGTACGTACGTACGT"));
    (* Block of 4 behind a gap of 2: 2 < 2.5 * 4, so the gap is not long
       enough to call the block a tip. *)
    Testing.check_string "a block behind a short gap is kept"
      ~expected:"ACGT--ACGTACGTACGT"
      (Consensus.Alignment.remove_tips "ACGT--ACGTACGTACGT");
    (* Block of 6 behind a gap of 15, against a body of 12.  The gap is long
       enough -- 15 >= 2.5 * 6 -- but erasing 6 of the 18 non-gap characters is
       past the 30% ceiling of 5, so the block stays. *)
    Testing.check_string "a block too long to be a tip is kept however isolated"
      ~expected:("ACGTAC" ^ dashes 15 ^ "ACGTACGTACGT")
      (Consensus.Alignment.remove_tips ("ACGTAC" ^ dashes 15 ^ "ACGTACGTACGT"));
    (* The same line at a 60% ceiling, where 6 is now within the 10 allowed and
       the block goes.  Nothing else changed, so the ceiling and not the gap is
       what decided the previous check.  The body of 12 is still safe: it is
       past the ceiling itself, whatever the gap beside it grows to. *)
    Testing.check_string "raising the ceiling admits it"
      ~expected:(dashes 21 ^ "ACGTACGTACGT")
      (Consensus.Alignment.remove_tips ~max_tip_threshold:60
         ("ACGTAC" ^ dashes 15 ^ "ACGTACGTACGT"));
    Testing.check_string "and tips are taken off both ends"
      ~expected:(dashes 12 ^ "ACGTACGTACGTACGT" ^ dashes 12)
      (Consensus.Alignment.remove_tips
         ("AC" ^ dashes 10 ^ "ACGTACGTACGTACGT" ^ dashes 10 ^ "AC"));
    Testing.check_raises ~re:"cannot be negative" "a negative multiplier is refused"
      (fun () -> Consensus.Alignment.remove_tips ~tip_gap_multiplier:(-1.) "ACGT");
    Testing.check_raises ~re:"percentage" "a threshold outside 0..99 is refused"
      (fun () -> Consensus.Alignment.remove_tips ~max_tip_threshold:100 "ACGT"))

(* The consensus itself.  Two things are worth pinning beyond the obvious
   majority vote: that an empty alignment is the empty string rather than an
   error, and that the CASE of the output carries the coverage -- upper when at
   least [min_coverage] sequences voted for the winning window, lower when
   fewer did.  That is real information, and it is easy to lose in a refactor
   because both spellings look equally correct. *)

let test_of_alignment () =
  Testing.section "Consensus of an alignment" (fun () ->
    Testing.check_string "an empty alignment gives an empty consensus"
      ~expected:"" (Consensus.of_alignment [||]);
    Testing.check_string "one sequence under the coverage floor comes back lower case"
      ~expected:"acgtacgt" (Consensus.of_alignment [| "ACGTACGT" |]);
    Testing.check_string "and upper case once the floor is met"
      ~expected:"ACGTACGT" (Consensus.of_alignment ~min_coverage:1 [| "ACGTACGT" |]);
    Testing.check_string "five identical sequences meet the default floor"
      ~expected:"ACGTACGT" (Consensus.of_alignment (Array.make 5 "ACGTACGT"));
    Testing.check_string "the majority wins a disagreeing column"
      ~expected:"ACGTACGT"
      (Consensus.of_alignment ~min_coverage:1
         [| "ACGTACGT"; "ACGTACGT"; "ACGAACGT" |]);
    (* The guard on the truncation [remove_tips] used to do.  [of_alignment]
       indexes every line by the length it measured before tidying them, so a
       line coming back shorter is not a wrong answer but an out-of-bounds
       access: an interior gap is all it takes to reach that path. *)
    Testing.check_int "a line with an interior gap keeps its length" ~expected:18
      (String.length (Consensus.of_alignment ~min_coverage:1 [| "ACGT--ACGTACGTACGT" |]));
    Testing.check_string "input is DNA-linted on the way in"
      ~expected:"ACGTACGT"
      (Consensus.of_alignment ~min_coverage:1 [| "acgtacgt" |]);
    Testing.check_raises ~re:"Incompatible sequence length"
      "sequences of different lengths are refused"
      (fun () -> Consensus.of_alignment [| "ACGTACGT"; "ACGT" |]);
    Testing.check_raises ~re:"consensus_window" "a window wider than the alignment is refused"
      (fun () -> Consensus.of_alignment ~consensus_window:99 [| "ACGTACGT" |]);
    Testing.check_raises ~re:"consensus_window" "and one that is not positive"
      (fun () -> Consensus.of_alignment ~consensus_window:0 [| "ACGTACGT" |]);
    Testing.check_raises ~re:"cannot be negative" "as is a negative coverage floor"
      (fun () -> Consensus.of_alignment ~min_coverage:(-1) [| "ACGTACGT" |]))

(* Consensus from a pileup.  All of this runs through channels, so a case is a
   pileup in and the two files out -- which is what a caller actually sees, and
   the only place a wrong column shows up. *)

let read_file path =
  let ic = open_in_bin path in
  let res = really_input_string ic (in_channel_length ic) in
  close_in ic;
  res

let pileup lines = String.concat "\n" (List.map (String.concat "\t") lines) ^ "\n"

let consensus_of ?insertion_min_fraction ?insertion_min_coverage ?multiple_insertions ?seed text =
  let in_path = Filename.temp_file "BiOCamLib_Tests_" ".pileup"
  and seq_path = Filename.temp_file "BiOCamLib_Tests_" ".fasta"
  and bg_path = Filename.temp_file "BiOCamLib_Tests_" ".bedgraph" in
  Fun.protect ~finally:(fun () -> List.iter Sys.remove [ in_path; seq_path; bg_path ]) (fun () ->
    let oc = open_out in_path in
    output_string oc text;
    close_out oc;
    let ic = open_in in_path and seq_oc = open_out seq_path and bg_oc = open_out bg_path in
    let stats =
      Fun.protect ~finally:(fun () -> close_in ic; close_out seq_oc; close_out bg_oc) (fun () ->
        Consensus.Mpileup.from_mpileup ?insertion_min_fraction ?insertion_min_coverage
          ?multiple_insertions ?seed ~sequence:seq_oc ~bedgraph:bg_oc ic) in
    stats, read_file seq_path, read_file bg_path)

let sequence_of ?insertion_min_fraction ?insertion_min_coverage ?multiple_insertions ?seed text =
  let _, s, _ =
    consensus_of ?insertion_min_fraction ?insertion_min_coverage ?multiple_insertions ?seed text in
  s

let bedgraph_of text = let _, _, b = consensus_of text in b

let stats_of text = let st, _, _ = consensus_of text in st

let test_from_mpileup () =
  Testing.section "Consensus from a pileup" (fun () ->
    (* The plain case: whatever most reads said, one character per position. *)
    Testing.check_string "the most frequent base at each position wins"
      ~expected:">chr\nACGT\n"
      (sequence_of
         (pileup
            [ [ "chr"; "1"; "A"; "4"; "...."; "IIII" ]; [ "chr"; "2"; "C"; "4"; "...."; "IIII" ];
              [ "chr"; "3"; "G"; "4"; "...."; "IIII" ];
              [ "chr"; "4"; "T"; "4"; "...."; "IIII" ] ]));
    Testing.check_string "and a disagreeing majority is what is taken"
      ~expected:">chr\nGG\n"
      (sequence_of
         (pileup
            [ [ "chr"; "1"; "A"; "4"; "GGG."; "IIII" ];
              [ "chr"; "2"; "A"; "4"; ".GGG"; "IIII" ] ]));
    (* A deleted base is a vote, and the vote it wins is "no character here" --
       which is the one place the consensus's reading of a pileup differs from a
       variant caller's, where a read inside a deletion votes for nothing. *)
    Testing.check_string "a position most reads have deleted contributes nothing"
      ~expected:">chr\nAT\n"
      (sequence_of
         (pileup
            [ [ "chr"; "1"; "A"; "4"; "...."; "IIII" ]; [ "chr"; "2"; "C"; "4"; "***."; "IIII" ];
              [ "chr"; "3"; "T"; "4"; "...."; "IIII" ] ]));
    (* No coverage at all is not a deletion: a deletion is covered by reads that
       say so, while here nothing was read, so the segment is kept as an N
       rather than closed up. *)
    Testing.check_string "a position with no coverage becomes an N"
      ~expected:">chr\nANT\n"
      (sequence_of
         (pileup
            [ [ "chr"; "1"; "A"; "4"; "...."; "IIII" ]; [ "chr"; "2"; "C"; "0"; "*"; "*" ];
              [ "chr"; "3"; "T"; "4"; "...."; "IIII" ] ]));
    (* One record per sequence, in the order they appear. *)
    Testing.check_string "each sequence of the pileup becomes its own record"
      ~expected:">chr1\nA\n>chr2\nC\n"
      (sequence_of
         (pileup
            [ [ "chr1"; "1"; "A"; "2"; ".."; "II" ]; [ "chr2"; "1"; "C"; "2"; ".."; "II" ] ])))

(* The coverage track.  A BedGraph interval is zero-based and half-open, which
   is the whole of what these check: written inclusively -- as the tool this was
   taken from wrote it -- a run of one base is an interval of none, and every
   interval abuts the next one base short. *)

let test_bedgraph () =
  Testing.section "Consensus coverage track" (fun () ->
    Testing.check_string "a single-base run spans one base, not zero"
      ~expected:"chr\t0\t1\t4\nchr\t1\t2\t2\n"
      (bedgraph_of
         (pileup
            [ [ "chr"; "1"; "A"; "4"; "...."; "IIII" ]; [ "chr"; "2"; "C"; "2"; ".."; "II" ] ]));
    Testing.check_string "positions of equal coverage merge into one interval"
      ~expected:"chr\t0\t3\t4\n"
      (bedgraph_of
         (pileup
            [ [ "chr"; "1"; "A"; "4"; "...."; "IIII" ]; [ "chr"; "2"; "C"; "4"; "...."; "IIII" ];
              [ "chr"; "3"; "G"; "4"; "...."; "IIII" ] ]));
    Testing.check_string "an uncovered position is carried at zero"
      ~expected:"chr\t0\t1\t4\nchr\t1\t2\t0\nchr\t2\t3\t4\n"
      (bedgraph_of
         (pileup
            [ [ "chr"; "1"; "A"; "4"; "...."; "IIII" ]; [ "chr"; "2"; "C"; "0"; "*"; "*" ];
              [ "chr"; "3"; "T"; "4"; "...."; "IIII" ] ]));
    Testing.check_string "and a deleted position takes no interval at all"
      ~expected:"chr\t0\t2\t4\n"
      (bedgraph_of
         (pileup
            [ [ "chr"; "1"; "A"; "4"; "...."; "IIII" ]; [ "chr"; "2"; "C"; "4"; "***."; "IIII" ];
              [ "chr"; "3"; "T"; "4"; "...."; "IIII" ] ]));
    Testing.check_string "each sequence restarts the coordinates"
      ~expected:"chr1\t0\t1\t2\nchr2\t0\t1\t2\n"
      (bedgraph_of
         (pileup
            [ [ "chr1"; "1"; "A"; "2"; ".."; "II" ]; [ "chr2"; "1"; "C"; "2"; ".."; "II" ] ])))

(* Insertions, which are the part a consensus cannot take one position at a
   time: the pileup reports one between this position and the next, and whether
   it belongs in the sequence depends on support accumulated across several. *)

let test_insertions () =
  Testing.section "Consensus insertions" (fun () ->
    Testing.check_string "a well-supported insertion is written into the consensus"
      ~expected:">chr\nACACG\n"
      (sequence_of
         (pileup
            [ [ "chr"; "1"; "A"; "3"; "..."; "III" ];
              [ "chr"; "2"; "C"; "3"; ".+2AC.+2AC.+2AC"; "III" ];
              [ "chr"; "3"; "G"; "3"; "..."; "III" ] ]));
    Testing.check_string "and the bases it adds carry its own support"
      ~expected:"chr\t0\t5\t3\n"
      (bedgraph_of
         (pileup
            [ [ "chr"; "1"; "A"; "3"; "..."; "III" ];
              [ "chr"; "2"; "C"; "3"; ".+2AC.+2AC.+2AC"; "III" ];
              [ "chr"; "3"; "G"; "3"; "..."; "III" ] ]));
    Testing.check_string "one read out of four does not carry an insertion in"
      ~expected:">chr\nACG\n"
      (sequence_of
         (pileup
            [ [ "chr"; "1"; "A"; "4"; "...."; "IIII" ];
              [ "chr"; "2"; "C"; "4"; ".+2AC..."; "IIII" ];
              [ "chr"; "3"; "G"; "4"; "...."; "IIII" ] ]));
    Testing.check_int "an insertion written is counted" ~expected:1
      (stats_of
         (pileup
            [ [ "chr"; "1"; "A"; "3"; "..."; "III" ];
              [ "chr"; "2"; "C"; "3"; ".+2AC.+2AC.+2AC"; "III" ] ]))
        .Consensus.Mpileup.insertions;
    Testing.check_int "and one refused is not" ~expected:0
      (stats_of
         (pileup
            [ [ "chr"; "1"; "A"; "4"; "...."; "IIII" ];
              [ "chr"; "2"; "C"; "4"; ".+2AC..."; "IIII" ] ]))
        .Consensus.Mpileup.insertions;
    Testing.check_string "an insertion at the last position of a sequence still lands in it"
      ~expected:">chr1\nACTTT\n>chr2\nGG\n"
      (sequence_of
         (pileup
            [ [ "chr1"; "1"; "A"; "3"; "..."; "III" ];
              [ "chr1"; "2"; "C"; "3"; ".+3TTT.+3TTT.+3TTT"; "III" ];
              [ "chr2"; "1"; "G"; "3"; "..."; "III" ];
              [ "chr2"; "2"; "G"; "3"; "..."; "III" ] ]));
    (* An insertion still open at the end of one sequence describes a place in
       that sequence and has nothing to say about the beginning of the next, so
       what a sequence is read as cannot depend on what preceded it.
       The fixture is built to be able to see that.  The insertion is too weakly
       supported to be written where it occurs, so it is still open at the
       boundary; the next sequence is covered once, which is what drags the
       fraction it is judged against down far enough for the stale insertion to
       clear it.  Carried across, it is written into a sequence no read of which
       ever reported it. *)
    let chr1 =
      [ [ "chr1"; "1"; "A"; "4"; ".+5TTTTT..."; "IIII" ];
        [ "chr1"; "2"; "C"; "4"; "...."; "IIII" ] ]
    and chr2 = [ [ "chr2"; "1"; "G"; "1"; "."; "I" ] ] in
    Testing.check_string "and one still open at a sequence boundary does not cross it"
      ~expected:
        (sequence_of ~insertion_min_coverage:1 (pileup chr1)
          ^ sequence_of ~insertion_min_coverage:1 (pileup chr2))
      (sequence_of ~insertion_min_coverage:1 (pileup (chr1 @ chr2))))

(* Ties, and the seed that settles them.  Two genotypes with the same count are
   two equally supported readings, and something has to be picked; what matters
   is that the pick is stated rather than arbitrary, so that a run reproduces. *)

let test_ties () =
  Testing.section "Consensus tie-breaking" (fun () ->
    let tie = pileup [ [ "chr"; "1"; "A"; "4"; "..GG"; "IIII" ] ] in
    Testing.check_int "an evenly split position is counted as an ambiguity" ~expected:1
      (stats_of tie).Consensus.Mpileup.ambiguities;
    Testing.check_int "where a decided one is not" ~expected:0
      (stats_of (pileup [ [ "chr"; "1"; "A"; "4"; "...G"; "IIII" ] ]))
        .Consensus.Mpileup.ambiguities;
    Testing.check_string "the same seed gives the same answer"
      ~expected:(sequence_of ~seed:7 tie) (sequence_of ~seed:7 tie);
    (* And the seed has to do something, or fixing it would prove nothing: over
       a range of seeds the draw must land on both readings, not one. *)
    Testing.check_bool "and different seeds reach both readings" ~expected:true
      (let seen =
         List.init 24 (fun seed -> sequence_of ~seed tie) |> List.sort_uniq compare in
       List.length seen = 2);
    Testing.check_int "every position is counted, decided or not" ~expected:3
      (stats_of
         (pileup
            [ [ "chr"; "1"; "A"; "4"; "...."; "IIII" ]; [ "chr"; "2"; "C"; "0"; "*"; "*" ];
              [ "chr"; "3"; "G"; "4"; "..GG"; "IIII" ] ]))
        .Consensus.Mpileup.positions)

let test_from_mpileup_arguments () =
  Testing.section "Consensus from a pileup: arguments" (fun () ->
    Testing.check_raises ~re:"insertion_min_fraction" "a non-positive fraction is refused"
      (fun () -> sequence_of ~insertion_min_fraction:0. "");
    Testing.check_raises ~re:"insertion_min_coverage" "as is a coverage floor below one"
      (fun () -> sequence_of ~insertion_min_coverage:0 "");
    Testing.check_raises ~re:"column" "and a line with too few columns stops the run"
      (fun () -> sequence_of (pileup [ [ "chr"; "1"; "A" ] ]));
    Testing.check_string "an empty pileup produces an empty consensus" ~expected:""
      (sequence_of ""))

let run () =
  test_side_dashes ();
  test_remove_tips ();
  test_of_alignment ();
  test_from_mpileup ();
  test_bedgraph ();
  test_insertions ();
  test_ties ();
  test_from_mpileup_arguments ()
