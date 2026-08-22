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

let run () =
  test_side_dashes ();
  test_remove_tips ();
  test_of_alignment ()
