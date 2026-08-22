(*
    Tests_KMers.ml -- (c) 2026 Paolo Ribeca, <paolo.ribeca@gmail.com>

    This file is part of BiOCamLib, the OCaml foundations upon which
    a number of the bioinformatics tools I developed are built.

    Tests_KMers.ml exercises the sliding windows the k-mer machinery
    runs on.  Both are circular buffers kept in step with an index, and
    the double window maintains a running count of mismatches
    incrementally -- adding one for the pair coming in and taking one
    away for the pair going out -- so the case that matters is a
    difference rolling off the far end, where an incremental counter
    that forgets to decrement drifts and never recovers.

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

module W = KMers.SlidingWindow
module D = KMers.DoubleSlidingWindow

(* The single window. *)

let test_sliding_window () =
  Testing.section "Sliding window" (fun () ->
    Testing.check_int "the window is as long as the string it was made from"
      ~expected:3 (W.length (W.make "abc"));
    Testing.check_string "a fresh window reads back as that string"
      ~expected:"abc" (W.contents (W.make "abc"));
    (* add_char returns the character it evicts, which is what lets a caller
       update a rolling hash without re-reading the window. *)
    Testing.check_string "adding a character evicts the oldest"
      ~expected:"a" (String.make 1 (W.add_char (W.make "abc") 'd'));
    Testing.check_string "and the window has rolled by one"
      ~expected:"bcd"
      (let w = W.make "abc" in
       ignore (W.add_char w 'd');
       W.contents w);
    Testing.check_string "rolling twice"
      ~expected:"cde"
      (let w = W.make "abc" in
       ignore (W.add_char w 'd');
       ignore (W.add_char w 'e');
       W.contents w);
    Testing.check_string "a full cycle replaces every character"
      ~expected:"def"
      (let w = W.make "abc" in
       List.iter (fun c -> ignore (W.add_char w c)) [ 'd'; 'e'; 'f' ];
       W.contents w);
    Testing.check_string "and the window keeps rolling past a full cycle"
      ~expected:"efg"
      (let w = W.make "abc" in
       List.iter (fun c -> ignore (W.add_char w c)) [ 'd'; 'e'; 'f'; 'g' ];
       W.contents w);
    (* The degenerate width: every character is both the newest and the oldest. *)
    Testing.check_int "a window of one has length one" ~expected:1 (W.length (W.make "a"));
    Testing.check_string "a window of one evicts what it just held"
      ~expected:"a" (String.make 1 (W.add_char (W.make "a") 'b'));
    Testing.check_string "and then holds the new character"
      ~expected:"b"
      (let w = W.make "a" in
       ignore (W.add_char w 'b');
       W.contents w);
    Testing.check_int "the length never changes as the window rolls"
      ~expected:3
      (let w = W.make "abc" in
       List.iter (fun c -> ignore (W.add_char w c)) [ 'd'; 'e'; 'f'; 'g'; 'h' ];
       W.length w))

(* The double window, which tracks how many positions differ between two
   sequences as both roll forward. *)

let test_double_sliding_window () =
  Testing.section "Double sliding window" (fun () ->
    Testing.check_int "a fresh double window has no support" ~expected:0 (D.support (D.make "abc"));
    Testing.check_int "and no differences" ~expected:0 (D.diffs (D.make "abc"));
    (* Support counts how many positions have been fed, capped at the width. *)
    Testing.check_int "support grows with each pair"
      ~expected:2
      (let d = D.make "abc" in
       ignore (D.add_char d 'x' 'x');
       ignore (D.add_char d 'y' 'y');
       D.support d);
    Testing.check_int "support stops at the width of the window"
      ~expected:3
      (let d = D.make "abc" in
       List.iter (fun c -> ignore (D.add_char d c c)) [ 'x'; 'y'; 'z'; 'p'; 'q' ];
       D.support d);
    (* Matching pairs never raise the count. *)
    Testing.check_int "matching pairs leave the count at zero"
      ~expected:0
      (let d = D.make "abc" in
       List.iter (fun c -> ignore (D.add_char d c c)) [ 'x'; 'y'; 'z' ];
       D.diffs d);
    (* A mismatching pair raises it, and the call returns the new count. *)
    Testing.check_int "a mismatching pair is counted"
      ~expected:1 (D.add_char (D.make "abc") 'a' 'b');
    Testing.check_int "two mismatching pairs are counted"
      ~expected:2
      (let d = D.make "abc" in
       ignore (D.add_char d 'a' 'b');
       D.add_char d 'c' 'd');
    (* The case the incremental counter exists for: the mismatch rolls off the
       far end of the window and has to be taken away again. *)
    Testing.check_int "a mismatch is forgotten once it rolls out of the window"
      ~expected:0
      (let d = D.make "abc" in
       ignore (D.add_char d 'x' 'x');
       ignore (D.add_char d 'y' 'z');   (* the mismatch *)
       ignore (D.add_char d 'p' 'p');
       ignore (D.add_char d 'q' 'q');
       ignore (D.add_char d 'r' 'r');   (* the mismatch rolls out here *)
       D.diffs d);
    Testing.check_int "and is still counted while it is inside the window"
      ~expected:1
      (let d = D.make "abc" in
       ignore (D.add_char d 'x' 'x');
       ignore (D.add_char d 'y' 'z');
       ignore (D.add_char d 'p' 'p');
       D.diffs d);
    (* Feeding two sequences that differ everywhere saturates the count at the
       width rather than growing without bound. *)
    Testing.check_int "the count cannot exceed the width of the window"
      ~expected:3
      (let d = D.make "abc" in
       List.iter (fun (a, b) -> ignore (D.add_char d a b))
         [ 'a', 'b'; 'c', 'd'; 'e', 'f'; 'g', 'h'; 'i', 'j' ];
       D.diffs d);
    Testing.check "to_string shows both windows"
      (fun () ->
        let d = D.make "abc" in
        ignore (D.add_char d 'x' 'y');
        let s = D.to_string d in
        String.length s > 6))

let run () =
  test_sliding_window ();
  test_double_sliding_window ()
