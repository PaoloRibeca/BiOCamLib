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

(* Rolling hashes.  The whole point of one is that sliding it along a sequence
   costs the same as one symbol rather than the whole window, so the invariant
   worth pinning is that sliding and recomputing agree: everything else about a
   hash is a convention, but that equality is a correctness property, and a
   refactor of the shift-and-mask arithmetic can break it while leaving every
   individual hash looking perfectly reasonable. *)

module Bits2 = struct let n = 2 end
module K4 = struct let n = 4 end
module H4 = KMers.IntHash (Bits2) (K4)
module Z4 = KMers.IntZHash (Bits2) (K4)

(* ACGTAC over the usual two-bit encoding. *)
let encoded = [| 0; 1; 2; 3; 0; 1 |]

let test_kmer_hashes () =
  Testing.section "k-mer hashes" (fun () ->
    Testing.check_bool "sliding one symbol right agrees with recomputing" ~expected:true
      (H4.add_symbol_right (H4.compute encoded 0) encoded.(4) = H4.compute encoded 1);
    Testing.check_bool "and again, one further along" ~expected:true
      (H4.add_symbol_right (H4.compute encoded 1) encoded.(5) = H4.compute encoded 2);
    Testing.check_bool "sliding one symbol left agrees too" ~expected:true
      (H4.add_symbol_left (H4.compute encoded 1) encoded.(0) = H4.compute encoded 0);
    Testing.check_bool "the reverse complement is an involution" ~expected:true
      (H4.rc (H4.rc (H4.compute encoded 0)) = H4.compute encoded 0);
    Testing.check_bool "and so is complementing a single symbol" ~expected:true
      (List.for_all (fun s -> H4.symbol_complement (H4.symbol_complement s) = s)
         [ 0; 1; 2; 3 ]);
    Testing.check_bool "distinct k-mers hash apart" ~expected:true
      (H4.compute encoded 0 <> H4.compute encoded 1);
    Testing.check_bool "min1 picks the smaller" ~expected:true
      (let a = H4.compute encoded 0 and b = H4.compute encoded 1 in
       H4.min1 a b = Stdlib.min a b);
    Testing.check_bool "min2 does the same pairwise" ~expected:true
      (let a = H4.compute encoded 0 and b = H4.compute encoded 1 in
       H4.min2 (a, b) (b, a) = Stdlib.min (a, b) (b, a));
    Testing.check_bool "a hash renders as a string" ~expected:true
      (H4.to_string (H4.compute encoded 0) <> "");
    (* The two documented ways to hand it something it cannot hash. *)
    Testing.check_raises "fewer than k symbols left is refused"
      (fun () -> ignore (H4.compute encoded 3));
    Testing.check_raises "a symbol too wide for the alphabet is refused"
      (fun () -> ignore (H4.compute [| 0; 1; 2; 9 |] 0));
    (* The arbitrary-width implementation has to agree with the machine-word
       one wherever the latter can represent the answer at all -- they are two
       instantiations of one interface, and KPop picks between them by k. *)
    Testing.check_string "the wide implementation agrees with the narrow one"
      ~expected:(H4.to_string (H4.compute encoded 0))
      (Z4.to_string (Z4.compute encoded 0));
    Testing.check_bool "and slides the same way" ~expected:true
      (Z4.add_symbol_right (Z4.compute encoded 0) encoded.(4) = Z4.compute encoded 1))

(* Levenshtein balls.  A ball is every k-mer within a given edit radius of the
   centre, which is how a k-mer index tolerates a sequencing error.  The
   iterators are documented as repeating themselves and the constructors as not,
   so the set is the smaller of the two and that difference is the check. *)

module B3 = KMers.DNALevenshteinBall (struct let n = 3 end)

let test_levenshtein_balls () =
  Testing.section "Levenshtein balls" (fun () ->
    Testing.check_int "the ball knows its k" ~expected:3 B3.H.k;
    Testing.check_string "and its alphabet" ~expected:"ACGT" B3.H.alphabet;
    let centre = "ACG" in
    (* A ball is everything WITHIN its radius, so the centre belongs to it at
       every radius including zero.  That is worth stating as a check rather
       than assuming: the construction walks outwards one edit at a time, and
       it used to hand back only the k-mers at exactly [radius] edits -- which,
       since one edit cannot leave a k-mer where it was, excluded the very
       k-mer the ball was built around, and an index querying at radius one
       then failed to match what it had been handed. *)
    List.iter (fun r ->
      Testing.check_bool
        (Printf.sprintf "the centre lies in its own ball at radius %d" r)
        ~expected:true (B3.Base.mem centre (B3.makek ~radius:r centre)))
      [ 0; 1; 2; 3 ];
    Testing.check_int "a radius of zero holds the centre and nothing else"
      ~expected:1 (B3.Base.cardinal (B3.makek ~radius:0 centre));
    let ball = B3.makek ~radius:1 centre in
    Testing.check_bool "every member of a ball is itself a k-mer" ~expected:true
      (B3.Base.for_all (fun s -> String.length s = B3.H.k) ball);
    (* What one edit reaches: every single substitution, and beyond them the
       length-preserving indel pairs, which is why the count exceeds the nine
       substitutions and the centre. *)
    Testing.check_bool "one edit reaches every single substitution" ~expected:true
      (let ok = ref true in
       String.iteri (fun i _ ->
         String.iter (fun c ->
           let s = Bytes.of_string centre in
           Bytes.set s i c;
           if not (B3.Base.mem (Bytes.to_string s) ball) then ok := false)
           B3.H.alphabet) centre;
       !ok);
    Testing.check_int "and the ball holds twenty-two k-mers in all" ~expected:22
      (B3.Base.cardinal ball);
    (* Widening a radius can only add, never take away. *)
    Testing.check_bool "a wider radius contains a narrower one" ~expected:true
      (B3.Base.subset ball (B3.makek ~radius:2 centre));
    Testing.check_bool "and is strictly larger while there is room" ~expected:true
      (B3.Base.cardinal (B3.makek ~radius:2 centre) > B3.Base.cardinal ball);
    Testing.check_int "three edits reach the whole space" ~expected:64
      (B3.Base.cardinal (B3.makek ~radius:3 centre));
    (* The iterator covers the same ground and is documented to repeat itself,
       so every visit is a member and there are at least as many visits as
       members. *)
    let visited = ref 0 and all_in = ref true in
    B3.iterk ~radius:1 (fun s ->
      incr visited;
      if not (B3.Base.mem s ball) then all_in := false) centre;
    Testing.check_bool "the iterator stays inside the ball" ~expected:true !all_in;
    Testing.check_bool "and covers it at least once over" ~expected:true
      (!visited >= B3.Base.cardinal ball);
    Testing.check_bool "the iterator visits the centre too" ~expected:true
      (let seen = ref false in
       B3.iterk ~radius:1 (fun s -> if s = centre then seen := true) centre;
       !seen))

let run () =
  test_sliding_window ();
  test_double_sliding_window ();
  test_kmer_hashes ();
  test_levenshtein_balls ()
