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
    that forgets to decrement drifts and never recovers.  The Levenshtein
    balls are also checked against the implementation on strings they
    replaced, which is kept here for that purpose alone.

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
    (* A k-mer hashed where it sits in a longer string is the k-mer hashed on
       its own, at every position a k-mer can start at; a position one cannot
       start at is refused rather than read past the end. *)
    let text = "ACGTTGCAACGGTA" in
    Testing.check_bool "a k-mer hashed in place is the k-mer hashed on its own" ~expected:true
      (List.for_all
        (fun pos -> B3.H.encode_at text pos = B3.H.encode (String.sub text pos B3.H.k))
        (List.init (String.length text - B3.H.k + 1) Fun.id));
    Testing.check_raises "and a k-mer that would run past the end is refused"
      (fun () -> B3.H.encode_at text (String.length text - B3.H.k + 1) |> ignore);
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

(* The ball against the implementation it replaced.  The ball was walked on strings, copying and
   linting the k-mer and its contexts for every string it met, and is now walked on integers; the
   two must agree on the same strings, in the same order and as many times over, for centres and
   contexts of every shape -- contexts shorter than the radius and longer, lowercase bases, and
   characters that are not bases, which substitutions replace and hashes skip.  [Reference] is
   the string implementation, kept verbatim *)
module Reference (K: IntParameter_t) =
  struct
    module H =
      struct
        type t = int
        (* The check that k fits a machine integer is dropped in this copy, so that the strings
           of longer balls can be compared *)
        let k = K.n
        (* There are 4 symbols in the alphabet, each one encoded as a 2-bit number *)
        let alphabet = "ACGT"
        let encode_char = function
          | 'A' | 'a' -> 0
          | 'C' | 'c' -> 1
          | 'G' | 'g' -> 2
          | 'T' | 't' -> 3
          | _ -> -1
        let encode s =
          if String.length s <> k then
            Exception.raise __FUNCTION__ Initialize
              (Printf.sprintf "Invalid argument (string length must be k=%d, found %d)" k (String.length s));
          let res = ref 0 in
          for i = 0 to k - 1 do
            res :=
              !res lsl 2 +
                match s.[i] with
                | 'A' | 'a' -> 0
                | 'C' | 'c' -> 1
                | 'G' | 'g' -> 2
                | 'T' | 't' -> 3
                | c ->
                  Exception.raise __FUNCTION__ Initialize
                    (Printf.sprintf "Invalid argument (expected character in [ACGTacgt], found '%c')" c);
          done;
          !res
      end
    let lint s =
      (* This is not entirely general, but OK for the time being *)
      let s = String.uppercase_ascii s |> Bytes.of_string
      and encode = H.encode_char in
      Bytes.iteri
        (fun i c ->
          Bytes.(
            s.@(i) <-
              if encode c = -1 then
                ' '
              else
                c
          ))
        s;
      Bytes.to_string s
    let iter ?(radius = 1) f l_ctxt s r_ctxt =
      if radius < 0 then
        Exception.raise __FUNCTION__ Algorithm (Printf.sprintf "Invalid radius %d" radius);
      let l_ctxt, s, r_ctxt = lint l_ctxt, lint s, lint r_ctxt in
      (* We trim/pad contexts whenever needed *)
      let padding = String.make radius ' ' in
      let l_ctxt = String.sub (padding ^ l_ctxt) (String.length l_ctxt) radius
      and r_ctxt = String.sub (r_ctxt ^ padding) 0 radius in
      (* The string also includes left and right contexts *)
      let len = String.length s in
      let hi = radius + len - 1 in
      let last = hi + radius in
      let rec expand level orig_s =
        let open Bytes in
        (* Emit at every level and not only at the innermost one.  A ball is
           everything WITHIN its radius, which includes the centre -- zero edits
           -- and each intermediate string; emitting only at [level = 0] made it
           the set of k-mers at exactly [radius] edits instead, and since one
           edit cannot leave a k-mer where it was, a radius-one ball excluded the
           very k-mer it had been built around.  An index querying at radius one
           therefore failed to match the k-mer it was handed.  The call at the
           end of this function has always said the k-mer itself gets inserted
           here; now it does. *)
        (* We eliminate contexts *)
        String.sub orig_s radius len |> f;
        if level > 0 then begin
          let s = of_string orig_s in
          (* Mismatches *)
          for i = radius to hi do
            let c = s.@(i) in
            String.iter
              (fun cc ->
                if cc <> c then begin
                  s.@(i) <- cc;
                  to_string s |> expand (level - 1)
                end)
              H.alphabet;
            (* We restore the previous state *)
            s.@(i) <- c
          done;
          (* Deletions *)
          for i = radius to hi do
            let c = s.@(i) in
            (* Right-to-left deletion *)
            let l = last - i in
            blit s (i + 1) s i l;
            s.@(last) <- ' '; (* Padding *)
            if s.@(hi) <> ' ' then
              to_string s |> expand (level - 1);
            (* We restore the previous state *)
            blit s i s (i + 1) l;
            s.@(i) <- c;
            (* Left-to-right deletion *)
            blit s 0 s 1 i;
            s.@(0) <- ' '; (* Padding *)
            if s.@(radius) <> ' ' then
              to_string s |> expand (level - 1);
            (* We restore the previous state *)
            blit s 1 s 0 i;
            s.@(i) <- c
          done;
          (* Insertions *)
          for i = radius to hi - 1 do
            (* Left-to-right insertion *)
            let c = s.@(hi) in
            let l = hi - i in
            blit s i s (i + 1) l;
            String.iter
              (fun cc ->
                s.@(i) <- cc;
                to_string s |> expand (level - 1))
              H.alphabet;
            (* We restore the previous state *)
            blit s (i + 1) s i l;
            s.@(hi) <- c;
            (* Right-to-left insertion *)
            let c = s.@(0) in
            let l = i + 1 in
            blit s 1 s 0 l;
            String.iter
              (fun cc ->
                s.@(l) <- cc;
                to_string s |> expand (level - 1))
              H.alphabet;
            (* We restore the previous state *)
            blit s 0 s 1 l;
            s.@(0) <- c
          done;
          assert (to_string s = orig_s)
        end in
      (* The k-mer itself gets inserted here *)
      l_ctxt ^ s ^ r_ctxt |> expand radius
    let iterh ?(radius = 1) f =
      iter ~radius
        (fun s ->
          try
            H.encode s |> f
          with _ ->
            ())
    let iterk ?(radius = 1) f s =
      (* We begin by replacing non-alphabet characters with spaces,
          to be compatible with the conventions used by make() above *)
      let l = String.length s in
      for lo = 0 to l - H.k do
        iter ~radius f begin
          let ctxt_lo = (lo - radius) |> max 0 in
          String.sub s ctxt_lo (lo - ctxt_lo)
        end begin
          String.sub s lo H.k
        end begin
          let hi = lo + H.k in
          let ctxt_hi = (hi + radius) |> min l in
          String.sub s hi (ctxt_hi - hi)
        end
      done
    let iterkh ?(radius = 1) f =
      iterk ~radius
        (fun s ->
          try
            H.encode s |> f
          with _ ->
            ())
  end

let test_levenshtein_balls_against_reference () =
  Testing.section "Levenshtein balls against the implementation on strings" (fun () ->
    let state = Random.State.make [| 14092026 |] in
    (* Mostly bases, some of them lowercase, and now and then a character that is not one *)
    let random_string n =
      let chars = "ACGTACGTACGTacgtN" in
      String.init n (fun _ -> chars.[Random.State.int state (String.length chars)]) in
    let collect iterate =
      let res = ref [] in
      iterate (fun x -> res := x :: !res);
      List.rev !res in
    (* The hashes [IntZDNALevenshteinBall] must give, from the strings the implementation on strings
       emits: a string holding a character that is not a base has none *)
    let hashes_of strings =
      List.filter_map
        (fun s ->
          if String.contains s ' ' then
            None
          else
            Some
              (String.fold_left
                (fun h c -> IntZ.((h lsl 2) lor of_int (String.index "ACGT" c))) IntZ.zero s))
        strings in
    let cases = ref 0 and disagreements = ref 0 in
    let agree a b =
      incr cases;
      if a <> b then
        incr disagreements
    and agree_z a b =
      incr cases;
      if not (List.equal IntZ.equal a b) then
        incr disagreements in
    (* Machine-integer balls, at the k NINJA's read librarian seeds with and around it, with
       centres both of length k and of any length up to fifteen past it, beyond 30 included *)
    List.iter
      (fun (k, max_radius) ->
        let module New = KMers.DNALevenshteinBall (struct let n = k end) in
        let module Old = Reference (struct let n = k end) in
        for radius = 0 to max_radius do
          (* Wide balls are expensive on strings, so fewer and shorter cases there *)
          let centres = if radius >= 2 then 5 else 20
          and sequences = if radius >= 2 then 1 else 3
          and extra = if radius >= 2 then 4 else 20 in
          for _ = 1 to centres do
            let l_ctxt = random_string (Random.State.int state (radius + 3))
            and s = random_string k
            and r_ctxt = random_string (Random.State.int state (radius + 3))
            and t = random_string (Random.State.int state (k + 16)) in
            agree (collect (fun f -> Old.iter ~radius f l_ctxt s r_ctxt))
              (collect (fun f -> New.iter ~radius f l_ctxt s r_ctxt));
            agree (collect (fun f -> Old.iter ~radius f l_ctxt t r_ctxt))
              (collect (fun f -> New.iter ~radius f l_ctxt t r_ctxt));
            agree (collect (fun f -> Old.iterh ~radius f l_ctxt s r_ctxt))
              (collect (fun f -> New.iterh ~radius f l_ctxt s r_ctxt))
          done;
          for _ = 1 to sequences do
            let s = random_string (k + Random.State.int state extra) in
            agree (collect (fun f -> Old.iterk ~radius f s))
              (collect (fun f -> New.iterk ~radius f s));
            agree (collect (fun f -> Old.iterkh ~radius f s))
              (collect (fun f -> New.iterkh ~radius f s))
          done
        done)
      [ 3, 3; 11, 2; 15, 2; 30, 1 ];
    (* Balls of any k, with [IntZ.t] hashes: within 30 bases they are walked on integers and beyond
       on the buffer, and either way they must spell what the strings spell *)
    List.iter
      (fun (k, max_radius) ->
        let module New = KMers.IntZDNALevenshteinBall (struct let n = k end) in
        let module Old = Reference (struct let n = k end) in
        for radius = 0 to max_radius do
          for _ = 1 to 10 do
            let l_ctxt = random_string (Random.State.int state (radius + 3))
            and s = random_string k
            and r_ctxt = random_string (Random.State.int state (radius + 3)) in
            let strings = collect (fun f -> Old.iter ~radius f l_ctxt s r_ctxt) in
            agree strings (collect (fun f -> New.iter ~radius f l_ctxt s r_ctxt));
            agree_z (hashes_of strings) (collect (fun f -> New.iterh ~radius f l_ctxt s r_ctxt))
          done;
          for _ = 1 to 2 do
            let s = random_string (k + Random.State.int state 10) in
            let strings = collect (fun f -> Old.iterk ~radius f s) in
            agree strings (collect (fun f -> New.iterk ~radius f s));
            agree_z (hashes_of strings) (collect (fun f -> New.iterkh ~radius f s))
          done
        done)
      [ 3, 2; 30, 1; 31, 1; 40, 1 ];
    (* A radius too wide for an integer is walked on the buffer too *)
    let module Old = Reference (struct let n = 3 end) in
    agree (collect (fun f -> Old.iter ~radius:31 f "" "" ""))
      (collect (fun f -> B3.iter ~radius:31 f "" "" ""));
    Testing.check_bool "the comparison ran" ~expected:true (!cases > 0);
    Testing.check_int "and the rewrite agrees with the strings case by case" ~expected:0
      !disagreements;
    Testing.check_raises ~re:"k must be <= 30" "a machine-integer ball still refuses k beyond 30"
      (fun () -> let module B = KMers.DNALevenshteinBall (struct let n = 31 end) in B.H.k))

let run () =
  test_sliding_window ();
  test_double_sliding_window ();
  test_kmer_hashes ();
  test_levenshtein_balls ();
  test_levenshtein_balls_against_reference ()
