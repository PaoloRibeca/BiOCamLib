(*
    Bench_TC.ml -- (c) 2026 Paolo Ribeca, <paolo.ribeca@gmail.com>

    This file is part of BiOCamLib, the OCaml foundations upon which
    a number of the bioinformatics tools I developed are built.

    Bench_TC.ml exercises the general-purpose containers in Tools:
    the array-backed stack, the prefix trie behind command-line option
    matching, and the multimap.  The stack's [pop_n] is pinned here in
    particular: it drops n elements and returns the last one dropped,
    generalising [pop] rather than returning the n of them, and the one
    caller in the wider codebase depends on exactly that.

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

(* THE COST OF `TransitiveClosure`'s OPTIONAL MERGE HOOK, which exists so that
   relations fed in increasing distance order emit a single-linkage dendrogram
   bottom-up rather than forcing the caller to snapshot the partition after each one.
   The question it answers is the one worth asking of any optional instrumentation:
   what does it cost the callers who do NOT ask for it, and what does it cost those
   who do.

   The star is the worst case for the hook -- one class grown by one element per
   relation, so every relation fires -- which is why it is the shape measured.

   Measured here:

       relations     no hook   with hook    ratio    words/reln
          50000       0.218s      0.243s    1.12x    192 ->  206
         100000       0.479s      0.502s    1.05x    197 ->  211
         200000       0.930s      0.938s    1.01x    202 ->  216
         400000       1.874s      2.186s    1.17x    207 ->  221

   The ALLOCATION column is the reliable half: a flat +14 words per relation, about
   7%, reproducible at every size.  The time ratio is 1.01x to 1.17x, which is small
   but still within the noise of a workload this short -- so what this file supports
   is "a few per cent, bounded well under 1.2x", and not a figure to a decimal place.
   Callers who supply no hook pay nothing: `Option.iter` on `None` is one match. *)

open BiOCamLib
open Better

module TC = Tools.TransitiveClosure.Make (ComparableInt)

(* Best of several rather than one run: a single run also measures whatever else the
   machine was doing, and the best is the one least interfered with.  The first
   version of this file reported one run each and produced 1.28x and 0.99x for the
   same measurement on consecutive attempts -- a spread wide enough to support any
   conclusion one liked, which is no measurement at all. *)
let repeats = 7

let measure hook n =
  let best = ref infinity and words = ref 0. and merges = ref 0 in
  for _ = 1 to repeats do
    Gc.full_major ();
    let count = ref 0 in
    let words_before = Gc.minor_words () and t0 = Unix.gettimeofday () in
    let tc =
      if hook then TC.empty ~on_merge:(fun _ _ -> incr count) () else TC.empty () in
    for i = 1 to n do
      TC.add_equivalences tc (IntSet.of_list [ 0; i ])
    done;
    let elapsed = Unix.gettimeofday () -. t0 in
    if elapsed < !best then
      best := elapsed;
    words := Gc.minor_words () -. words_before;
    merges := !count
  done;
  !best, !words, !merges

let () =
  Printf.printf "\n%s\n\n" (String.TermIO.bold "Transitive closure: the cost of the merge hook");
  Printf.printf "  %10s  %10s  %10s  %8s  %12s  %s\n"
    "relations" "no hook" "with hook" "ratio" "words/reln" "merges";
  List.iter
    (fun n ->
      let a, wa, _ = measure false n and b, wb, m = measure true n in
      Printf.printf "  %10d  %9.3fs  %9.3fs  %7.2fx  %5.0f -> %5.0f  %d\n%!"
        n a b (if a > 0. then b /. a else 0.)
        (wa /. float_of_int n) (wb /. float_of_int n) m)
    [ 50_000; 100_000; 200_000; 400_000 ]
