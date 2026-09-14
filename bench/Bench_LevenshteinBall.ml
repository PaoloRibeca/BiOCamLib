(*
    Bench_LevenshteinBall.ml -- (c) 2026 Paolo Ribeca, <paolo.ribeca@gmail.com>

    This file is part of BiOCamLib, the OCaml foundations upon which
    a number of the bioinformatics tools I developed are built.

    Bench_LevenshteinBall.ml measures how fast KMers.DNALevenshteinBall
    hashes the k-mers of a sequence and the balls around them, which is
    what a k-mer index tolerating errors spends its lookups on.

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

(* THE COST OF A LEVENSHTEIN BALL, per k-mer of a random sequence: the hashes of every k-mer and of
   every string within the radius of it, as an index tolerating errors asks for them at every
   position of a query.  Measured here, best of three, at k = 30 with the ball walked on strings,
   as it was until September 2026, and on integers, as [IntDNALevenshteinBall] walks it now:
       radius   k-mers    hashes   strings, ns per k-mer   integers, ns per k-mer   speed-up
            0   999971    999971                    3363                       31       107x
            1     1971    754833                  331200                    19395        17x
            2       11   1566957               127549908                  7024267        18x
   Both produce the same hashes.  Per hash the walk on integers costs about 50 ns, against 860 to
   900 on strings, and at radius 0, where a k-mer is its only hash, the rolling pass costs 31 ns
   against 3.4 microseconds.  In NINJA's read librarian, searching the 500-base windows of an
   E. coli genome among two or three others indexed as overlapping pieces, one search went from
   6.9 to 0.8 ms and from 5.0 to 0.7 ms, finding the same windows.
   At k = 40, beyond what a machine integer holds, [IntZDNALevenshteinBall] walks the ball on a
   buffer and hashes with IntZ: 3181 ns per k-mer at radius 0 and 742214 at radius 1, 1447 ns per
   hash, where the strings could not go at all, their hashes being machine integers. *)

open BiOCamLib
open Better

module B = KMers.DNALevenshteinBall (struct let n = 30 end)
module BZ = KMers.IntZDNALevenshteinBall (struct let n = 40 end)

(* Best of several runs, as in the other benchmarks: the best is the run least interfered with *)
let repeats = 3

let measure walk radius sequence =
  let timer = Tools.Timer.of_string "Bench_LevenshteinBall.Walk"
  and best = ref infinity and hashes = ref 0 in
  for _ = 1 to repeats do
    Gc.full_major ();
    let count = ref 0 in
    Tools.Timer.reset timer;
    Tools.Timer.start timer;
    walk radius count sequence;
    Tools.Timer.stop timer;
    best := Float.min !best (Tools.Timer.to_string timer |> Tools.Timer.read);
    hashes := !count
  done;
  !best, !hashes

let () =
  let state = Random.State.make [| 42 |] in
  let machine radius count sequence = B.iterkh ~radius (fun _ -> incr count) sequence
  and arbitrary radius count sequence = BZ.iterkh ~radius (fun _ -> incr count) sequence in
  Printf.printf "ball\tk\tradius\tk-mers\thashes\tns_per_kmer\tns_per_hash\n%!";
  List.iter
    (fun (name, walk, k, radius, length) ->
      let sequence = String.init length (fun _ -> "ACGT".[Random.State.int state 4]) in
      let (seconds, hashes) = measure walk radius sequence and kmers = length - k + 1 in
      Printf.printf "%s\t%d\t%d\t%d\t%d\t%.1f\t%.2f\n%!" name k radius kmers hashes
        (1e9 *. seconds /. Float.of_int kmers) (1e9 *. seconds /. Float.of_int (max 1 hashes)))
    [ "int", machine, 30, 0, 1_000_000; "int", machine, 30, 1, 2_000; "int", machine, 30, 2, 40;
      "IntZ", arbitrary, 40, 0, 200_000; "IntZ", arbitrary, 40, 1, 2_000 ]
