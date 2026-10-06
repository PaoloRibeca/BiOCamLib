(*
    Bench_BAVector.ml -- (c) 2026 Paolo Ribeca, <paolo.ribeca@gmail.com>

    This file is part of BiOCamLib, the OCaml foundations upon which
    a number of the bioinformatics tools I developed are built.

    Bench_BAVector.ml measures what an access to one of the vectors
    Numbers.Bigarray.Vector makes costs, against the same access to a
    bare Bigarray and against one through the generic accessor.

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

(* THE COST OF AN ACCESS TO A BIGARRAY VECTOR, per element: the same loop over a vector, over a
   bare Bigarray of the same kind, which the compiler specialises to the kind, and through a
   function generic in the kind, which it compiles to the C accessor looking the kind up at run
   time -- what every access written inside the functor would be, were its accessors not matching
   on the kind.  Measured here in ns per element, best of five over 10^7 elements, release-static
   with flambda's -O3, on a laptop busy with other work:
       kind      operation   vector    bare   generic
       int       get            3.6     6.1      92.5
       int       set            4.4     3.7      85.7
       int       incr_by        5.5     5.5     163.8
       int       map           11.3    12.2     173.0
       float32   get            4.1     4.6      70.8
       float32   set            4.2     5.3      60.5
       float32   incr_by        7.0     7.0     133.8
       float32   map            9.7    11.7     139.4
   The vector costs what the bare Bigarray does, to within the noise, and the generic access 15 to
   30 times as much.  Their machine code is the same: a load or store of the kind, after a bound
   check where the access is a checked one. *)

open BiOCamLib
open Better

module BA1 = Bigarray.Array1

let n = 10_000_000

(* Best of several runs, as in the other benchmarks: the best is the run least interfered with *)
let repeats = 5

let time f =
  let timer = Tools.Timer.of_string "Bench_BAVector.Access" in
  Gc.full_major ();
  Tools.Timer.reset timer;
  Tools.Timer.start timer;
  f ();
  Tools.Timer.stop timer;
  Tools.Timer.to_string timer |> Tools.Timer.read

(* Accesses the compiler cannot specialise, the kind being a type variable here *)
let generic_get (v: ('a, 'b, Bigarray.c_layout) BA1.t) i = BA1.get v i
let generic_set (v: ('a, 'b, Bigarray.c_layout) BA1.t) i x = BA1.set v i x

(* Where each loop leaves what it computed, so that none is optimised away *)
let sink = ref 0.

(* The three loops take turns, round after round, so that none of them always runs first *)
let row kind operation via_vector via_bare via_generic =
  let best = [| infinity; infinity; infinity |] in
  for _ = 1 to repeats do
    List.iteri (fun i f -> best.(i) <- Float.min best.(i) (time f))
      [ via_vector; via_bare; via_generic ]
  done;
  let ns i = 1e9 *. best.(i) /. Float.of_int n in
  Printf.printf "%s\t%s\t%.2f\t%.2f\t%.2f\n%!" kind operation (ns 0) (ns 1) (ns 2)

let () =
  Printf.printf "kind\toperation\tvector_ns\tbare_ns\tgeneric_ns\n%!";
  let module V = Numbers.IntBAVector in
  let v = V.make n 1 and bare = BA1.create Bigarray.int Bigarray.c_layout n in
  BA1.fill bare 1;
  row "int" "get"
    (fun () ->
      let s = ref 0 in
      for i = 0 to n - 1 do s := !s + V.get v i done;
      sink := Float.of_int !s)
    (fun () ->
      let s = ref 0 in
      for i = 0 to n - 1 do s := !s + bare.{i} done;
      sink := Float.of_int !s)
    (fun () ->
      let s = ref 0 in
      for i = 0 to n - 1 do s := !s + generic_get bare i done;
      sink := Float.of_int !s);
  row "int" "set"
    (fun () -> for i = 0 to n - 1 do V.set v i i done)
    (fun () -> for i = 0 to n - 1 do bare.{i} <- i done)
    (fun () -> for i = 0 to n - 1 do generic_set bare i i done);
  row "int" "incr_by"
    (fun () -> for i = 0 to n - 1 do V.(v.+(i) <- 2) done)
    (fun () -> for i = 0 to n - 1 do bare.{i} <- bare.{i} + 2 done)
    (fun () -> for i = 0 to n - 1 do generic_set bare i (generic_get bare i + 2) done);
  row "int" "map"
    (fun () -> sink := Float.of_int (V.map (fun x -> x + 1) v).V.@(n - 1))
    (fun () ->
      let res = BA1.create Bigarray.int Bigarray.c_layout n in
      for i = 0 to n - 1 do res.{i} <- bare.{i} + 1 done;
      sink := Float.of_int res.{n - 1})
    (fun () ->
      let res = BA1.create Bigarray.int Bigarray.c_layout n in
      for i = 0 to n - 1 do generic_set res i (generic_get bare i + 1) done;
      sink := Float.of_int res.{n - 1});
  let module V = Numbers.Float32BAVector in
  let v = V.make n 1. and bare = BA1.create Bigarray.float32 Bigarray.c_layout n in
  BA1.fill bare 1.;
  row "float32" "get"
    (fun () ->
      let s = ref 0. in
      for i = 0 to n - 1 do s := !s +. V.get v i done;
      sink := !s)
    (fun () ->
      let s = ref 0. in
      for i = 0 to n - 1 do s := !s +. bare.{i} done;
      sink := !s)
    (fun () ->
      let s = ref 0. in
      for i = 0 to n - 1 do s := !s +. generic_get bare i done;
      sink := !s);
  row "float32" "set"
    (fun () -> for i = 0 to n - 1 do V.set v i 0.5 done)
    (fun () -> for i = 0 to n - 1 do bare.{i} <- 0.5 done)
    (fun () -> for i = 0 to n - 1 do generic_set bare i 0.5 done);
  row "float32" "incr_by"
    (fun () -> for i = 0 to n - 1 do V.(v.+(i) <- 0.5) done)
    (fun () -> for i = 0 to n - 1 do bare.{i} <- bare.{i} +. 0.5 done)
    (fun () -> for i = 0 to n - 1 do generic_set bare i (generic_get bare i +. 0.5) done);
  row "float32" "map"
    (fun () -> sink := (V.map (fun x -> x +. 1.) v).V.@(n - 1))
    (fun () ->
      let res = BA1.create Bigarray.float32 Bigarray.c_layout n in
      for i = 0 to n - 1 do res.{i} <- bare.{i} +. 1. done;
      sink := res.{n - 1})
    (fun () ->
      let res = BA1.create Bigarray.float32 Bigarray.c_layout n in
      for i = 0 to n - 1 do generic_set res i (generic_get bare i +. 1.) done;
      sink := res.{n - 1})

