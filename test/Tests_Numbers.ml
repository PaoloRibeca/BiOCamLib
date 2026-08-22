(*
    Tests_Numbers.ml -- (c) 2026 Paolo Ribeca, <paolo.ribeca@gmail.com>

    This file is part of BiOCamLib, the OCaml foundations upon which
    a number of the bioinformatics tools I developed are built.

    Tests_Numbers.ml exercises the scalar layer and the online
    statistics.  The latter carry the most weight: they accumulate in
    one pass by Welford's recurrence rather than by summing squares,
    and the point of doing so is accuracy on data whose mean is far
    from zero, so there is a check here for exactly that case -- the
    naive formula loses every significant digit on it.

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

module Stats = Numbers.OnlineStats (Numbers.Float)

(* Helpers. *)

(* Compare to a fixed number of decimals, so that a failure reports both
   numbers rather than just [false]. *)
let check_float ?(digits = 9) name ~expected got =
  Testing.check_string name
    ~expected:(Printf.sprintf "%.*f" digits expected) (Printf.sprintf "%.*f" digits got)

let stats_of l =
  let s = Stats.make () in
  List.iter (Stats.add s) l;
  s

(* Online statistics. *)

let test_online_stats () =
  Testing.section "Online statistics" (fun () ->
    Testing.check_int "a fresh accumulator has seen nothing"
      ~expected:0 (Stats.count (Stats.make ()));
    let s = stats_of [ 1.; 2.; 3.; 4. ] in
    Testing.check_int "count is the number of values added" ~expected:4 (Stats.count s);
    check_float "the mean is the arithmetic mean" ~expected:2.5 (Stats.mean s);
    (* Population variance divides by n; the sample variance by n - 1.  For
       1,2,3,4 the squared deviations sum to 5. *)
    check_float "the population variance divides by n" ~expected:1.25 (Stats.variance s);
    check_float "the sample variance divides by n - 1"
      ~expected:(5. /. 3.) (Stats.sample_variance s);
    check_float "the standard deviation is the root of the variance"
      ~expected:(sqrt 1.25) (Stats.standard_deviation s);
    check_float "the sample standard deviation likewise"
      ~expected:(sqrt (5. /. 3.)) (Stats.sample_standard_deviation s);
    check_float "the coefficient of variation is the deviation over the mean"
      ~expected:(sqrt 1.25 /. 2.5) (Stats.coefficient_of_variation s);
    (* The sample coefficient of variation is not simply the sample deviation
       over the mean: it carries the (1 + 1/4n) small-sample bias correction,
       which for n = 4 is a 6.25% adjustment.  Worth pinning, because the
       uncorrected form is what a reader would assume. *)
    check_float "the sample coefficient of variation carries a bias correction"
      ~expected:((1. +. 1. /. 16.) *. sqrt (5. /. 3.) /. 2.5)
      (Stats.sample_coefficient_of_variation s);
    Testing.check "the correction shrinks as the sample grows"
      (fun () ->
        let ratio n =
          let l = List.init n (fun i -> float_of_int (i + 1)) in
          let s = stats_of l in
          Stats.sample_coefficient_of_variation s
            /. (Stats.sample_standard_deviation s /. Stats.mean s) in
        ratio 100 < ratio 10 && ratio 10 < ratio 4);
    (* Both coefficients are defined as zero when the mean is, rather than
       raising or returning an infinity. *)
    check_float "a zero mean yields a zero coefficient of variation"
      ~expected:0. (Stats.sample_coefficient_of_variation (stats_of [ -1.; 1. ]));
    (* Order must not matter. *)
    check_float "the mean does not depend on the order values arrive in"
      ~expected:(Stats.mean s) (Stats.mean (stats_of [ 4.; 1.; 3.; 2. ]));
    check_float "nor does the variance"
      ~expected:(Stats.variance s) (Stats.variance (stats_of [ 4.; 1.; 3.; 2. ]));
    (* A constant sample has no spread at all. *)
    check_float "a constant sample has zero variance"
      ~expected:0. (Stats.variance (stats_of [ 7.; 7.; 7. ]));
    check_float "and its mean is that constant"
      ~expected:7. (Stats.mean (stats_of [ 7.; 7.; 7. ]));
    (* One value: the population variance is defined and zero. *)
    Testing.check_int "a single value counts as one"
      ~expected:1 (Stats.count (stats_of [ 42. ]));
    check_float "a single value has zero population variance"
      ~expected:0. (Stats.variance (stats_of [ 42. ]));
    (* This is why the accumulator is Welford's and not sum-of-squares: the same
       four values displaced by 1e9.  Summing squares would compute a difference
       of two numbers around 1e18 and keep none of the answer. *)
    let offset = 1e9 in
    check_float ~digits:6 "the variance survives a large offset"
      ~expected:1.25 (Stats.variance (stats_of [ offset +. 1.; offset +. 2.;
                                                 offset +. 3.; offset +. 4. ]));
    check_float ~digits:6 "and so does the sample variance"
      ~expected:(5. /. 3.)
      (Stats.sample_variance (stats_of [ offset +. 1.; offset +. 2.;
                                         offset +. 3.; offset +. 4. ]));
    (* clear returns the accumulator to its initial state rather than
       allocating a new one. *)
    Testing.check_int "clear forgets everything"
      ~expected:0 (let s = stats_of [ 1.; 2. ] in Stats.clear s; Stats.count s);
    check_float "and the accumulator is reusable afterwards"
      ~expected:10. (let s = stats_of [ 1.; 2. ] in
                     Stats.clear s;
                     Stats.add s 10.;
                     Stats.mean s))

(* The scalar layer.  [Scalar] wraps a base type in operators; the one that
   needs pinning is [==], which is a three-way comparison here rather than
   physical equality. *)

let test_scalars () =
  Testing.section "Scalars" (fun () ->
    let module I = Numbers.Int in
    Testing.check_string "an integer round-trips through its string form"
      ~expected:"42" (I.to_string (I.of_string "42"));
    Testing.check_int "of_int and to_int are inverse" ~expected:7 (I.to_int (I.of_int 7));
    Testing.check "of_string_opt rejects what is not a number"
      (fun () -> I.of_string_opt "not-a-number" = None);
    Testing.check "of_string_opt accepts what is"
      (fun () -> I.of_string_opt "13" <> None);
    Testing.check_raises "of_string raises on the same input"
      (fun () -> ignore (I.of_string "not-a-number"));
    Testing.check_int "addition" ~expected:5 (I.add 2 3);
    Testing.check_int "subtraction" ~expected:(-1) (I.sub 2 3);
    Testing.check_int "multiplication" ~expected:6 (I.mul 2 3);
    Testing.check_int "the remainder" ~expected:1 (I.rem 7 3);
    Testing.check_int "absolute value" ~expected:3 (I.abs (-3));
    Testing.check_int "min and max" ~expected:2 (I.min 2 3);
    Testing.check "equal compares by value" (fun () -> I.equal 3 3);
    (* [==] is compare, not physical equality, which is worth stating out loud
       because it reads like the OCaml operator it is not. *)
    Testing.check "the == operator is a three-way comparison, not identity"
      (fun () -> I.(2 == 3) < 0 && I.(3 == 3) = 0 && I.(4 == 3) > 0);
    let module F = Numbers.Float in
    Testing.check_string "a float round-trips through its string form"
      ~expected:"2.5" (F.to_string (F.of_string "2.5"));
    check_float "float addition" ~expected:0.3 (F.add 0.1 0.2);
    check_float "rounding goes to the nearest integer" ~expected:3. (F.round 2.6);
    check_float "and rounds a negative number away from zero"
      ~expected:(-3.) (F.round (-2.6)))

let run () =
  test_online_stats ();
  test_scalars ()
