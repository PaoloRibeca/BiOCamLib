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
module V = Numbers.FloatArrayVector
module LF = Numbers.LinearFit (Numbers.FloatArrayVector)

(* Helpers. *)

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
    Testing.check_float "the mean is the arithmetic mean" ~expected:2.5 (Stats.mean s);
    (* Population variance divides by n; the sample variance by n - 1.  For
       1,2,3,4 the squared deviations sum to 5. *)
    Testing.check_float "the population variance divides by n" ~expected:1.25 (Stats.variance s);
    Testing.check_float "the sample variance divides by n - 1"
      ~expected:(5. /. 3.) (Stats.sample_variance s);
    Testing.check_float "the standard deviation is the root of the variance"
      ~expected:(sqrt 1.25) (Stats.standard_deviation s);
    Testing.check_float "the sample standard deviation likewise"
      ~expected:(sqrt (5. /. 3.)) (Stats.sample_standard_deviation s);
    Testing.check_float "the coefficient of variation is the deviation over the mean"
      ~expected:(sqrt 1.25 /. 2.5) (Stats.coefficient_of_variation s);
    (* The sample coefficient of variation is not simply the sample deviation
       over the mean: it carries the (1 + 1/4n) small-sample bias correction,
       which for n = 4 is a 6.25% adjustment.  Worth pinning, because the
       uncorrected form is what a reader would assume. *)
    Testing.check_float "the sample coefficient of variation carries a bias correction"
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
    Testing.check_float "a zero mean yields a zero coefficient of variation"
      ~expected:0. (Stats.sample_coefficient_of_variation (stats_of [ -1.; 1. ]));
    (* Order must not matter. *)
    Testing.check_float "the mean does not depend on the order values arrive in"
      ~expected:(Stats.mean s) (Stats.mean (stats_of [ 4.; 1.; 3.; 2. ]));
    Testing.check_float "nor does the variance"
      ~expected:(Stats.variance s) (Stats.variance (stats_of [ 4.; 1.; 3.; 2. ]));
    (* A constant sample has no spread at all. *)
    Testing.check_float "a constant sample has zero variance"
      ~expected:0. (Stats.variance (stats_of [ 7.; 7.; 7. ]));
    Testing.check_float "and its mean is that constant"
      ~expected:7. (Stats.mean (stats_of [ 7.; 7.; 7. ]));
    (* One value: the population variance is defined and zero. *)
    Testing.check_int "a single value counts as one"
      ~expected:1 (Stats.count (stats_of [ 42. ]));
    Testing.check_float "a single value has zero population variance"
      ~expected:0. (Stats.variance (stats_of [ 42. ]));
    (* This is why the accumulator is Welford's and not sum-of-squares: the same
       four values displaced by 1e9.  Summing squares would compute a difference
       of two numbers around 1e18 and keep none of the answer. *)
    let offset = 1e9 in
    Testing.check_float "the variance survives a large offset"
      ~expected:1.25 (Stats.variance (stats_of [ offset +. 1.; offset +. 2.;
                                                 offset +. 3.; offset +. 4. ]));
    Testing.check_float "and so does the sample variance"
      ~expected:(5. /. 3.)
      (Stats.sample_variance (stats_of [ offset +. 1.; offset +. 2.;
                                         offset +. 3.; offset +. 4. ]));
    (* clear returns the accumulator to its initial state rather than
       allocating a new one. *)
    Testing.check_int "clear forgets everything"
      ~expected:0 (let s = stats_of [ 1.; 2. ] in Stats.clear s; Stats.count s);
    Testing.check_float "and the accumulator is reusable afterwards"
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
    Testing.check_float "float addition" ~expected:0.3 (F.add 0.1 0.2);
    Testing.check_float "rounding goes to the nearest integer" ~expected:3. (F.round 2.6);
    Testing.check_float "and rounds a negative number away from zero"
      ~expected:(-3.) (F.round (-2.6)))

(* Least-squares line fitting.  The interesting cases are the ones with an
   answer that can be worked out by hand, and the singular system, where every
   abscissa is the same and there is no line to fit. *)

let test_linear_fit () =
  Testing.section "Linear fit" (fun () ->
    let fit xs ys =
      let m, prediction, residuals = LF.make (V.of_list xs) (V.of_list ys) in
      m, V.to_list prediction, V.to_list residuals in
    let shows l = List.map (Printf.sprintf "%.6f") l |> String.concat "," in
    (* y = 2x + 1, exactly. *)
    let m, prediction, residuals = fit [ 0.; 1.; 2.; 3. ] [ 1.; 3.; 5.; 7. ] in
    Testing.check_float "an exact line recovers its slope" ~expected:2. (LF.get_slope m);
    Testing.check_float "and its intercept" ~expected:1. (LF.get_intercept m);
    Testing.check_string "the predictions reproduce the data"
      ~expected:"1.000000,3.000000,5.000000,7.000000" (shows prediction);
    Testing.check_string "and the residuals are zero"
      ~expected:"0.000000,0.000000,0.000000,0.000000" (shows residuals);
    (* A horizontal line: no slope, and the intercept is the common value. *)
    let m, _, _ = fit [ 0.; 1.; 2. ] [ 5.; 5.; 5. ] in
    Testing.check_float "a horizontal line has no slope" ~expected:0. (LF.get_slope m);
    Testing.check_float "and its intercept is the common value"
      ~expected:5. (LF.get_intercept m);
    (* A negative slope, still exact. *)
    let m, _, _ = fit [ 0.; 1.; 2. ] [ 4.; 2.; 0. ] in
    Testing.check_float "a descending line has a negative slope"
      ~expected:(-2.) (LF.get_slope m);
    (* Points that do not lie on a line: worked out by hand, the least-squares
       fit through (0,0) (1,0) (2,2) (3,2) has slope 4/5 and intercept -1/5. *)
    let m, _, residuals = fit [ 0.; 1.; 2.; 3. ] [ 0.; 0.; 2.; 2. ] in
    Testing.check_float "a scattered set gets its least-squares slope"
      ~expected:0.8 (LF.get_slope m);
    Testing.check_float "and its least-squares intercept"
      ~expected:(-0.2) (LF.get_intercept m);
    (* The defining property of a least-squares fit: the residuals sum to zero.
       It lands a rounding error either side of it, which is exactly what the
       tolerance is for. *)
    Testing.check_float "the residuals of a least-squares fit sum to zero"
      ~expected:0. (List.fold_left ( +. ) 0. residuals);
    (* predict is the same model applied to fresh abscissae. *)
    let m, _, _ = fit [ 0.; 1.; 2.; 3. ] [ 1.; 3.; 5.; 7. ] in
    Testing.check_string "the model predicts beyond the data it was fitted on"
      ~expected:"21.000000,201.000000"
      (shows (V.to_list (LF.predict m (V.of_list [ 10.; 100. ]))));
    (* Every abscissa the same: the system is singular and there is no fit. *)
    Testing.check_raises "a singular system is refused"
      (fun () -> ignore (fit [ 2.; 2.; 2. ] [ 1.; 2.; 3. ]));
    Testing.check_raises "and so is a single point"
      (fun () -> ignore (fit [ 1. ] [ 1. ])))

(* Frequency vectors: a multiset kept as distinct values with their counts,
   ordered by the scalar it is built over.  The reverse-comparable
   instantiation orders the other way, which is what makes [first], [last] and
   the running [median] worth stating separately for each. *)

let test_frequencies () =
  Testing.section "Frequency vectors" (fun () ->
    let module FV = Numbers.FloatFreqsVector in
    let module RFV = Numbers.RFloatFreqsVector in
    (* 0 twice, 0.5, 1, 3.5, 4 twice: seven values, five of them distinct. *)
    let init = Better.Float.Array.of_list [ 1.; 0.; 4.; 0.5; 0.; 3.5; 4. ] in
    let show fv =
      let acc = ref [] in
      FV.iter (fun v n -> List.accum acc (Printf.sprintf "%g*%d" v n)) fv;
      List.rev !acc |> String.concat "," in
    let rshow fv =
      let acc = ref [] in
      RFV.iter (fun v n -> List.accum acc (Printf.sprintf "%g*%d" v n)) fv;
      List.rev !acc |> String.concat "," in
    let a () = FV.of_floatarray ~non_negative:true init in
    Testing.check_string "values are held once each, with their counts, in order"
      ~expected:"0*2,0.5*1,1*1,3.5*1,4*2" (show (a ()));
    Testing.check_string "the reverse ordering holds the same values the other way round"
      ~expected:"4*2,3.5*1,1*1,0.5*1,0*2"
      (rshow (RFV.of_floatarray ~non_negative:true init));
    Testing.check_int "a repeated value is counted, not stored twice"
      ~expected:2 (FV.frequency (a ()) 0.);
    Testing.check_int "and a value that occurs once has a count of one"
      ~expected:1 (FV.frequency (a ()) 3.5);
    Testing.check_int "a value that never occurs has a count of zero"
      ~expected:0 (FV.frequency (a ()) 99.);
    Testing.check_float "the sum is of every element, not of the distinct ones"
      ~expected:13. (FV.sum (a ()));
    Testing.check_float "and the mean divides by the number of elements"
      ~expected:(13. /. 7.) (FV.mean (a ()));
    Testing.check_float "sum_abs agrees with sum on a non-negative vector"
      ~expected:13. (FV.sum_abs (a ()));
    (* The moments.  For 0,0,0.5,1,3.5,4,4 the squared deviations from the mean
       sum to 45.5 - 13^2/7, which is what both variances divide down.  They are
       accumulated one element at a time as the vector is built, so the case to
       state is the FIRST element: there is no deviation to add yet, and the
       increment has to be skipped rather than evaluated. *)
    let sum_sq_dev = 45.5 -. 13. *. 13. /. 7. in
    Testing.check_float "the population variance divides by n"
      ~expected:(sum_sq_dev /. 7.) (FV.variance (a ()));
    Testing.check_float "and the sample variance by n - 1"
      ~expected:(sum_sq_dev /. 6.) (FV.sample_variance (a ()));
    Testing.check_float "the standard deviation is its root"
      ~expected:(sqrt (sum_sq_dev /. 7.)) (FV.standard_deviation (a ()));
    Testing.check_float "and the sample standard deviation the root of the other"
      ~expected:(sqrt (sum_sq_dev /. 6.)) (FV.sample_standard_deviation (a ()));
    (* This vector is non-negative, so its absolute moments must agree with the
       plain ones.  Over a signed vector they do not, which is why both are
       kept: the absolute ones describe the magnitudes and not the values. *)
    Testing.check_float "the absolute variance agrees with it on a non-negative vector"
      ~expected:(sum_sq_dev /. 7.) (FV.variance_abs (a ()));
    let signed () = let v = FV.make () in FV.add v (-2.); FV.add v 2.; v in
    Testing.check_float "on a signed one the values are spread"
      ~expected:4. (FV.variance (signed ()));
    Testing.check_float "while their magnitudes are not"
      ~expected:0. (FV.variance_abs (signed ()));
    (* The degenerate lengths.  One element has a variance and it is zero, and
       an empty vector answers zero rather than a negative zero. *)
    Testing.check_float "a single element has zero variance"
      ~expected:0. (let v = FV.make () in FV.add v 7.; FV.variance v);
    Testing.check_string "and an empty vector zero rather than minus zero"
      ~expected:"0" (FV.sample_variance (FV.make ()) |> Printf.sprintf "%g");
    (* The ends of the order, which is where the two instantiations differ. *)
    Testing.check_float "first is the smallest under the natural order"
      ~expected:0. (FV.first (a ()));
    Testing.check_float "and the largest under the reverse one"
      ~expected:4. (RFV.first (RFV.of_floatarray ~non_negative:true init));
    Testing.check_float "last mirrors it" ~expected:4. (FV.last (a ()));
    (* The vector was declared non-negative, so it refuses to become
       otherwise rather than quietly holding a value it promised not to. *)
    Testing.check "a vector declared non-negative says so"
      (fun () -> FV.is_non_negative (a ()));
    Testing.check_raises "and refuses a negative element"
      (fun () -> FV.add (a ()) (-1.));
    Testing.check_string "adding a value that is already there only raises its count"
      ~expected:"0*2,0.5*2,1*1,3.5*1,4*2"
      (let v = a () in FV.add v 0.5; show v);
    Testing.check_string "and a new value takes its place in the order"
      ~expected:"0*2,0.5*1,1*1,2*1,3.5*1,4*2"
      (let v = a () in FV.add v 2.; show v);
    Testing.check_int "clear empties it" ~expected:0
      (let v = a () in FV.clear v; FV.length v);
    (* Round trip through a plain array, which sorts as a side effect of the
       vector holding its values in order. *)
    Testing.check_string "to_floatarray yields the elements in order"
      ~expected:"0,0,0.5,1,3.5,4,4"
      (FV.to_floatarray (a ()) |> Better.Float.Array.to_list
       |> List.map (Printf.sprintf "%g") |> String.concat ",");
    Testing.check_string "pow_abs raises every element"
      ~expected:"0*2,0.25*1,1*1,12.25*1,16*2" (show (FV.pow_abs 2. (a ())));
    (* The median walks the counts rather than a materialised list, so the
       cases worth stating are an odd count, an even one, and a median that
       falls inside a repeated value. *)
    Testing.check_float "the median of an odd number of elements is the middle one"
      ~expected:1. (FV.median (a ()));
    Testing.check_float "and does not depend on the direction of the order"
      ~expected:1. (RFV.median (RFV.of_floatarray ~non_negative:true init));
    (* An even count straddles two elements and takes their mean -- 0.5 and 1
       here -- rather than picking one of them. *)
    Testing.check_float "an even count interpolates between the two middle elements"
      ~expected:0.75 (let v = a () in FV.add v 0.5; FV.median v);
    Testing.check_float "unless the two fall inside one repeated value"
      ~expected:0.5 (let v = a () in FV.add v 0.5; FV.add v 0.5; FV.median v);
    Testing.check_float "a single element is its own median"
      ~expected:7. (let v = FV.make () in FV.add v 7.; FV.median v);
    (* An empty vector has no median, and says so rather than answering zero:
       zero is a perfectly good median, so a caller given one could not tell an
       empty vector from a vector of zeros.  [first], [last] and
       [most_frequent] beside it refuse the same way. *)
    Testing.check_raises ~re:"is empty" "an empty vector has no median"
      (fun () -> FV.median (FV.make ()));
    Testing.check_raises ~re:"is empty" "nor a first, a last or a most frequent"
      (fun () -> FV.first (FV.make ()));
    (* [threshold_accum_abs] keeps elements while the absolute mass accumulated
       so far is under the given fraction of the total, and zeroes the rest --
       preserving the number of elements rather than dropping any.  It walks in
       the vector's own order, so the two instantiations do opposite things
       with it, and that is what the reverse-comparable one is for: ascending
       keeps the small elements, descending keeps the dominant ones. *)
    Testing.check_string "thresholding ascending keeps the small elements"
      ~expected:"0*3,0.5*1,1*1,3.5*1,4*1" (show (FV.threshold_accum_abs 0.5 (a ())));
    Testing.check_string "and descending keeps the dominant ones"
      ~expected:"4*2,0*5"
      (rshow (RFV.threshold_accum_abs 0.5 (RFV.of_floatarray ~non_negative:true init)));
    Testing.check_int "either way the number of elements is unchanged"
      ~expected:7 (FV.length (FV.threshold_accum_abs 0.5 (a ())));
    Testing.check_string "a threshold of one changes nothing"
      ~expected:(show (a ())) (show (FV.threshold_accum_abs 1. (a ())));
    Testing.check_raises "a threshold outside zero to one is refused"
      (fun () -> ignore (FV.threshold_accum_abs 1.5 (a ()))))

(* The Bigarray-backed vectors.  There are four, two integer widths and two
   float widths, and they satisfy the same [Vector_t] as the floatarray vector
   the checks above use.  So the check worth having is not that each one works
   in isolation but that they AGREE: one script, run over two implementations of
   one interface, has to give one answer, and where it cannot -- a 32-bit float
   cannot hold what a 64-bit one holds -- that difference is itself the thing to
   pin, since it is the only reason to reach for the narrow variant. *)

let vector_script (module V: Numbers.Vector_t with type N.t = float) =
  let v = V.init 5 (fun i -> float_of_int i) in
  V.set v 0 10.;
  V.incr v 1;
  V.incr_by v 2 3.;
  V.decr v 3;
  let w = V.sub v 1 3 in
  V.fill w 0 1 99.;
  let seen = ref [] in
  V.iteri (fun i x -> List.accum seen (Printf.sprintf "%d:%g" i x)) v;
  Printf.sprintf "%s | %s | %s | %d"
    (V.to_list v |> List.map (Printf.sprintf "%g") |> String.concat ",")
    (V.to_list w |> List.map (Printf.sprintf "%g") |> String.concat ",")
    (List.rev !seen |> String.concat " ")
    (V.length v)

let test_bigarray_vectors () =
  Testing.section "Bigarray vectors" (fun () ->
    (* The same operations over the same values, on two different backings. *)
    Testing.check_string "the float Bigarray vector agrees with the floatarray one"
      ~expected:(vector_script (module Numbers.FloatArrayVector))
      (vector_script (module Numbers.FloatBAVector));
    (* A sub-vector is a copy and not a window: writing into it must not reach
       back into what it came from.  The script above writes 99 into [w], and
       both implementations agreeing is only half the answer if both share. *)
    Testing.check_bool "a sub-vector does not write back into its parent"
      ~expected:true
      (let module V = Numbers.FloatBAVector in
       let v = V.init 4 (fun i -> float_of_int i) in
       let w = V.sub v 1 2 in
       V.set w 0 99.;
       V.get v 1 = 1.);
    (* What the narrow float variant is for.  Storing a tenth and reading it
       back gives a value close to a tenth and not a tenth, which is the whole
       trade: half the memory for seven digits instead of sixteen. *)
    Testing.check_float ~tolerance:1e-7 "a 32-bit float vector keeps a value to single precision"
      ~expected:0.1
      (let module V = Numbers.Float32BAVector in
       let v = V.make 1 0. in
       V.set v 0 0.1;
       V.get v 0);
    Testing.check_bool "but not to double precision" ~expected:true
      (let module V = Numbers.Float32BAVector in
       let v = V.make 1 0. in
       V.set v 0 0.1;
       V.get v 0 <> 0.1);
    (* And the integer variant, which has no such trade at this width. *)
    Testing.check_bool "the int Bigarray vector stores and returns what it was given"
      ~expected:true
      (let module V = Numbers.IntBAVector in
       let v = V.init 3 (fun i -> i * 7) in
       V.incr v 0;
       V.decr_by v 2 4;
       V.to_list v = [ 1; 7; 10 ]);
    (* [empty] is the degenerate case every one of them has to get right. *)
    Testing.check_int "an empty Bigarray vector has no elements" ~expected:0
      (Numbers.FloatBAVector.length Numbers.FloatBAVector.empty);
    Testing.check_string "and iterating one visits nothing" ~expected:""
      (let module V = Numbers.FloatBAVector in
       let seen = ref [] in
       V.iter (fun x -> List.accum seen (Printf.sprintf "%g" x)) V.empty;
       String.concat "," !seen))


let run () =
  test_online_stats ();
  test_frequencies ();
  test_scalars ();
  test_linear_fit ();
  test_bigarray_vectors ()
