(*
    Numbers.ml -- (c) 2015-2024 Paolo Ribeca, <paolo.ribeca@gmail.com>

    This file is part of BiOCamLib, the OCaml foundations upon which
    a number of the bioinformatics tools I developed are built.

    Numbers.ml implements numerical types such as:
     * a generalised type to provide a unified interface to all number
        types in the OCaml standard library
     * a general numerical vector type
     * a "collected" numerical vector type storing sorted elements and
        their frequencies.

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

open Better

(* A general module type to unify numbers *)
(* module type M = sig include module type of struct include Int end end *)
module type BaseScalar_t =
  sig
    type t
    val zero: t
    val one: t
    val minus_one: t
    val neg: t -> t
    val add: t -> t -> t
    val sub: t -> t -> t
    val mul: t -> t -> t
    val div: t -> t -> t
    val rem: t -> t -> t
    val succ: t -> t
    val pred: t -> t
    val abs: t -> t
    val equal: t -> t -> bool
    val compare: t -> t -> int
    val min: t -> t -> t
    val max: t -> t -> t
    val round: t -> t
    val to_int: t -> int
    val of_int: int -> t
    val of_int_opt: int -> t option
    val to_float: t -> float
    val of_float: float -> t
    val of_float_opt: float -> t option
    val to_string: t -> string
    val of_string: string -> t
    val of_string_opt: string -> t option
  end

module BaseInt: BaseScalar_t with type t = Int.t
= struct
    include Int (* Provides compare() *)
    let round n:int = n [@@inline]
    let to_int n:int = n [@@inline]
    let of_int n:int = n [@@inline]
    let of_int_opt n = Some n [@@inline]
    let to_float = float_of_int
    let of_float_opt x = Some (int_of_float x) [@@inline] (* TODO: No error checking at the moment *)
    let to_string = string_of_int
    let of_string = int_of_string
    let of_string_opt = int_of_string_opt
  end

module BaseInt32: BaseScalar_t with type t = Int32.t
= struct
    include Int32
    let round n:int32 = n [@@inline]
    let of_int_opt n = Some (of_int n) [@@inline] (* TODO: No error checking at the moment *)
    let of_float_opt x = Some (of_float x) [@@inline] (* TODO: No error checking at the moment *)
  end

module BaseInt64: BaseScalar_t with type t = Int64.t
= struct
    include Int64
    let round n:int64 = n [@@inline]
    let of_int_opt n = Some (of_int n) [@@inline] (* TODO: No error checking at the moment *)
    let of_float_opt x = Some (of_float x) [@@inline] (* TODO: No error checking at the moment *)
  end

module BaseFloat: BaseScalar_t with type t = Float.t
= struct
    include Float (* Our Float! Provides round() *)
    let to_int = int_of_float (* TODO: No error checking at the moment *)
    let of_int = float_of_int
    let of_int_opt n = Some (float_of_int n) [@@inline]
    let to_float x:float = x [@@inline]
    let of_float x:float = x [@@inline]
    let of_float_opt x = Some x [@@inline]
  end

(* A more refined scalar with additional operations and operators *)
module type Scalar_t =
  sig
    include BaseScalar_t
    val ( ~+ ): t -> t (* Unary plus *)
    val ( + ): t -> t -> t
    val ( ~- ): t -> t (* Unary minus *)
    val ( - ): t -> t -> t
    val ( * ): t -> t -> t
    val ( / ): t -> t -> t
    val ( % ): t -> t -> t
    val incr: t ref -> unit
    val ( ~++ ): t ref -> unit (* Unary increment *)
    val incr_by: t ref -> t -> unit
    val ( ++ ): t ref -> t -> unit
    val decr: t ref -> unit
    val ( ~-- ): t ref -> unit
    val decr_by: t ref -> t -> unit
    val ( -- ): t ref -> t -> unit
    val ( = ): t -> t -> bool
    val ( == ): t -> t -> int
    val ( < ): t -> t -> bool
    val ( <= ): t -> t -> bool
    val ( > ): t -> t -> bool
    val ( >= ): t -> t -> bool
    val pow: t -> t -> t
    val log: t -> t (* Needed for stats *)
    val ( ** ): t -> t -> t
  end

module Scalar (N: BaseScalar_t): Scalar_t with type t = N.t
= struct
    include N
    let ( ~+ ) n:N.t = n [@@inline] (* We eliminate polymorphism *)
    let ( + ) = N.add
    let ( ~- ) = N.neg
    let ( - ) = N.sub
    let ( * ) = N.mul
    let ( / ) = N.div
    let ( % ) = N.rem
    let incr rn = rn := !rn + N.one [@@inline]
    let ( ~++ ) = incr
    let incr_by rn n = rn := !rn + n [@@inline]
    let ( ++ ) = incr_by
    let decr rn = rn := !rn - N.one [@@inline]
    let ( ~-- ) = decr
    let decr_by rn n = rn := !rn - n [@@inline]
    let ( -- ) = decr_by
    let ( = ) = N.equal
    let ( == ) = N.compare
    let ( < ) a b = N.compare a b < 0 [@@inline]
    let ( <= ) a b = N.compare a b <= 0 [@@inline]
    let ( > ) a b = N.compare a b > 0 [@@inline]
    let ( >= ) a b = N.compare a b >= 0 [@@inline]
    (* We provide other functions by conversion to/from float *)
    let pow n p = N.to_float n ** N.to_float p |> N.of_float [@@inline] (* Note that overflows will be silent *)
    let ( ** ) = pow
    let _unary_float_function f n = N.to_float n |> f |> N.of_float [@@inline]
    let log = _unary_float_function log
  end

module Int = Scalar(BaseInt)
module Int32 = Scalar(BaseInt32)
module Int64 = Scalar(BaseInt64)
module Float = Scalar(BaseFloat)

(* As we have redefined Float, from now on Float.Array will be Better.Float.Array *)

module type Vector_t =
  sig
    module N: Scalar_t
    type t
    val make: int -> N.t -> t
    val init: int -> (int -> N.t) -> t
    val empty: t
    val length: t -> int
    val get: t -> int -> N.t
    val ( .@() ): t -> int -> N.t
    val set: t -> int -> N.t -> unit
    val ( .@()<- ): t -> int -> N.t -> unit
    val incr: t -> int -> unit
    val ( .+() ): t -> int -> unit
    val ( .++() ): t -> int -> unit
    val incr_by: t -> int -> N.t -> unit
    val ( .+()<- ): t -> int -> N.t -> unit
    val decr: t -> int -> unit
    val ( .-() ): t -> int -> unit
    val ( .--() ): t -> int -> unit
    val decr_by: t -> int -> N.t -> unit
    val ( .-()<- ): t -> int -> N.t -> unit
    val sub: t -> int -> int -> t
    val blit: t -> int -> t -> int -> int -> unit
    val fill: t -> int -> int -> N.t -> unit
    val resize: ?is_buffer:bool -> ?fill_with:N.t -> int -> t -> t
    val to_floatarray: t -> floatarray
    val of_floatarray: floatarray -> t
  end

(* Uniform vector based on floatarrays.
   For consistency with the rest, we encapsulate things in a module *)
module Floatarray =
  struct
    module type Type_t = Vector_t
    module Vector: Vector_t =
      struct
        module N = Scalar(BaseFloat)
        module FA = Better.Float.Array
        include FA
        let empty = FA.make 0 0.
        let ( .@() ) = get
        let ( .@()<- ) = set
        let incr fa i = FA.(set fa i N.(get fa i + one)) [@@inline]
        let ( .+() ) = incr
        let ( .++() ) = incr
        let incr_by fa i n = FA.(set fa i N.(get fa i + n)) [@@inline]
        let ( .+()<- ) = incr_by
        let decr fa i = FA.(set fa i N.(get fa i - one)) [@@inline]
        let ( .-() ) = decr
        let ( .--() ) = decr
        let decr_by fa i n = FA.(set fa i N.(get fa i - n)) [@@inline]
        let ( .-()<- ) = decr_by
        let resize ?(is_buffer = false) ?(fill_with = N.zero) n fa =
          let l = length fa in
          if n > l then begin
            let res =
              make begin
                if is_buffer then
                  max n (l * 14 / 10)
                else
                  n
              end fill_with in
            FA.(blit fa 0 res 0 l);
            res
          end else if n < l && not is_buffer then
            (* We have to resize in order to honour the request *)
            let res = make n fill_with in
            FA.(blit fa 0 res 0 n);
            res
          else
            fa
        let to_floatarray fa:t = fa [@@inline]
        let of_floatarray fa:t = fa [@@inline]
      end
  end

(* Uniform vectors based on bigarrays *)
module Bigarray:
  sig
    module type Scalar_t =
      sig
        include Scalar_t
        type elt_t
        val elt: (t, elt_t) Bigarray.kind
      end
    module type Type_t = functor (T: Scalar_t) -> Vector_t with type N.t = T.t
    module Vector: Type_t
  end
= struct
    module type Scalar_t =
      sig
        include Scalar_t
        type elt_t
        val elt: (t, elt_t) Bigarray.kind
      end
    module type Type_t = functor (T: Scalar_t) -> Vector_t with type N.t = T.t
    module Vector (T: Scalar_t) = (* This should work with float too *)
      struct
        module BA1 = Bigarray.Array1
        module N = T
        type t = (N.t, T.elt_t, Bigarray.c_layout) BA1.t
        let make n a =
          let res = BA1.create T.elt Bigarray.C_layout n in
          BA1.fill res a;
          res
        let init = BA1.init T.elt Bigarray.C_layout
        let empty = make 0 N.zero
        let length = BA1.dim
        let get = BA1.get
        let ( .@() ) = BA1.get
        let set = BA1.set
        let ( .@()<- ) = BA1.set
        let incr ba n = BA1.(set ba n N.(get ba n + one)) [@@inline]
        let ( .+() ) = incr
        let ( .++() ) = incr
        let incr_by ba n v = BA1.(set ba n N.(get ba n + v)) [@@inline]
        let ( .+()<- ) = incr_by
        let decr ba n = BA1.(set ba n N.(get ba n - one)) [@@inline]
        let ( .-() ) = decr
        let ( .--() ) = decr
        let decr_by ba n v = BA1.(set ba n N.(get ba n - v)) [@@inline]
        let ( .-()<- ) = decr_by
        let sub = BA1.sub
        let blit ba_1 i_1 ba_2 i_2 l = BA1.(blit (sub ba_1 i_1 l) (sub ba_2 i_2 l))
        let fill ba i l n = BA1.(fill (sub ba i l) n)
        let resize ?(is_buffer = false) ?(fill_with = N.zero) n v =
          let l = length v in
          if n > l then begin
            let res =
              make begin
                if is_buffer then
                  max n (l * 14 / 10)
                else
                  n
              end fill_with in
            BA1.(blit v (sub res 0 l));
            res
          end else if n < l && not is_buffer then
            (* We have to resize in order to honour the request *)
            let res = make n fill_with in
            BA1.(blit (sub v 0 n) res);
            res
          else
            v
        let to_floatarray v =
          let l = length v in
          let res = Better.Float.Array.make l 0. in
          for i = 0 to l - 1 do
            N.to_float v.@(i) |> Better.Float.Array.set res i
          done;
          res
        let of_floatarray f =
          init (Better.Float.Array.length f) (fun i -> N.of_float Better.Float.Array.(get f i))
      end
  end

(* Functor to wrap uniform numbers into comparable types *)
module type MakeComparableNumber_t = functor (N: Scalar_t) -> ComparableType_t with type t = N.t
module MakeComparableNumber (N: Scalar_t): ComparableType_t with type t = N.t =
  struct
    type t = N.t
    (* Informs the compiler that polymorphism is not needed *)
    let compare (a:t) (b:t) = compare a b [@@inline]
  end
module MakeRComparableNumber (N: Scalar_t): ComparableType_t with type t = N.t =
  struct
    type t = N.t
    (* Informs the compiler that polymorphism is not needed *)
    let compare (a:t) (b:t) = compare b a [@@inline]
  end
  
module type FrequenciesVector_t =
  sig
    module N: Scalar_t
    module C: MakeComparableNumber_t
    type t
    val empty: ?non_negative:bool -> unit -> t
    val is_non_negative: t -> bool
    exception Negative_element of N.t
    val add: t -> N.t -> unit
    val iter: (N.t -> int -> unit) -> t -> unit
    val length: t -> int
    (* Returns the frequency of a given value *)
    val frequency: t -> N.t -> int
    exception Empty
    (* The next 6 functions can raise Empty.
       The results of the next four functions depend on the definition of the comparison function *)
    val first: t -> N.t
    val first_opt: t -> N.t option
    val last: t -> N.t
    val last_opt: t -> N.t option
    (* In case of ties, one random binding is returned *)
    val most_frequent: t -> N.t * int
    val median: t -> N.t
    val sum: t -> N.t
    val sum_abs: t -> N.t
    val sum_log_abs: t -> float
    val pow_abs: N.t -> t -> t
    val normalize_abs: t -> t
    exception Invalid_threshold of float
    (* Examine values in order, and null frequencies when accumulated absolute values are > threshold * sum_abs.
       Threshold must be between 0. and 1. *)
    val threshold_accum_abs: float -> t -> t
    val of_floatarray: Better.Float.Array.t -> t
    val to_floatarray: t -> Better.Float.Array.t
  end

module Frequencies:
  sig
    module type Type_t = FrequenciesVector_t
    module Vector: functor (NT: Scalar_t) (MCN: MakeComparableNumber_t) -> Type_t with module N = NT and module C = MCN
  end
= struct
    module type Type_t = FrequenciesVector_t
    module Vector (NT: Scalar_t) (MCN: MakeComparableNumber_t) =
      struct
        module N = NT
        module C = MCN
        module CN = C(N)
        module M = Map.Make(C(N))
        type t = {
          non_negative: bool;
          mutable data: int ref M.t;
          mutable length: int;
          mutable sum: N.t;
          mutable sum_abs: N.t;
          mutable sum_log_abs: float;
          mutable unnorm_variance: float;
          mutable unnorm_variance_abs: float;
          (* The most frequent element and its frequency *)
          mutable most_frequent: N.t * int;
          (* The lowest element of the median pair and its offset within its group *)
          mutable median: N.t * int
        }
        let empty ?(non_negative = false) () = {
          non_negative = non_negative;
          data = M.empty;
          length = 0;
          sum = N.zero;
          sum_abs = N.zero;
          sum_log_abs = 0.;
          unnorm_variance = 0.;
          unnorm_variance_abs = 0.;
          most_frequent = N.zero, 0;
          median = N.zero, 0
        }
        let is_non_negative fv = fv.non_negative
        exception Negative_element of N.t
        let add fv n =
          if fv.non_negative && n < N.zero then
            Negative_element n |> raise;
          begin match M.find_opt n fv.data with
          | None ->
            fv.data <- M.add n (ref 1) fv.data
          | Some r ->
            incr r
          end;
          let abs_n = N.abs n
          and old_length = fv.length and old_length_N = N.of_int fv.length in
          fv.length <- fv.length + 1;
          let quantity = N.(old_length_N * n - fv.sum |> to_float)
          and quantity_abs = N.(old_length_N * abs_n - fv.sum_abs |> to_float) in
          let normalisation = 1. /. (old_length * fv.length |> float_of_int) in
          fv.unnorm_variance <- fv.unnorm_variance +. quantity *. quantity *. normalisation;
          fv.unnorm_variance_abs <- fv.unnorm_variance_abs +. quantity_abs *. quantity_abs *. normalisation;
          (* THESE TWO SUMS MUST NOT BE UPDATED EARLIER *)
          fv.sum <- N.(fv.sum + n);
          fv.sum_abs <- N.(fv.sum_abs + abs_n);
          let abs_n_f = N.to_float abs_n in
          fv.sum_log_abs <- fv.sum_log_abs +. log abs_n_f;
          let new_frequency = !(M.find n fv.data)
          and _, old_largest_frequency = fv.most_frequent in
          if new_frequency > old_largest_frequency then
            fv.most_frequent <- n, new_frequency;
          let median_n, median_idx = fv.median in
          (* Is_even is_right *)
          match (fv.length / 2) * 2 = fv.length, CN.compare n median_n >= 0 with
          | false, false ->
            if median_idx > 0 then
              fv.median <- median_n, median_idx - 1
            else begin
              let left, _, _ = M.split median_n fv.data in
              let max_left_n, max_left_freq = M.max_binding left in
              fv.median <- max_left_n, !max_left_freq - 1
            end
          | true, true ->
            if median_idx <= new_frequency - 2 then
              fv.median <- median_n, median_idx + 1
            else begin
              let _, _, right = M.split median_n fv.data in
              let min_right_n, _ = M.min_binding right in
              fv.median <- min_right_n, 0
            end
          | false, true | true, false -> ()
        let iter f fv =
          M.iter
            (fun el r ->
              f el !r)
            fv.data
          [@@inline]
        let length { length; _ } =
          length
          [@@inline]
        let frequency { data; _ } n =
          match M.find_opt n data with
          | None -> 0
          | Some n -> !n
        exception Empty
        let first fv =
          match M.min_binding_opt fv.data with
          | None -> raise Empty
          | Some (n, _) -> n
        let first_opt fv =
          match M.min_binding_opt fv.data with
          | None -> None
          | Some (n, _) -> Some n
        let last fv =
          match M.max_binding_opt fv.data with
          | None -> raise Empty
          | Some (n, _) -> n
        let last_opt fv =
          match M.max_binding_opt fv.data with
          | None -> None
          | Some (n, _) -> Some n
        let most_frequent { length; most_frequent; _ } =
          if length = 0 then
            raise Empty
          else
            most_frequent
        let two = N.of_int 2
        let median { data; length; median = median_n, median_idx; _ } =
          if (length / 2) * 2 = length then begin
            let median_freq = M.find median_n data in
            if median_idx <= !median_freq - 2 then
              median_n
            else begin
              let _, _, right = M.split median_n data in
              let min_right_n, _ = M.min_binding right in
              N.((median_n + min_right_n) / two)
            end
          end else
            median_n
        let sum { sum; _ } =
          sum
          [@@inline]
        let sum_abs { sum_abs; _ } =
          sum_abs
          [@@inline]
        let sum_log_abs { sum_log_abs; _ } =
          sum_log_abs
          [@@inline]
        let pow_abs p fv =
          if p = N.one then
            fv
          else begin
            let res = empty ~non_negative:true () in
            M.iter
              (fun el freq ->
                for _ = 1 to !freq do
                  add res N.(abs el ** p)
                done)
              fv.data;
            res
          end
        let normalize_abs fv =
          let norm = fv.sum_abs in
          if norm = N.zero || norm = N.one then
            (* If acc = 0, all elements must be 0 *)
            fv
          else begin
            let res = empty ~non_negative:true () in
            M.iter
              (fun el freq ->
                for _ = 1 to !freq do
                  add res N.(el / norm)
                done)
              fv.data;
            res
          end
        (* This function must _not_ change the total number of elements *)
        exception Invalid_threshold of float
        let threshold_accum_abs t fv =
          if t < 0. || t > 1. then
            Invalid_threshold t |> raise;
          if t = 1. then
            fv
          else begin
            let t = t *. N.to_float fv.sum_abs |> N.of_float and res = empty ~non_negative:true () in
            let acc = ref N.zero in
            M.iter
              (fun el freq ->
                for _ = 1 to !freq do
                  add res (if !acc < t then el else N.zero);
                  N.(acc ++ abs el)
                done)
              fv.data;
            res
          end
        let of_floatarray fa =
          let res = empty ~non_negative:true () and non_negative = ref true in
          Better.Float.Array.iter
            (fun el ->
              if el < 0. then
                non_negative := false;
              N.of_float el |> add res)
            fa;
          { res with non_negative = !non_negative }
        (* The exported floatarray will have sorted elements *)
        let to_floatarray { length; data; _ } =
          let res = Better.Float.Array.create length and idx = ref 0 in
          M.iter
            (fun el freq ->
              for i = !idx to !idx + !freq - 1 do
                Better.Float.Array.set res i N.(to_float el)
              done;
              idx := !idx + !freq)
            data;
          res
      end
  end

