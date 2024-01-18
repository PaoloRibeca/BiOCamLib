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

(* We extend some stdlib types with a few additional functions *)

module Number:
  sig
    (* A general module type to unify numbers *)
    (* module type M = sig include module type of struct include Int end end *)
    module type BasicType_t =
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
        val to_int: t -> int
        val of_int: int -> t
        val to_float: t -> float
        val of_float: float -> t
        val to_string: t -> string
        val of_string: string -> t
        val of_string_opt: string -> t option
      end
    module type Type_t =
      sig
        include BasicType_t
        val ( .-() ): t -> t
        val ( + ): t -> t -> t
        val ( - ): t -> t -> t
        val ( * ): t -> t -> t
        val ( / ): t -> t -> t
        val ( % ): t -> t -> t
        val incr: t ref -> unit
        val ( .++() ): t ref -> unit
        val decr: t ref -> unit
        val ( .--() ): t ref -> unit
        val ( = ): t -> t -> bool
        val ( == ): t -> t -> int
      end
    module Make: functor (N: BasicType_t) -> Type_t with type t = N.t
  end
= struct
    module type BasicType_t =
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
        val to_int: t -> int
        val of_int: int -> t
        val to_float: t -> float
        val of_float: float -> t
        val to_string: t -> string
        val of_string: string -> t
        val of_string_opt: string -> t option
      end
    module type Type_t =
      sig
        include BasicType_t
        val ( .-() ): t -> t
        val ( + ): t -> t -> t
        val ( - ): t -> t -> t
        val ( * ): t -> t -> t
        val ( / ): t -> t -> t
        val ( % ): t -> t -> t
        val incr: t ref -> unit
        val ( .++() ): t ref -> unit
        val decr: t ref -> unit
        val ( .--() ): t ref -> unit
        val ( = ): t -> t -> bool
        val ( == ): t -> t -> int
      end
    module Make (N: BasicType_t): Type_t with type t = N.t =
      struct
        include N
        let ( .-() ) = N.neg
        let ( + ) = N.add
        let ( - ) = N.sub
        let ( * ) = N.mul
        let ( / ) = N.div
        let ( % ) = N.rem
        let incr rn = rn := !rn + N.one
        let ( .++() ) = incr
        let decr rn = rn := !rn - N.one
        let ( .--() ) = decr
        let ( = ) = N.equal
        let ( == ) = N.compare
      end
  end

(* Encapsulated vectors based on bigarrays *)
module BA:
  sig
    module type ScalarType_t =
      sig
        include Number.Type_t
        type elt_t
        val elt: (t, elt_t) Bigarray.kind
      end
    module type Type_t =
      sig
        module BA1 = Bigarray.Array1
        module N: ScalarType_t
        type t
        val init: int -> N.t -> t
        val empty: t
        val length: t -> int
        val get: t -> int -> N.t
        val ( .@() ): t -> int -> N.t
        val set: t -> int -> N.t -> unit
        val ( .@()<- ): t -> int -> N.t -> unit
        val incr: t -> int -> unit
        val ( .+() ): t -> int -> unit
        val incr_by: t -> int -> N.t -> unit
        val ( .+()<- ): t -> int -> N.t -> unit
        val decr: t -> int -> unit
        val ( .-() ): t -> int -> unit
        val decr_by: t -> int -> N.t -> unit
        val ( .-()<- ): t -> int -> N.t -> unit
        val sub: t -> int -> int -> t
        val blit: t -> t -> unit
        val fill: t -> N.t -> unit
        val resize: ?is_buffer:bool -> int -> N.t -> t -> t
        val to_floatarray: t -> floatarray
      end
    module Vector: functor (T: ScalarType_t) -> Type_t with type N.t = T.t
  end
= struct
    module type ScalarType_t =
      sig
        include Number.Type_t
        type elt_t
        val elt: (t, elt_t) Bigarray.kind
      end
    module type Type_t =
      sig
        module BA1 = Bigarray.Array1
        module N: ScalarType_t
        type t
        val init: int -> N.t -> t
        val empty: t
        val length: t -> int
        val get: t -> int -> N.t
        val ( .@() ): t -> int -> N.t
        val set: t -> int -> N.t -> unit
        val ( .@()<- ): t -> int -> N.t -> unit
        val incr: t -> int -> unit
        val ( .+() ): t -> int -> unit
        val incr_by: t -> int -> N.t -> unit
        val ( .+()<- ): t -> int -> N.t -> unit
        val decr: t -> int -> unit
        val ( .-() ): t -> int -> unit
        val decr_by: t -> int -> N.t -> unit
        val ( .-()<- ): t -> int -> N.t -> unit
        val sub: t -> int -> int -> t
        val blit: t -> t -> unit
        val fill: t -> N.t -> unit
        val resize: ?is_buffer:bool -> int -> N.t -> t -> t
        val to_floatarray: t -> floatarray
      end
    module Vector (T: ScalarType_t) = (* This should work with float too *)
      struct
        module BA1 = Bigarray.Array1
        module N = T
        type t = (N.t, T.elt_t, Bigarray.c_layout) BA1.t
        let init n a =
          let res = BA1.create T.elt Bigarray.C_layout n in
          BA1.fill res a;
          res
        let empty = init 0 N.zero
        let length = BA1.dim
        let get = BA1.get
        let ( .@() ) = BA1.get
        let set = BA1.set
        let ( .@()<- ) = BA1.set
        let incr ba n =
          BA1.get ba n |> N.add N.one |> BA1.set ba n
        let ( .+() ) = incr
        let incr_by ba n v =
          BA1.get ba n |> N.add v |> BA1.set ba n
        let ( .+()<- ) = incr_by
        let decr ba n =
          N.sub (BA1.get ba n) N.one |> BA1.set ba n
        let ( .-() ) = decr
        let decr_by ba n v =
          N.sub (BA1.get ba n) v |> BA1.set ba n
        let ( .-()<- ) = decr_by
        let sub = BA1.sub
        let blit = BA1.blit
        let fill = BA1.fill
        let resize ?(is_buffer = false) n zero v =
          let l = length v in
          if n > l then begin
            let res =
              init begin
                if is_buffer then
                  max n (l * 14 / 10)
                else
                  n
              end zero in
            BA1.blit v (BA1.sub res 0 l);
            res
          end else if n < l && not is_buffer then
            (* We have to resize in order to honour the request *)
            let res = init n zero in
            BA1.blit (BA1.sub v 0 n) res;
            res
          else
            v
        let to_floatarray v =
          let l = length v in
          let res = Float.Array.make l 0. in
          for i = 0 to l - 1 do
            N.to_float v.@(i) |> Float.Array.set res i
          done;
          res
      end
  end

