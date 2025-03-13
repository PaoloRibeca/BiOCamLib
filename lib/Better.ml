(*
    Better.ml -- (c) 2015-2025 Paolo Ribeca, <paolo.ribeca@gmail.com>

    This file is part of BiOCamLib, the OCaml foundations upon which
    a number of the bioinformatics tools I developed are built.

    Better.ml implements a number of general-purpose tools useful
    to write OCaml programs. In particular, it contains several
    improvements to OCaml standard library modules.

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

(* We extend some Stdlib types with additional functionality *)

let min_max a b =
  if compare a b > 0 then
    b, a
  else
    a, b

module Option:
  sig
    val unbox: 'a option -> 'a
    val unbox_def: 'a -> 'a option -> 'a
  end
= struct
    let unbox = function
      | None -> assert false
      | Some r -> r
    let unbox_def def = function
      | None -> def
      | Some r -> r
  end

module type PeanoInt_t =
  sig
    type t
    val succ: t -> t
    val pred: t -> t
  end
module type IncrementableType_t =
  sig
    type tt
    val get_and_incr: tt ref -> tt
    val incr_and_get: tt ref -> tt
    val get_and_decr: tt ref -> tt
    val decr_and_get: tt ref -> tt
  end
module type PlusPlus_t =
  sig
    include IncrementableType_t
    val ( !++ ): tt ref -> tt
    val ( ++! ): tt ref -> tt
    val ( !-- ): tt ref -> tt
    val ( --! ): tt ref -> tt
  end
module MakeIncrementable (T: PeanoInt_t): IncrementableType_t with type tt = T.t =
  struct
    type tt = T.t
    let get_and_incr r =
      let res = !r in
      r := T.succ !r;
      res
    let incr_and_get r =
      r := T.succ !r;
      !r
    let get_and_decr r =
      let res = !r in
      r := T.pred !r;
      res
    let decr_and_get r =
      r := T.pred !r;
      !r
  end
module IncrementableInt: IncrementableType_t with type tt = int = (* Optimisation using incr/decr *)
  struct
    type tt = int
    let get_and_incr ir =
      let res = !ir in
      incr ir;
      res
    let incr_and_get ir =
      incr ir;
      !ir
    let get_and_decr ir =
      let res = !ir in
      decr ir;
      res
    let decr_and_get ir =
      decr ir;
      !ir
  end
module IncrementableIntZ: IncrementableType_t with type tt = Z.t = MakeIncrementable(Z)
module MakePlusPlus (T: IncrementableType_t): PlusPlus_t with type tt = T.tt =
  struct
    include T
    let ( !++ ) r = T.get_and_incr r [@@inline]
    let ( ++! ) r = T.incr_and_get r [@@inline]
    let ( !-- ) r = T.get_and_decr r [@@inline]
    let ( --! ) r = T.decr_and_get r [@@inline]
  end

module Int:
  sig
    include module type of Int
    include module type of MakePlusPlus(IncrementableInt)
  end
= struct
    include Int
    include MakePlusPlus(IncrementableInt)
    let compare a b = a - b [@@inline] (* Optimisation (hopefully) *)
  end
module IntZ:
  sig
    include module type of Z
    include module type of MakePlusPlus(IncrementableIntZ)
  end
= struct
    include Z
    include MakePlusPlus(IncrementableIntZ)
  end

module Bytes:
  sig
    include module type of Bytes
    val ( ^ ): bytes -> bytes -> bytes
    val ( .@() ): bytes -> int -> char
    val ( .@()<- ): bytes -> int -> char -> unit
    (* In-place flipping *)
    val rev: bytes -> unit
    val accum: bytes ref -> bytes -> unit
  end
= struct
    include Bytes
    let ( ^ ) = Bytes.cat
    let ( .@() ) = Bytes.get
    let ( .@()<- ) = Bytes.set
    let rev b =
      let red_len = length b - 1 in
      let half_len = red_len / 2 in
      for i = 0 to half_len do
        let idx = red_len - i in
        let c = b.@(i) in
        b.@(i) <- b.@(idx);
        b.@(idx) <- c
      done
      let accum br b = br := !br ^ b
  end

module Str =
  struct
    include Str
    let matches re s =
      try
        ignore (Str.search_forward re s 0);
        true
      with Not_found ->
        false
  end

module String:
  sig
    include module type of String
    val ( ^ ): string -> string -> string
    val ( .@() ): string -> int -> char
    val rev: string -> string
    val accum: string ref -> string -> unit
    val pluralize: ?plural:string -> one:'a -> string -> 'a -> string
    val pluralize_int : ?plural:string -> string -> int -> string
    val pluralize_float : ?plural:string -> string -> float -> string
    module Split:
      sig
        val as_list: Str.regexp -> string -> string list
        val as_array: Str.regexp -> string -> string array
        val on_char_as_list: char -> string -> string list
        val on_char_as_array: char -> string -> string array
        val delim_as_list: Str.regexp -> string -> string list
        val delim_as_array: Str.regexp -> string -> string array
        val full_as_list: Str.regexp -> string -> Str.split_result list
        val full_as_array: Str.regexp -> string -> Str.split_result array
      end
    module TermIO:
      sig
        val bold: string -> string
        val italic: string -> string
        val under: string -> string
        val grey: string -> string
        val red: string -> string
        val green: string -> string
        val blue: string -> string
        val clear: string
        val column: int -> string
        val return: string
      end
  end
= struct
    include String
    let ( ^ ) = Stdlib.( ^ )
    let ( .@() ) = String.get
    let rev s =
      (* This function allocates memory *)
      let b = Bytes.of_string s in
      Bytes.rev b;
      Bytes.unsafe_to_string b
    let accum sr s = sr := !sr ^ s
    let pluralize (type a) ?(plural = "") ~(one:a) s n =
      if n = one then
        s
      else if plural <> "" then
        plural
      else
        s ^ "s"
    let pluralize_int = pluralize ~one:1
    let pluralize_float = pluralize ~one:1.
    module Split =
      struct
        let as_list = Str.split
        let as_array re s = Str.split re s |> Array.of_list
        (* All splits that were previously done with one-char regexps can now be done with the following two *)
        let on_char_as_list = String.split_on_char
        let on_char_as_array c s = String.split_on_char c s |> Array.of_list
        let delim_as_list = Str.split_delim
        let delim_as_array re s = Str.split_delim re s |> Array.of_list
        let full_as_list = Str.full_split
        let full_as_array re s = Str.full_split re s |> Array.of_list
      end
    module TermIO =
      struct
        let bold = Printf.sprintf "\027[1m%s\027[0m"
        let italic = Printf.sprintf "\027[3m%s\027[0m"
        let under = Printf.sprintf "\027[4m%s\027[0m"
        let grey = Printf.sprintf "\027[38;5;7m%s\027[0m"
        let red = Printf.sprintf "\027[38;5;1m%s\027[0m"
        let green = Printf.sprintf "\027[38;5;2m%s\027[0m"
        let blue = Printf.sprintf "\027[38;5;4m%s\027[0m"
        let clear = "\027[2K"
        let column = Printf.sprintf "\027[%dG"
        let return = column 1
      end
  end

module List:
  sig
    include module type of List
    val accum: 'a list ref -> 'a -> unit
    val pop: 'a list ref -> 'a
    val pop_opt: 'a list ref -> 'a option
  end
= struct
    include List
    let accum rl el = rl := el :: !rl
    let pop rl =
      match !rl with
      | [] -> raise Not_found
      | hd :: tail ->
        rl := tail;
        hd
    let pop_opt rl =
      match !rl with
      | [] -> None
      | hd :: tail ->
        rl := tail;
        Some hd
  end

(* Note that due to the different arities for the constructors of floatarray and 'a Array.t,
    the following only works if we define an auxiliary type tt first and we make it the same as t later *)
module type Array_t =
  sig
    type 'a tt
    type 'a elt_tt
    val make: int -> 'a elt_tt -> 'a tt
    val length: 'a tt -> int
    val get: 'a tt -> int -> 'a elt_tt
    val unsafe_get: 'a tt -> int -> 'a elt_tt
    val set: 'a tt -> int -> 'a elt_tt -> unit
    val unsafe_set: 'a tt -> int -> 'a elt_tt -> unit
    val sub: 'a tt -> int -> int -> 'a tt
    val blit: 'a tt -> int -> 'a tt -> int -> int -> unit
    val iter: ('a elt_tt -> unit) -> 'a tt -> unit
    val iteri: (int -> 'a elt_tt -> unit) -> 'a tt -> unit
    val iter2: ('a elt_tt -> 'b elt_tt -> unit) -> 'a tt -> 'b tt -> unit
    val map: ('a elt_tt -> 'b elt_tt) -> 'a tt -> 'b tt
    val mapi: (int -> 'a elt_tt -> 'b elt_tt) -> 'a tt -> 'b tt
    val of_list: 'a elt_tt list -> 'a tt
    val to_list: 'a tt -> 'a elt_tt list
  end
module type ExtendedArrayFunctionality_t =
  sig
    type 'a tt
    type 'a elt_tt
    val ( .@() ): 'a tt -> int -> 'a elt_tt
    val ( .@()<- ): 'a tt -> int -> 'a elt_tt -> unit
    val riter: ('a elt_tt -> unit) -> 'a tt -> unit
    val riteri: (int -> 'a elt_tt -> unit) -> 'a tt -> unit
    val iter2i: (int -> 'a elt_tt -> 'b elt_tt -> unit) -> 'a tt -> 'b tt -> unit
    val riter2: ('a elt_tt -> 'b elt_tt -> unit) -> 'a tt -> 'b tt -> unit
    val riter2i: (int -> 'a elt_tt -> 'b elt_tt -> unit) -> 'a tt -> 'b tt -> unit
    val map2: ('a elt_tt -> 'b elt_tt -> 'c elt_tt) -> 'a tt -> 'b tt -> 'c tt
    val map2i: (int -> 'a elt_tt -> 'b elt_tt -> 'c elt_tt) -> 'a tt -> 'b tt -> 'c tt
    val resize: ?is_buffer:bool -> int -> 'a elt_tt -> 'a tt -> 'a tt
    val of_rlist: 'a elt_tt list -> 'a tt
    val to_rlist: 'a tt -> 'a elt_tt list
  end
module MakeExtendedArrayFunctionality (Array: Array_t): ExtendedArrayFunctionality_t with type 'a tt = 'a Array.tt and type 'a elt_tt = 'a Array.elt_tt =
  struct
    include Array
    let ( .@() ) = Array.get
    let ( .@()<- ) = Array.set
    let riter f a =
      for i = Array.length a - 1 downto 0 do
        Array.unsafe_get a i |> f
      done
    let riteri f a =
      for i = Array.length a - 1 downto 0 do
        Array.unsafe_get a i |> f i
      done
    let check__raise_different_lengths l_1 l_2 =
      if l_1 <> l_2 then 
        Invalid_argument
          (Printf.sprintf "(%s): The two arrays have different lengths (%d and %d)" __FUNCTION__ l_1 l_2)
        |> raise
    let iter2i f a_1 a_2 =
      let l = Array.length a_1 in
      Array.length a_2 |> check__raise_different_lengths l;
      for i = 0 to l - 1 do
        f i (Array.unsafe_get a_1 i) (Array.unsafe_get a_2 i)
      done
    let riter2 f a_1 a_2 =
      let l = Array.length a_1 in
      Array.length a_2 |> check__raise_different_lengths l;
      for i = l - 1 downto 0 do
        f (Array.unsafe_get a_1 i) (Array.unsafe_get a_2 i)
      done
    let riter2i f a_1 a_2 =
      let l = Array.length a_1 in
      Array.length a_2 |> check__raise_different_lengths l;
      for i = l - 1 downto 0 do
        f i (Array.unsafe_get a_1 i) (Array.unsafe_get a_2 i)
      done
    let map2 f a_1 a_2 =
      let l = Array.length a_1 in
      Array.length a_2 |> check__raise_different_lengths l;
      Array.mapi (fun i e_1 -> f e_1 a_2.@(i)) a_1
    let map2i f a_1 a_2 =
      let l = Array.length a_1 in
      Array.length a_2 |> check__raise_different_lengths l;
      Array.mapi (fun i e_1 -> f i e_1 a_2.@(i)) a_1
    let resize ?(is_buffer = false) n fill_with a =
      let l = Array.length a in
      if n > l then begin
        let res =
          Array.make begin
            if is_buffer then
              max n (l * 14 / 10)
            else
              n
          end fill_with in
        Array.blit a 0 res 0 l;
        res
      end else if n < l && not is_buffer then
        (* We have to resize in order to honour the request *)
        Array.sub a 0 n
      else
        a
    let of_rlist l =
      List.rev l |> Array.of_list
    let to_rlist a =
      Array.to_list a |> List.rev
  end
module Array:
  sig
    include module type of Array
    include ExtendedArrayFunctionality_t with type 'a tt = 'a t and type 'a elt_tt = 'a
  end
= struct
    include Array
    include MakeExtendedArrayFunctionality (
      struct
        include Array
        type 'a tt = 'a t
        type 'a elt_tt = 'a
      end
    )
  end

module Float:
  sig
    include module type of Float
    val round: float -> float
    module Array:
      sig
        include module type of Float.Array
        include ExtendedArrayFunctionality_t with type 'a tt = t and type 'a elt_tt = float
      end
  end
= struct
    include Float
    let round x = if x >= 0. then floor (x +. 0.5) else ceil (x -. 0.5)
    module Array =
      struct
        include Float.Array
        include MakeExtendedArrayFunctionality (
          struct
            include Float.Array
            type 'a tt = t
            type 'a elt_tt = float
          end
        )
      end
  end

module Printf:
  sig
    include module type of Printf
    type mode_t =
      | Time
      | Space
      | Empty
    val tfprintf: ?mode:mode_t -> out_channel -> ('a, out_channel, unit) format -> 'a
    val tprintf: ?mode:mode_t -> ('a, out_channel, unit) format -> 'a
    val teprintf: ?mode:mode_t -> ('a, out_channel, unit) format -> 'a
    val pfprintf: out_channel -> ('a, out_channel, unit) format -> 'a
    val pprintf: ('a, out_channel, unit) format -> 'a
    val peprintf: ('a, out_channel, unit) format -> 'a
    val ptfprintf: ?mode:mode_t -> out_channel -> ('a, out_channel, unit) format -> 'a
    val ptprintf: ?mode:mode_t -> ('a, out_channel, unit) format -> 'a
    val pteprintf: ?mode:mode_t -> ('a, out_channel, unit) format -> 'a
    (* Channel printers for String.TermIO *)
    module TermIO:
      sig
        val bold: out_channel -> string -> unit
        val italic: out_channel -> string -> unit
        val under: out_channel -> string -> unit
        val grey: out_channel -> string -> unit
        val red: out_channel -> string -> unit
        val green: out_channel -> string -> unit
        val blue: out_channel -> string -> unit
        val clear: out_channel -> unit
        val column: int -> out_channel -> unit
        val return: out_channel -> unit
      end
  end
= struct
    include Printf
    type mode_t =
      | Time
      | Space
      | Empty
    let tfprintf ?(mode = Time) ch =
      let t = Unix.localtime (Unix.time ()) in
      begin match mode with
      | Time ->
        Printf.sprintf "%s %s %2d %02d:%02d:%02d %4d" begin
          match t.Unix.tm_wday with
          | 0 -> "Sun" | 1 -> "Mon" | 2 -> "Tue" | 3 -> "Wed"
          | 4 -> "Thu" | 5 -> "Fri" | 6 -> "Sat"
          | _ -> assert false
        end begin
          match t.Unix.tm_mon with
          | 0 -> "Jan" | 1 -> "Feb" | 2 -> "Mar" | 3 -> "Apr"
          | 4 -> "May" | 5 -> "Jun" | 6 -> "Jul" | 7 -> "Aug"
          | 8 -> "Sep" | 9 -> "Oct" | 10 -> "Nov" | 11 -> "Dec"
          | _ -> assert false
        end t.Unix.tm_mday t.Unix.tm_hour t.Unix.tm_min t.Unix.tm_sec (1900 + t.Unix.tm_year)
          |> String.TermIO.blue |> Printf.fprintf ch "%s -- "
      | Space -> Printf.fprintf ch "                         -- "
      | Empty -> ()
      end;
      Printf.fprintf ch
    let tprintf ?(mode = Time) = tfprintf ~mode stdout
    let teprintf ?(mode = Time) = tfprintf ~mode stderr
    let proto_pfprintf ?(mode = Time) f ch =
      begin match mode with
      | Time -> Unix.getpid () |> Printf.sprintf "%07d" |> String.TermIO.blue |> Printf.fprintf ch "[#%s]: "
      | Space -> Printf.fprintf ch "            "
      | Empty -> ()
      end;
      f ch
    let pfprintf ch = proto_pfprintf ~mode:Time Printf.fprintf ch
    let pprintf w = pfprintf stdout w
    let peprintf w = pfprintf stderr w
    let ptfprintf ?(mode = Time) = proto_pfprintf ~mode (tfprintf ~mode)
    let ptprintf ?(mode = Time) = ptfprintf ~mode stdout
    let pteprintf ?(mode = Time) = ptfprintf ~mode stderr
    module TermIO =
      struct
        let _printer f = fun ch s -> f s |> Printf.fprintf ch "%s"
        let bold = _printer String.TermIO.bold
        let italic = _printer String.TermIO.italic
        let under = _printer String.TermIO.under
        let grey = _printer String.TermIO.grey
        let red = _printer String.TermIO.red
        let green = _printer String.TermIO.green
        let blue = _printer String.TermIO.blue
        let clear ch = Printf.fprintf ch "%s" String.TermIO.clear
        let column n ch = String.TermIO.column n |> Printf.fprintf ch "%s"
        let return ch = Printf.fprintf ch "%s" String.TermIO.return
      end
  end

(* General C++-style iterator *)
module type Iterator_t =
  sig
    type 'a init_t
    type 'a t
    type 'a ret_t
    val empty: unit -> 'a t
    val is_empty: 'a t -> bool
    val make: 'a init_t -> 'a t
    val assign: 'a t -> 'a t -> unit
    (* None means there are no elements left *)
    val get: 'a t -> 'a ret_t option
    val get_and_incr: 'a t -> 'a ret_t option
    val incr: 'a t -> unit
  end
(* Implementation for Stdlib modules built upon Seq *)
module Iterator:
  Iterator_t with type 'a init_t := 'a Seq.t and type 'a ret_t := 'a
= struct
    type 'a t = 'a Seq.t ref
    let empty () = ref Seq.empty
    let is_empty it = !it () = Seq.Nil
    let make seq = ref seq
    let assign it seq = it := !seq
    let get it =
      match !it () with
      | Seq.Nil -> None
      | Cons (deref, _) -> Some deref
    let get_and_incr it =
      match !it () with
      | Seq.Nil -> None
      | Cons (deref, next) -> it := next; Some deref
    let incr it =
      match !it () with
      | Seq.Nil -> ()
      | Cons (_, next) -> it := next
  end

(* Frequently used module idioms *)

module type IntParameter_t = sig val n: int end
module type TypeContainer_t = sig type t end

(* Same as Set.OrderedType or Map.OrderedType *)
module type ComparableType_t =
  sig
    type t
    val compare: t -> t -> int
  end

module Set:
  sig
    include module type of Set
    module Make (O: ComparableType_t):
      sig
        include module type of Set.Make(O)
        val iteri: (int -> elt -> unit) -> t -> unit
        val elements_array: t -> elt array
        val find_next: elt -> t -> elt
        val find_next_opt: elt -> t -> elt option
      end
  end
= struct
    include Set
    module Make (O: ComparableType_t) =
      struct
        include Set.Make(O)
        let iteri f =
          let cntr = ref 0 in
          iter
            (fun el ->
              f !cntr el;
              incr cntr)
        let elements_array s =
          let n = cardinal s in
          if n > 0 then begin
            let res = min_elt s |> Array.make n in
            iteri
              (fun i el ->
                res.(i) <- el)
              s;
            res
          end else
            [||]
        let find_next lo = find_first (fun k -> O.compare k lo > 0)
        let find_next_opt lo = find_first_opt (fun k -> O.compare k lo > 0)
      end
  end

module Map:
  sig
    include module type of Map
    module Make (O: ComparableType_t):
      sig
        include module type of Map.Make(O)
        val replace: key -> 'a -> 'a t -> 'a t
        val iteri: (int -> key -> 'a -> unit) -> 'a t -> unit
        val bindings_array: 'a t -> (key * 'a) array
        val find_next: key -> 'a t -> key * 'a
        val find_next_opt: key -> 'a t -> (key * 'a) option
      end
  end
= struct
    include Map
    module Make (O: ComparableType_t) =
      struct
        include Map.Make(O)
        let replace k v m =
          remove k m |> add k v
        let iteri f =
          let cntr = ref 0 in
          iter
            (fun k v ->
              f !cntr k v;
              incr cntr)
        let bindings_array m =
          let n = cardinal m in
          if n > 0 then begin
            let res = min_binding m |> Array.make n in
            iteri
              (fun i k v ->
                res.(i) <- k, v)
              m;
            res
          end else
            [||]
        let find_next lo = find_first (fun k -> O.compare k lo > 0)
        let find_next_opt lo = find_first_opt (fun k -> O.compare k lo > 0)
      end
  end

module MakeComparable (T: TypeContainer_t): ComparableType_t with type t = T.t =
  struct
    type t = T.t
    (* Informs the compiler that polymorphism is not needed *)
    let compare (a:t) (b:t) = compare a b [@@inline]
  end
module MakeRComparable (T: TypeContainer_t): ComparableType_t with type t = T.t =
  struct
    type t = T.t
    (* Informs the compiler that polymorphism is not needed *)
    let compare (a:t) (b:t) = compare b a [@@inline]
  end

(* A few frequently used modules.
   We use the following modules rather than Char, Int, String in order to enforce explicit typing of compare *)
module ComparableChar = MakeComparable (struct type t = char end)
module CharSet: module type of Set.Make (ComparableChar) = Set.Make (ComparableChar)
module CharMap: module type of Map.Make (ComparableChar) = Map.Make (ComparableChar)
module RComparableChar = MakeRComparable (struct type t = char end)
module CharRSet: module type of Set.Make (RComparableChar) = Set.Make (RComparableChar)
module CharRMap: module type of Map.Make (RComparableChar) = Map.Make (RComparableChar)
module ComparableInt = MakeComparable (struct type t = Int.t end) (* Our Int! *)
module IntSet: module type of Set.Make (ComparableInt) = Set.Make (ComparableInt)
module IntMap: module type of Map.Make (ComparableInt) = Map.Make (ComparableInt)
module RComparableInt = MakeRComparable (struct type t = Int.t end) (* Our Int! *)
module IntRSet: module type of Set.Make (RComparableInt) = Set.Make (RComparableInt)
module IntRMap: module type of Map.Make (RComparableInt) = Map.Make (RComparableInt)
module ComparableIntZ = MakeComparable (struct type t = IntZ.t end)
module IntZSet: module type of Set.Make (ComparableIntZ) = Set.Make (ComparableIntZ)
module IntZMap: module type of Map.Make (ComparableIntZ) = Map.Make (ComparableIntZ)
module RComparableIntZ = MakeRComparable (struct type t = IntZ.t end)
module IntZRSet: module type of Set.Make (RComparableIntZ) = Set.Make (RComparableIntZ)
module IntZRMap: module type of Map.Make (RComparableIntZ) = Map.Make (RComparableIntZ)
module ComparableFloat = MakeComparable (struct type t = Float.t end) (* Our Float! *)
module FloatSet: module type of Set.Make (ComparableFloat) = Set.Make (ComparableFloat)
module FloatMap: module type of Map.Make (ComparableFloat) = Map.Make (ComparableFloat)
module RComparableFloat = MakeRComparable (struct type t = Float.t end) (* Our Float! *)
module FloatRSet: module type of Set.Make (RComparableFloat) = Set.Make (RComparableFloat)
module FloatRMap: module type of Map.Make (RComparableFloat) = Map.Make (RComparableFloat)
module ComparableString = MakeComparable (struct type t = String.t end) (* Our String! *)
module StringSet: module type of Set.Make (ComparableString) = Set.Make (ComparableString)
module StringMap: module type of Map.Make (ComparableString) = Map.Make (ComparableString)
module RComparableString = MakeRComparable (struct type t = String.t end) (* Our String! *)
module StringRSet: module type of Set.Make (RComparableString) = Set.Make (RComparableString)
module StringRMap: module type of Map.Make (RComparableString) = Map.Make (RComparableString)

(* Same as Hashtbl.HashedType *)
module type HashableType_t =
  sig
    type t
    val equal: t -> t -> bool
    val hash: t -> int
  end
(* Optimisation for integers *)
module IntHash: HashableType_t with type t = Int.t =
  struct
    type t = Int.t (* Our Int! *)
    let equal (i:int) (j:int) = (i - j = 0) [@@inline] (*(i = j)*)
    let hash (i:int) = i [@@inline] (* land max_int *)
  end

module MakeHashable (T: TypeContainer_t): HashableType_t with type t = T.t =
  struct
    type t = T.t
    (* Informs the compiler that polymorphism is not needed *)
    let equal (a:t) (b:t) = a = b [@@inline]
    let hash (a:t) = Hashtbl.hash a [@@inline]
  end
module MakeSeededHashable (T: TypeContainer_t) (I: IntParameter_t): HashableType_t with type t = T.t =
  struct
    type t = T.t
    (* Informs the compiler that polymorphism is not needed *)
    let equal (a:t) (b:t) = a = b [@@inline]
    let hash (a:t) = Hashtbl.seeded_hash I.n a [@@inline]
  end

module Hashtbl:
  sig
    include module type of Hashtbl
    val map: ?random:bool -> ('a -> 'b) -> ('c, 'a) Hashtbl.t -> ('c, 'b) Hashtbl.t
    val mapi: ?random:bool -> ('a -> 'b -> 'c) -> ('a, 'b) Hashtbl.t -> ('a, 'c) Hashtbl.t
  end
= struct
    include Hashtbl
    let map ?(random = false) f h =
      let res = Hashtbl.create ~random (Hashtbl.length h) in
      Hashtbl.iter (fun k v -> f v |> Hashtbl.add res k) h;
      res
    let mapi ?(random = false) f h =
      let res = Hashtbl.create ~random (Hashtbl.length h) in
      Hashtbl.iter (fun k v -> f k v |> Hashtbl.add res k) h;
      res
  end

module IntHashtbl = Hashtbl.Make (IntHash)
module IntZHashtbl = Hashtbl.Make (MakeHashable(IntZ))
module StringHashtbl = Hashtbl.Make (MakeHashable(String))

