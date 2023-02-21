(*
    Tools.ml -- (c) 2015-2023 Paolo Ribeca, <paolo.ribeca@gmail.com>

    This file is part of BiOCamLib, the OCaml foundations upon which
    a number of the bioinformatics tools I developed are built.

    Tools.ml implements a number of general-purpose tools useful
    to implement OCaml programs. In particular, it contains:
     * several useful additions to the OCaml standard library
     * a module to parse command-line options.

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

module Float:
  sig
    include module type of Float
    val round: float -> float
  end
= struct
    include Float
    let round x = if x >= 0. then floor (x +. 0.5) else ceil (x -. 0.5)
  end

module String:
  sig
    include module type of String
    val ( ^ ): string -> string -> string
    val ( .@() ): string -> int -> char
    val accum: string ref -> string -> unit
    val pluralize: ?plural:string -> one:'a -> string -> 'a -> string
    val pluralize_int : ?plural:string -> string -> int -> string
    val pluralize_float : ?plural:string -> string -> float -> string
  end
= struct
    include String
    let ( ^ ) = Stdlib.( ^ )
    let ( .@() ) = String.get
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
  end

module Bytes:
  sig
    include module type of Bytes
    val ( ^ ): bytes -> bytes -> bytes
    val ( .@() ): bytes -> int -> char
    val ( .@()<- ): bytes -> int -> char -> unit
    val accum: bytes ref -> bytes -> unit
  end
= struct
    include Bytes
    let ( ^ ) = Bytes.cat
    let ( .@() ) = Bytes.get
    let ( .@()<- ) = Bytes.set
    let accum br b = br := !br ^ b
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


module Array:
  sig
    include module type of Array
    val of_rlist: 'a list -> 'a array
    val riter: ('a -> unit) -> 'a array -> unit
    val riteri: (int -> 'a -> unit) -> 'a array -> unit
  end
= struct
    include Array
    let of_rlist l =
      List.rev l |> Array.of_list
    let riter f a =
      for i = Array.length a - 1 downto 0 do
        Array.unsafe_get a i |> f
      done
    let riteri f a =
      for i = Array.length a - 1 downto 0 do
        Array.unsafe_get a i |> f i
      done
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
        Printf.fprintf ch "%s %s %2d %02d:%02d:%02d %4d -- " begin
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
      | Space -> Printf.fprintf ch "                         -- "
      | Empty -> ()
      end;
      Printf.fprintf ch
    let tprintf ?(mode = Time) = tfprintf ~mode stdout
    let teprintf ?(mode = Time) = tfprintf ~mode stderr
    let proto_pfprintf ?(mode = Time) f ch =
      begin match mode with
      | Time -> Unix.getpid () |> Printf.fprintf ch "[%07d] "
      | Space -> Printf.fprintf ch "          "
      | Empty -> ()
      end;
      f ch
    let pfprintf ch = proto_pfprintf ~mode:Time Printf.fprintf ch
    let pprintf w = pfprintf stdout w
    let peprintf w = pfprintf stderr w
    let ptfprintf ?(mode = Time) = proto_pfprintf ~mode (tfprintf ~mode)
    let ptprintf ?(mode = Time) = ptfprintf ~mode stdout
    let pteprintf ?(mode = Time) = ptfprintf ~mode stderr
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

module IntHash =
  struct
    type t = int
    let equal (i: int) (j: int) = (i - j = 0) (*(i = j)*)
    let hash (i: int) = i (* land max_int *)
  end
module IntHashtbl = Hashtbl.Make (IntHash)

module Set:
  sig
    include module type of Set
    module Make (O:Set.OrderedType):
      sig
        include module type of Set.Make(O)
        val iteri: (int -> elt -> unit) -> t -> unit
        val find_next: elt -> t -> elt
        val find_next_opt: elt -> t -> elt option
      end
  end
= struct
    include Set
    module Make (O:Set.OrderedType) =
      struct
        include Set.Make(O)
        let iteri f =
          let cntr = ref 0 in
          iter
            (fun el ->
              f !cntr el;
              incr cntr)
        let find_next lo = find_first (fun k -> O.compare k lo > 0)
        let find_next_opt lo = find_first_opt (fun k -> O.compare k lo > 0)
      end
  end

module Map:
  sig
    include module type of Map
    module Make (O:Map.OrderedType):
      sig
        include module type of Map.Make(O)
        val iteri: (int -> key -> 'a -> unit) -> 'a t -> unit
        val find_next: key -> 'a t -> key * 'a
        val find_next_opt: key -> 'a t -> (key * 'a) option
      end
  end
= struct
    include Map
    module Make (O:Map.OrderedType) =
      struct
        include Map.Make(O)
        let iteri f =
          let cntr = ref 0 in
          iter
            (fun k v ->
              f !cntr k v;
              incr cntr)
        let find_next lo = find_first (fun k -> O.compare k lo > 0)
        let find_next_opt lo = find_first_opt (fun k -> O.compare k lo > 0)
      end
  end

(* Frequently used module idioms *)
module type TypeContainer_t = sig type t end
module MakeComparable (T: TypeContainer_t) =
  struct
    type t = T.t
    (* Informs the compiler that polymorphism is not needed *)
    let compare (a:t) (b:t) = compare a b
  end
module MakeRComparable (T: TypeContainer_t) =
  struct
    type t = T.t
    (* Informs the compiler that polymorphism is not needed *)
    let compare (a:t) (b:t) = compare b a
  end

(* A few frequently used modules.
   We use the following modules rather than Char, Int, String in order to enforce explicit typing of compare *)
module ComparableChar = MakeComparable (struct type t = char end)
module CharSet: module type of Set.Make (ComparableChar) = Set.Make (ComparableChar)
module CharMap: module type of Map.Make (ComparableChar) = Map.Make (ComparableChar)
module RComparableChar = MakeRComparable (struct type t = char end)
module CharRSet: module type of Set.Make (RComparableChar) = Set.Make (RComparableChar)
module CharRMap: module type of Map.Make (RComparableChar) = Map.Make (RComparableChar)
(* Optimisation for integers *)
module ComparableInt = struct type t = int let compare a b = a - b end
module IntSet: module type of Set.Make (ComparableInt) = Set.Make (ComparableInt)
module IntMap: module type of Map.Make (ComparableInt) = Map.Make (ComparableInt)
(* Optimisation for integers *)
module RComparableInt = struct type t = int let compare a b = b - a end
module IntRSet: module type of Set.Make (RComparableInt) = Set.Make (RComparableInt)
module IntRMap: module type of Map.Make (RComparableInt) = Map.Make (RComparableInt)
module ComparableFloat = MakeComparable (struct type t = float end)
module FloatSet: module type of Set.Make (ComparableFloat) = Set.Make (ComparableFloat)
module FloatMap: module type of Map.Make (ComparableFloat) = Map.Make (ComparableFloat)
module RComparableFloat = MakeRComparable (struct type t = float end)
module FloatRSet: module type of Set.Make (RComparableFloat) = Set.Make (RComparableFloat)
module FloatRMap: module type of Map.Make (RComparableFloat) = Map.Make (RComparableFloat)
module ComparableString = MakeComparable (struct type t = string end)
module StringSet: module type of Set.Make (ComparableString) = Set.Make (ComparableString)
module StringMap: module type of Map.Make (ComparableString) = Map.Make (ComparableString)
module RComparableString = MakeRComparable (struct type t = string end)
module StringRSet: module type of Set.Make (RComparableString) = Set.Make (RComparableString)
module StringRMap: module type of Map.Make (RComparableString) = Map.Make (RComparableString)

(* An ordered multimap is a map 'key -> 'val Set (no duplications allowed) *)
module Multimap (OKey:Map.OrderedType) (OVal:Set.OrderedType) =
  struct
    module KeyOrd = OKey
    module ValOrd = OVal
    (* Keys have type OKey.t, values OVal.t *)
    module KeyMap = Map.Make (OKey)
    module ValSet = Set.Make (OVal)
    (* To store the values *)
    type t = ValSet.t KeyMap.t
    let empty = KeyMap.empty
    let is_empty = KeyMap.is_empty
    let add k v om =
      try
        let s = KeyMap.find k om in
        KeyMap.add k (ValSet.add v s) (KeyMap.remove k om)
      with Not_found ->
        KeyMap.add k (ValSet.singleton v) om
    let remove_set =
      KeyMap.remove
    let remove k v om =
      try
        let s = KeyMap.find k om in
        let s = ValSet.remove v s in
        if ValSet.is_empty s then
          KeyMap.remove k om
        else
          KeyMap.add k (ValSet.remove v s) (KeyMap.remove k om)
      with Not_found ->
        om
    let cardinal_set = KeyMap.cardinal
    let cardinal om =
      let res = ref 0 in
      KeyMap.iter
        (fun _ s ->
          res := !res + ValSet.cardinal s)
        om;
      !res
    let iter_set = KeyMap.iter
    let iteri_set f =
      let i = ref 0 in
      KeyMap.iter
        (fun k s ->
          f !i k s;
          incr i)
    let iter f =
      KeyMap.iter
        (fun k s ->
          ValSet.iter
            (fun v ->
              f k v)
            s)
    let iteri f =
      let i = ref 0 in
      KeyMap.iter
        (fun k s ->
          ValSet.iter
            (fun v ->
              f !i k v;
              incr i)
            s)
    let max_binding = KeyMap.max_binding
    let min_binding = KeyMap.min_binding
  end

module TransitiveClosure =
  struct
    module type T_t =
      sig
        type element_t
        type t
        val empty: unit -> t
        val add_one: t -> element_t -> t
        val add_two: t -> element_t -> element_t -> t
        val iter: (unit -> element_t -> unit) -> t -> unit
      end
    module Make (T: TypeContainer_t):
      T_t with type element_t := T.t
    = struct
        type element_t = T.t
        type class_t =
          | Singleton of element_t
          | Node of class_t * class_t
        module ElementMap = Map.Make (MakeComparable (T))
        type t = {
          (* Each class has an associated unique numerical ID for fast comparison *)
          elements_to_classes: (int * class_t) ElementMap.t;
          ids_to_classes: class_t IntMap.t;
          id: int
        }
        let rec iter_class f = function
          | Singleton el -> f el
          | Node (cl_l, cl_r) ->
            iter_class f cl_l;
            iter_class f cl_r
        let empty () = {
          elements_to_classes = ElementMap.empty;
          ids_to_classes = IntMap.empty;
          id = 0
        }
        let add_one tc el =
          match ElementMap.find_opt el tc.elements_to_classes with
          | None ->
            (* The element is in its own separate equivalence class *)
            let cl = Singleton el in
            { elements_to_classes = ElementMap.add el (tc.id, cl) tc.elements_to_classes;
              ids_to_classes = IntMap.add tc.id cl tc.ids_to_classes;
              id = tc.id + 1 }
          | Some _ ->
            tc
        (* Adds two equivalent elements *)
        let add_two tc el_1 el_2 =
          let tc = add_one tc el_1 in
          let tc = add_one tc el_2 in
          let id_1, cl_1 = ElementMap.find el_1 tc.elements_to_classes
          and id_2, cl_2 = ElementMap.find el_2 tc.elements_to_classes in
          if id_1 = id_2 then
            (* Nothing to do - elements belonging to the same class *)
            tc
          else begin
            let cl = Node (cl_1, cl_2) and res = ref tc.elements_to_classes in
            iter_class
              (fun el ->
                res := ElementMap.add el (tc.id, cl) !res)
              cl;
            { elements_to_classes = !res;
              ids_to_classes =
                IntMap.(remove id_1 tc.ids_to_classes |> remove id_2 |> add tc.id cl);
              id = tc.id + 1 }
          end
        let iter f' tc =
          IntMap.iter
            (fun _ ->
              let f = f' () in
              iter_class f)
            tc.ids_to_classes
      end
  end
module IntTransitiveClosure = TransitiveClosure.Make (struct type t = int end)
(* Optimised implementation for String that hashes elements *)
module StringTransitiveClosure:
  TransitiveClosure.T_t with type element_t := string
= struct
    type t = {
      tc: IntTransitiveClosure.t;
      string_to_int: (string, int) Hashtbl.t;
      int_to_string: (int, string) Hashtbl.t;
      id: int
    }
    let empty () = {
      tc = IntTransitiveClosure.empty ();
      string_to_int = Hashtbl.create 16;
      int_to_string = Hashtbl.create 16;
      id = 0
    }
    let add_one tc el =
      if Hashtbl.mem tc.string_to_int el then
        tc
      else begin
        Hashtbl.add tc.string_to_int el tc.id;
        Hashtbl.add tc.int_to_string tc.id el;
        { tc with
          tc = Hashtbl.find tc.string_to_int el |> IntTransitiveClosure.add_one tc.tc;
          id = tc.id + 1 }
      end
    let add_two tc el_1 el_2 =
      let tc = add_one tc el_1 in
      let tc = add_one tc el_2 in
      { tc with
        tc =
          IntTransitiveClosure.add_two tc.tc
            (Hashtbl.find tc.string_to_int el_1) (Hashtbl.find tc.string_to_int el_2) }
    let iter f' tc =
      IntTransitiveClosure.iter
        (fun () ->
          let string_f = f' () in
          fun i -> Hashtbl.find tc.int_to_string i |> string_f)
        tc.tc
  end

module Trie:
  sig
    type t
    val empty: t
    val add: t -> string -> t
    type result_t =
      | Not_found
      | Partial of string (* Partial match *)
      | Ambiguous of string list (* Multiple partial matches *)
      | Contained of string list (* Exact match, but also other longer matches containing it *)
      | Unique of string (* Exact match and no other containing matches *)
    val find: t -> string -> result_t
    (* Converts the result to string whenever it is possible to do so unambiguously *)
    val find_string: t -> string -> string
    val find_all: t -> string list
  end
= struct
    module CharMap = Map.Make (Char)
    type t = Node of t CharMap.t
    let empty = Node CharMap.empty
    let find_all t =
      let res = ref [] in
      let rec _find_all t s =
        let Node cm = t in
        CharMap.iter
          (fun c tt ->
            if c = '\000' then begin
              List.accum res s;
              assert (tt = empty)
            end else
              s ^ String.make 1 c |> _find_all tt)
          cm in
      _find_all t "";
      List.rev !res
    let add t s =
      let n = String.length s in
      let rec _add t i =
        let Node cm = t in
        if i = n then
          Node (CharMap.add '\000' empty cm)
        else
          match CharMap.find_opt s.[i] cm with
          | None ->
            let tail = Node (CharMap.singleton '\000' empty) |> ref in
            for ii = n - 1 downto i + 1 do
              tail := Node (CharMap.singleton s.[ii] !tail)
            done;
            Node (CharMap.add s.[i] !tail cm)
          | Some tt ->
            Node (CharMap.add s.[i] (i + 1 |> _add tt) cm) in
      _add t 0
    type result_t =
      | Not_found
      | Partial of string
      | Ambiguous of string list
      | Contained of string list
      | Unique of string
    let find_all_tails p t =
      let res = ref [] in
      let rec _find_all_tails t s =
        let Node cm = t in
        CharMap.iter
          (fun c tt ->
            if c = '\000' then
              (p ^ s) |> List.accum res
            else
              s ^ String.make 1 c |> _find_all_tails tt)
          cm in
      _find_all_tails t "";
      List.rev !res
    let find t s =
      let n = String.length s in
      let rec _find t i =
        let Node cm = t in
        let c = CharMap.cardinal cm in
        if i = n then begin
          if CharMap.mem '\000' cm then begin
            assert (CharMap.find '\000' cm = empty);
            match c with
            | 1 -> (* Exact match and no other containing matches *)
              Unique s
            | _ -> (* Exact match, but also other longer matches containing it *)
              Contained (find_all_tails s t)
          end else begin
            match c with
            | 0 ->
              Not_found
            | 1 -> (* Partial match *)
              begin match find_all_tails s t with
              | [ ss ] ->
                Partial ss
              | l -> (* Multiple partial matches *)
                Ambiguous l
              end
            | _ -> (* Multiple partial matches *)
              Ambiguous (find_all_tails s t)
          end
        end else begin
          match CharMap.find_opt s.[i] cm with
          | None ->
            Not_found
          | Some tt ->
            i + 1 |> _find tt
        end in
      _find t 0
    let find_string t s =
      match find t s with
      | Not_found -> ""
      | Partial s -> s
      | Ambiguous _ -> ""
      | Contained _ -> s
      | Unique s -> s
  end

(* General C++-style iterator *)
module type Sequable_t =
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
module Seq:
  Sequable_t with type 'a init_t := 'a Seq.t and type 'a ret_t := 'a
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

module Split =
  struct
    let fasta_name_re = Str.regexp "^>"
    let as_list = Str.split
    let as_array re s = Str.split re s |> Array.of_list
    (* All splits that were previously done with one-char regexps can now be done with the following two *)
    let on_char_as_list = String.split_on_char
    let on_char_as_array c s = String.split_on_char c s |> Array.of_list
  end

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
      end
  end

module Argv:
  sig
    type class_t =
      | Mandatory (* Implies no default *)
      | Optional (* Implies no default *)
      | Default of (unit -> string) (* Implies optional - the function prints the default *)
    (* The specs from which usage is produced are a tuple with the following elements:
        (1) Equivalent option names
        (2) Optional explanation for the argument(s)
        (3) Help text lines.
            An empty help means the option is private and no usage will be output for it
        (4) Class (can be mandatory, optional, default)
        (5) Parsing action when option is encountered *)
    type spec_t = string list * string option * string list * class_t * (string -> unit)
    (* Sets the header text *)
    val set_header: string -> unit
    (* Sets the text following the program name *)
    val set_synopsis: string -> unit
    val header: ?output:out_channel -> unit -> unit
    val synopsis: ?output:out_channel -> unit -> unit
    val usage: ?output:out_channel -> unit -> unit
    val markdown: ?output:out_channel -> unit -> unit
    val get_parameter: unit -> string
    val get_parameter_boolean: unit -> bool
    val get_parameter_int: unit -> int
    val get_parameter_float: unit -> float
    val get_parameter_int_pos: unit -> int
    val get_parameter_float_pos: unit -> float
    val get_parameter_int_non_neg: unit -> int
    val get_parameter_float_non_neg: unit -> float
    val get_parameter_int_percentage: unit -> int
    val get_parameter_float_fraction: unit -> float
    (* Consumes and returns all the parameters which are left on the command line *)
    val get_remaining_parameters: unit -> string array
    val parse: spec_t list -> unit
    (* Can be invoked from within the actions given as argument to parse() *)
    val parse_error: ?output:out_channel -> string -> unit
    (* Makes a textual separator between groups of options *)
    val make_separator: string -> spec_t
    val make_separator_multiline: string list -> spec_t
  end
= struct
    type class_t =
      | Mandatory
      | Optional
      | Default of (unit -> string)
    type spec_t = string list * string option * string list * class_t * (string -> unit)
    let _header = ref ""
    let set_header s = _header := s
    let _synopsis = ref ""
    let set_synopsis s = _synopsis := s
    let argv = Sys.argv
    let i = ref 1
    (* Both _usage and _md will be completed by parse () *)
    let _usage = ref ""
    let _md = ref ""
    let header ?(output = stderr) () = Stdlib.Printf.fprintf output "%s%!" !_header
    let synopsis ?(output = stderr) () = Stdlib.Printf.fprintf output "%s%!" !_synopsis
    let usage ?(output = stderr) () = Stdlib.Printf.fprintf output "%s%!" !_usage
    let markdown ?(output = stderr) () = Stdlib.Printf.fprintf output "%s%!" !_md
    let error ?(output = stderr) f_n msg =
      usage ~output ();
      Stdlib.Printf.fprintf output "Tools.Argv.%s: %s\n%!" f_n msg;
      exit 1
    let template_get n what f =
      (fun () ->
        try
          f ()
        with _ ->
          error n ("Option '" ^ argv.(!i - 1) ^ "' needs a " ^ what ^ " parameter"))
    let template_filter f g =
      (fun () ->
        let res = f () in
        if g res then
          res
        else
          raise Not_found)
    let get_parameter =
      template_get "get_parameter" "" (fun () -> incr i; argv.(!i))
    let get_parameter_boolean =
      template_get "get_parameter_boolean" "boolean" (fun () -> get_parameter () |> bool_of_string)
    let get_parameter_int =
      template_get "get_parameter_int" "integer" (fun () -> get_parameter () |> int_of_string)
    let get_parameter_float =
      template_get "get_parameter_float" "float" (fun () -> get_parameter () |> float_of_string)
    let get_parameter_int_pos =
      template_get "get_parameter_int_pos" "positive integer"
      (template_filter get_parameter_int (fun x -> x > 0))
    let get_parameter_float_pos =
      template_get "get_parameter_float_pos" "positive float"
      (template_filter get_parameter_float (fun x -> x > 0.))
    let get_parameter_int_non_neg =
      template_get "get_parameter_int_non_neg" "non-negative integer"
      (template_filter get_parameter_int (fun x -> x >= 0))
    let get_parameter_float_non_neg =
      template_get "get_parameter_float_non_neg" "non-negative float"
      (template_filter get_parameter_float (fun x -> x >= 0.))
    let get_parameter_int_percentage =
      template_get "get_parameter_int_percentage" "integer between 0 and 100"
      (template_filter get_parameter_int (fun x -> x >= 0 && x <= 100))
    let get_parameter_float_fraction =
      template_get "get_parameter_float_fraction" "float between 0 and 1"
      (template_filter get_parameter_float (fun x -> x >= 0. && x <= 1.))
    let get_remaining_parameters () =
      let len = Array.length argv in
      let res = Array.sub argv (!i + 1) (len - !i - 1) in
      i := len;
      res
    let parse specs =
      _usage := !_header ^ "Usage:\n  \027[1m" ^ argv.(0) ^ "\027[0m " ^ !_synopsis ^ "\n";
      _md := "```\n" ^ !_header ^ "```\nUsage:\n```\n" ^ argv.(0) ^ " " ^ !_synopsis ^ "\n```\n";
      let accum_usage = String.accum _usage
      and accum_md ?(escape = false) s =
        let res = ref "" in
        String.iter
          (function
            | '\\' | '`' | '*' | '_' | '{' | '}' | '[' | ']' | '(' | ')' | '#' | '+' | '-' | '.' | '!' | '~'
                as c when escape ->
              "\\" ^ String.make 1 c |> String.accum res
            | '<' when escape ->
              "&lt;" |> String.accum res
            | '>' when escape ->
              "&gt;" |> String.accum res
            | '|' when escape ->
              "&#124;" |> String.accum res
            | c ->
              String.make 1 c |> String.accum res)
          s;
        String.accum _md !res
      and need_table_header = ref false in
      let emit_table_header_if_needed () =
        if !need_table_header then begin
          need_table_header := false;
          "\n| Option | Argument(s) | Effect | Note(s) |\n|-|-|-|-|\n" |> accum_md
        end
      and trie = ref Trie.empty and table = ref StringMap.empty and mandatory = ref StringSet.empty in
      List.iteri
        (fun i (opts, vl, help, clss, act) ->
          if opts = [] && help = [] then
            error "parse" ("Malformed initializer for option #" ^ string_of_int i);
          if opts = [] then begin
            (* Case of a separator *)
            accum_md "\n";
            List.iteri
              (fun i line ->
                if line <> "" then begin
                  if i = 0 then
                    accum_md "**";
                  accum_md ~escape:true line;
                  if i = 0 then
                    accum_md "**";
                  accum_md "\n"
                end)
              help;
            (* Section headers require a new table *)
            need_table_header := true
          end else
            if help <> [] then begin
              emit_table_header_if_needed ();
              accum_md "| ";
              accum_usage "  "
            end;
          List.iteri
            (fun i opt ->
              if help <> [] then begin (* The option might be hidden *)
                if i > 0 then begin
                  accum_usage "\027[2m|\027[0m";
                  accum_md "<br>"
                end;
                "\027[1m" ^ opt ^ "\027[0m" |> accum_usage;
                (* No escaping needed here, as the text is already surrounded by quotes *)
                "`" ^ opt ^ "`" |> accum_md
              end;
              if Trie.find_string !trie opt <> "" then
                "Clashing command line option '" ^ opt ^ "' in table" |> error "parse";
              trie := Trie.add !trie opt;
              if clss = Mandatory then begin
                let repr = List.fold_left (fun a b -> a ^ (if a = "" then "" else "|") ^ b) "" opts in
                mandatory := StringSet.add repr !mandatory;
                table :=
                  StringMap.add opt
                    (fun arg ->
                      mandatory := StringSet.remove repr !mandatory;
                      act arg)
                    !table
              end else
                table := StringMap.add opt act !table)
            opts;
          if opts <> [] && help <> [] then begin
            accum_md " | ";
            begin match vl with
            | None -> ()
            | Some vl ->
              accum_md "_";
              accum_md ~escape:true vl;
              accum_md "_"
            end;
            accum_md " | "
          end;
          if help <> [] then begin
            begin match vl with
              | None -> ""
              | Some vl -> "\n    \027[3m" ^ vl ^ "\027[0m"
            end ^ begin
              if opts <> [] then
                "\n"
              else
                ""
            end |> accum_usage;
            let last_char = ref "" in
            List.iteri begin
              if opts <> [] then
                (fun i help ->
                  "   \027[38;5;7m|\027[0m " ^ help ^ "\n" |> accum_usage;
                  let l = String.length help in
                  let first_char =
                    if l > 0 then
                      String.sub help 0 1
                    else
                      "" in
                  accum_md begin
                    if i > 0 && !last_char = "." && String.uppercase_ascii first_char = first_char then
                      "<br>"
                    else
                      " "
                  end;
                  last_char := begin
                    if l > 0 then
                      String.sub help (l - 1) 1
                    else
                      ""
                  end;
                  accum_md ~escape:true help)
              else
                (* Case of a separator *)
                (fun i help ->
                  begin if i = 0 then
                    if help <> "" then
                      " \027[4m" ^ help ^ "\027[0m\n"
                    else
                      ""
                  else
                    " \027[4m| " ^ help ^ "\027[0m\n"
                  end |> accum_usage)
            end help;
            if opts <> [] && help <> [] then
              accum_md " | ";
            begin match clss with
            | Mandatory ->
              accum_usage "   \027[38;5;7m*\027[0m (\027[4mmandatory\027[0m)\n";
              accum_md "*(mandatory)*"
            | Optional -> ()
            | Default def ->
              "   \027[38;5;7m|\027[0m (default='\027[4m" ^ def () ^ "\027[0m')\n" |> accum_usage;
              accum_md "<ins>default=<mark>_";
              def () |> accum_md ~escape:true;
              accum_md "_</mark></ins>"
            end;
            if opts <> [] && help <> [] then
              accum_md " |\n"
          end)
        specs;
      (* The actual parsing :) *)
      let trie = !trie and table = !table and len = Array.length argv in
      while !i < len do
        let arg = argv.(!i) in
        begin try
          StringMap.find (Trie.find_string trie arg) table
        with Not_found ->
          error "parse" ("Unknown option '" ^ argv.(!i) ^ "'")
        end arg;
        incr i
      done;
      if !mandatory <> StringSet.empty then
        StringSet.iter
          (fun opt ->
            error "parse" ("Option '" ^ opt ^ "' is mandatory"))
          !mandatory
    let parse_error ?(output = stderr) = error ~output "parse"
    let make_separator s =
      [], None, [ s ], Optional, (fun _ -> ())
    let make_separator_multiline a =
      [], None, a, Optional, (fun _ -> ())
  end

