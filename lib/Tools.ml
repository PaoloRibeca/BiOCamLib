(*
    Tools.ml -- (c) 2015-2024 Paolo Ribeca, <paolo.ribeca@gmail.com>

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

module Int:
  module type of Int
= struct
    include Int
    let compare a b = a - b [@@inline] (* Optimisation (hopefully) *)
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

module Array:
  sig
    include module type of Array
    val of_rlist: 'a list -> 'a array
    val riter: ('a -> unit) -> 'a array -> unit
    val riteri: (int -> 'a -> unit) -> 'a array -> unit
    val resize: ?is_buffer:bool -> int -> 'a -> 'a array -> 'a array
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
    let resize ?(is_buffer = false) n null a =
      let l = Array.length a in
      if n > l then begin
        let res =
          Array.make begin
            if is_buffer then
              max n (l * 14 / 10)
            else
              n
          end null in
        Array.blit a 0 res 0 l;
        res
      end else if n < l && not is_buffer then
        (* We have to resize in order to honour the request *)
        Array.sub a 0 n
      else
        a
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
    type t = Int.t (* Our Int! *)
    let equal (i:int) (j:int) = (i - j = 0) [@@inline] (*(i = j)*)
    let hash (i:int) = i [@@inline] (* land max_int *)
  end
module IntHashtbl = Hashtbl.Make (IntHash)

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

(* Frequently used module idioms *)

module type TypeContainer_t = sig type t end
module type MakeComparable_t = functor (T: TypeContainer_t) -> ComparableType_t with type t = T.t
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

(* An ordered multimap is a map 'key -> 'val Set (no duplications allowed) *)
module Multimap (CmpKey: ComparableType_t) (CmpVal: ComparableType_t) =
  struct
    (* Keys have type CmpKey.t, values CmpVal.t *)
    module KeyMap = Map.Make (CmpKey)
    module ValSet = Set.Make (CmpVal)
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
        type set_t
        type t
        val empty: unit -> t
        val add_equivalences: t -> set_t -> unit
        val cardinal: t -> int
        val iter: (unit -> element_t -> unit) -> t -> unit
      end
    module Make (O: ComparableType_t):
      T_t with type element_t := O.t and type set_t := Set.Make(O).t
    = struct
        type element_t = O.t
        module ValueSet = Set.Make (O)
        type t = {
          (* We need this, which is also the ID of the next element,
              because vectors will be buffers and hence
              the size of id_to_element might not coincide with it *)
          mutable cardinal: int;
          (* Elements having a general type are hashed to integers.
             The hash contains:
              (1) The element ID (a progressive integer)
              (2) The index of the first relation the element appears in *)
          hash: (element_t, int * int) Hashtbl.t;
          (* The inverse of the previous one *)
          mutable id_to_element: element_t array;
          (* We need this because vectors will be buffers and hence
              the size of relations_to_classes might not coincide with it *)
          mutable relation_number: int;
          (* We keep track of relations as they are progressively added.
             The class of a relation is the lowest relation index of its elements *)
          mutable relation_to_class: int array;
          (* The classes *)
          mutable classes: IntSet.t IntMap.t
        }
        let empty () = {
          cardinal = 0;
          hash = Hashtbl.create 128;
          id_to_element = [||];
          relation_number = 0;
          relation_to_class = [||];
          classes = IntMap.empty
        }
        let add_equivalences tc set =
          if set <> ValueSet.empty then begin
            (* We'll need the new relation when inserting new elements,
                so we have to initialise it immediately *)            
            (*Printf.eprintf "Processing relation #%d...\n%!" tc.relation_number;*)
            (* We make sure there is enough space in relations_to_classes *)
            tc.relation_to_class <- Array.resize ~is_buffer:true (tc.relation_number + 1) (-1) tc.relation_to_class;
            (* For the time being, the new relations are in a class of their own *)
            tc.relation_to_class.(tc.relation_number) <- tc.relation_number;
            (* Here new_class are the new elements appearing in the relation,
                and new_relation are the classes to be merged *)
            let new_class = ref IntSet.empty and new_relation = ref IntSet.empty in
            ValueSet.iter
              (fun element ->
                if Hashtbl.mem tc.hash element |> not then begin
                  Hashtbl.add tc.hash element (tc.cardinal, tc.relation_number);
                  (* We make sure there is enough space in id_to_element *)
                  tc.id_to_element <- Array.resize ~is_buffer:true (tc.cardinal + 1) element tc.id_to_element;
                  tc.id_to_element.(tc.cardinal) <- element;
                  new_class := IntSet.add tc.cardinal !new_class;
                  tc.cardinal <- tc.cardinal + 1
                end;
                let _, relation = Hashtbl.find tc.hash element in
                if relation <> tc.relation_number then begin
                  (* Update dangling links *)
                  let dangling = ref IntSet.empty in
                  while IntMap.mem tc.relation_to_class.(relation) tc.classes |> not do
                    dangling := IntSet.add relation !dangling;
                    tc.relation_to_class.(relation) <- tc.relation_to_class.(tc.relation_to_class.(relation))
                  done;
                  let ok = tc.relation_to_class.(relation) in
                  IntSet.iter
                    (fun relation ->
                      tc.relation_to_class.(relation) <- ok)
                    !dangling
                end;
                new_relation := IntSet.add tc.relation_to_class.(relation) !new_relation)
              set;
            let new_class = !new_class and new_relation = !new_relation in
            (*Printf.eprintf " New class: %d -> {" tc.relation_number;
            IntSet.iter (Printf.eprintf " %d") new_class;
            Printf.eprintf " } (elements)\n%!";
            Printf.eprintf " New relation: %d -> {" tc.relation_number;
            IntSet.iter (Printf.eprintf " %d") new_relation;
            Printf.eprintf " } (classes)\n%!";*)
            if new_class <> IntSet.empty then
              tc.classes <- IntMap.add tc.relation_number new_class tc.classes;
            (* We can update the relations ID now - DON'T MOVE *)
            tc.relation_number <- tc.relation_number + 1;
            (* We merge and update classes *)
            let min_class = IntSet.min_elt new_relation in
            (*Printf.eprintf " Minimum class is %d\n%!" min_class;*)
            let union = ref IntSet.empty in
            IntSet.iter
              (fun class_ ->
                tc.relation_to_class.(class_) <- min_class;
                union := IntMap.find class_ tc.classes |> IntSet.union !union;
                tc.classes <- IntMap.remove class_ tc.classes)
              new_relation;
            tc.classes <- IntMap.add min_class !union tc.classes
          end
        let cardinal tc = tc.cardinal
        let iter f' tc =
          IntMap.iter
            (fun _ elements ->
              let f = f' () in
              IntSet.iter
                (fun id -> f tc.id_to_element.(id))
                elements)
            tc.classes
      end
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

module Argv:
  sig
    type header_info_t = {
      name: string;
      version: string;
      date: string
    }
    (* The specs from which the header is produced are a tuple with the following elements:
        (1) program name, version, date (as Info)
        (2) list of authors as (year range, name, email)
        (3) list of dependencies as (name, version, date) - each one as Info.
       We use a tuple rather than anything fancier to keep usages easier to write *)
    type header_specs_t = header_info_t * (string * string * string) list * header_info_t list
    type arg_t =
      | Mandatory (* Implies no default *)
      | Optional (* Implies no default *)
      | Default of (unit -> string) (* Implies optional - the function prints the default *)
    (* Each spec from which the usage is produced is a tuple with the following elements:
        (1) Equivalent option names
        (2) Optional explanation for the argument(s)
        (3) Help text lines.
            An empty help means the option is private and no usage will be output for it
        (4) Class (can be mandatory, optional, default)
        (5) Parsing action when option is encountered.
       We use a tuple rather than anything fancier to keep usages easier to write *)
    type argv_spec_t = string list * string option * string list * arg_t * (string -> unit)
    (* Compiles and sets the structured header *)
    val set_header: header_specs_t -> unit
    (* Sets the text following the program name *)
    val set_synopsis: string -> unit
    (* Functions to get arguments from within the actions given as argument to parse() *)
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
    (* Makes a textual separator between groups of options *)
    val make_separator: string -> argv_spec_t
    val make_separator_multiline: string list -> argv_spec_t
    (* Parses command line options and generates usage and markdown.
       Must be called _after_ set_header() and set_synopsis() *)
    val parse: argv_spec_t list -> unit
    (* Can be invoked from within the actions given as argument to parse() *)
    val parse_error: ?output:out_channel -> string -> unit
    (* Functions to print results of parsing *)
    val header: ?output:out_channel -> unit -> unit
    val synopsis: ?output:out_channel -> unit -> unit
    val usage: ?output:out_channel -> unit -> unit
    val markdown: ?output:out_channel -> unit -> unit
  end
= struct
    type header_info_t = {
      name: string;
      version: string;
      date: string
    }
    type header_specs_t = header_info_t * (string * string * string) list * header_info_t list
    type arg_t =
      | Mandatory
      | Optional
      | Default of (unit -> string)
    type argv_spec_t = string list * string option * string list * arg_t * (string -> unit)
    (* The header and its markdown version are different *)
    let _header = ref ""
    let _md_header = ref ""
    let set_header ({ name; version; date }, author_info, depends_on) =
      let open String.TermIO in
      _header := begin
        let line = ref "" and spaces = ref "" in
        for _ = 1 to String.length (name ^ version ^ date) + 23 do
          line := !line ^ "━";
          spaces := !spaces ^ " "
        done;
        Printf.sprintf "%s\n%s\n%s  %s %s %s %s [%s]  %s\n%s\n%s\n"
          ("╔━" ^ !line ^ "╗" |> grey)
          ("┃ " ^ !spaces ^ "┃" |> grey)
          (grey "┃") (bold "This is") (green name) (bold "version") (green version) (blue date)  (grey "┃")
          ("┃ " ^ !spaces ^ "┃" |> grey)
          ("╚┯" ^ !line ^ "╝" |> grey)
      end;
      _md_header := Printf.sprintf "This is %s version %s [%s]\n" name version date;
      let red_l = List.length depends_on - 1 in
      List.iteri
        (fun i { name; version; date } ->
          _header := !_header ^ begin
            Printf.sprintf " %s%s version %s [%s]%s\n"
              (if i = 0 then grey "│" ^ " compiled against: " else "                    ")
              (green name) (green version) (blue date)
              (if i = red_l then "" else ";")
          end;
          _md_header := !_md_header  ^ begin
            Printf.sprintf " %s%s version %s [%s]%s\n"
              (if i = 0 then "compiled against: " else "                  ")
              name version date
              (if i = red_l then "" else ";")
          end)
        depends_on;
      List.iteri
        (fun i (years, name, email) ->
          _header := !_header ^ begin
            Printf.sprintf " %s %s %s <%s>\n"
              (if i = 0 then grey "│" ^ " (c)" else "     ") years (bold name) (under email)
          end;
          _md_header := !_md_header ^ begin
            Printf.sprintf " %s %s %s <%s>\n" (if i = 0 then "(c)" else "   ") years name email
          end)
        author_info
    let _synopsis = ref ""
    let _md_synopsis = ref ""
    let set_synopsis s =
      (* At the moment, the synopsis and its markdown version are the same *)
      _synopsis := s;
      _md_synopsis := s
    let argv = Sys.argv
    let i = ref 1
    (* Both _usage and _md will be completed by parse () *)
    let _usage = ref ""
    let _md_usage = ref ""
    let header ?(output = stderr) () = Stdlib.Printf.fprintf output "%s%!" !_header
    let synopsis ?(output = stderr) () = Stdlib.Printf.fprintf output "%s%!" !_synopsis
    let usage ?(output = stderr) () = Stdlib.Printf.fprintf output "%s%!" !_usage
    let markdown ?(output = stderr) () = Stdlib.Printf.fprintf output "%s%!" !_md_usage
    let error ?(output = stderr) f_n msg =
      let open String.TermIO in
      usage ~output ();
      Stdlib.Printf.fprintf output "(%s): %s\n%!" f_n (red msg);
      exit 1
    let template_get n what f =
      (fun () ->
        try
          f ()
        with _ ->
          error n ("Option '" ^ argv.(!i - 1) ^ "' needs " ^ what ^ " parameter"))
    let template_filter f g =
      (fun () ->
        let res = f () in
        if g res then
          res
        else
          raise Not_found)
    let get_parameter =
      template_get __FUNCTION__ "a" (fun () -> incr i; argv.(!i))
    let get_parameter_boolean =
      template_get __FUNCTION__ "a boolean" (fun () -> get_parameter () |> bool_of_string)
    let get_parameter_int =
      template_get __FUNCTION__ "an integer" (fun () -> get_parameter () |> int_of_string)
    let get_parameter_float =
      template_get __FUNCTION__ "a float" (fun () -> get_parameter () |> float_of_string)
    let get_parameter_int_pos =
      template_get __FUNCTION__ "a positive integer"
      (template_filter get_parameter_int (fun x -> x > 0))
    let get_parameter_float_pos =
      template_get __FUNCTION__ "a positive float"
      (template_filter get_parameter_float (fun x -> x > 0.))
    let get_parameter_int_non_neg =
      template_get __FUNCTION__ "a non-negative integer"
      (template_filter get_parameter_int (fun x -> x >= 0))
    let get_parameter_float_non_neg =
      template_get __FUNCTION__ "a non-negative float"
      (template_filter get_parameter_float (fun x -> x >= 0.))
    let get_parameter_int_percentage =
      template_get __FUNCTION__ "an integer between 0 and 100 as"
      (template_filter get_parameter_int (fun x -> x >= 0 && x <= 100))
    let get_parameter_float_fraction =
      template_get __FUNCTION__ "a float between 0 and 1 as"
      (template_filter get_parameter_float (fun x -> x >= 0. && x <= 1.))
    let get_remaining_parameters () =
      let len = Array.length argv in
      let res = Array.sub argv (!i + 1) (len - !i - 1) in
      i := len;
      res
    let make_separator s =
      [], None, [ s ], Optional, (fun _ -> ())
    let make_separator_multiline a =
      [], None, a, Optional, (fun _ -> ())
    let parse specs =
      let open String.TermIO in
      let basename = Filename.basename argv.(0) in
      _usage := !_header ^ red " Usage:" ^ "\n  " ^ bold basename ^ " " ^ blue !_synopsis ^ "\n";
      _md_usage := "```\n" ^ !_md_header ^ "```\n*Usage:*\n```\n" ^ basename ^ " " ^ !_md_synopsis ^ "\n```\n";
      let accum_usage = String.accum _usage
      and accum_md_usage ?(escape = false) s =
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
        String.accum _md_usage !res
      and need_table_header = ref false in
      let emit_table_header_if_needed () =
        if !need_table_header then begin
          need_table_header := false;
          "\n| Option | Argument(s) | Effect | Note(s) |\n|-|-|-|-|\n" |> accum_md_usage
        end
      and trie = ref Trie.empty and table = ref StringMap.empty and mandatory = ref StringSet.empty in
      List.iteri
        (fun i (opts, vl, help, class_, act) ->
          if opts = [] && help = [] then
            error __FUNCTION__ ("Malformed initializer for option #" ^ string_of_int i);
          if opts = [] then begin
            (* Case of a separator *)
            accum_md_usage "\n";
            List.iteri
              (fun i line ->
                if line <> "" then begin
                  if i = 0 then
                    accum_md_usage "**";
                  accum_md_usage ~escape:true line;
                  if i = 0 then
                    accum_md_usage "**";
                  accum_md_usage "\n"
                end)
              help;
            (* Section headers require a new table *)
            need_table_header := true
          end else
            if help <> [] then begin
              emit_table_header_if_needed ();
              accum_md_usage "| ";
              accum_usage "  "
            end;
          List.iteri
            (fun i opt ->
              if help <> [] then begin (* The option might be hidden *)
                if i > 0 then begin
                  grey "|" |> accum_usage;
                  accum_md_usage "<br>"
                end;
                blue opt |> accum_usage;
                (* No escaping needed here, as the text is already surrounded by quotes *)
                "`" ^ opt ^ "`" |> accum_md_usage
              end;
              if Trie.find_string !trie opt <> "" then
                "Clashing command line option '" ^ opt ^ "' in table" |> error __FUNCTION__;
              trie := Trie.add !trie opt;
              if class_ = Mandatory then begin
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
            accum_md_usage " | ";
            begin match vl with
            | None -> ()
            | Some vl ->
              accum_md_usage "_";
              accum_md_usage ~escape:true vl;
              accum_md_usage "_"
            end;
            accum_md_usage " | "
          end;
          if help <> [] then begin
            begin match vl with
              | None -> ""
              | Some vl -> "\n    " ^ bold vl
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
                  "   " ^ grey "│" ^ " " ^ help ^ "\n" |> accum_usage;
                  let l = String.length help in
                  let first_char =
                    if l > 0 then
                      String.sub help 0 1
                    else
                      "" in
                  accum_md_usage begin
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
                  accum_md_usage ~escape:true help)
              else
                (* Case of a separator *)
                (fun i help ->
                  begin if i = 0 then
                    if help <> "" then
                      " " ^ green help ^ "\n"
                    else
                      ""
                  else
                    " " ^ grey "│" ^ " " ^ green help ^ "\n"
                  end |> accum_usage)
            end help;
            if opts <> [] && help <> [] then
              accum_md_usage " | ";
            begin match class_ with
            | Mandatory ->
              "   " ^ grey "*" ^ " (" ^ red "mandatory" ^ ")\n" |> accum_usage;
              accum_md_usage "*(mandatory)*"
            | Optional -> ()
            | Default def ->
              "   " ^ grey "│" ^ " (default='" ^ (def () |> bold |> under) ^ "')\n" |> accum_usage;
              accum_md_usage "<ins>default=<mark>_";
              def () |> accum_md_usage ~escape:true;
              accum_md_usage "_</mark></ins>"
            end;
            if opts <> [] && help <> [] then
              accum_md_usage " |\n"
          end)
        specs;
      (* And finally, the actual parsing :) *)
      let trie = !trie and table = !table and len = Array.length argv in
      while !i < len do
        let arg = argv.(!i) in
        begin try
          StringMap.find (Trie.find_string trie arg) table
        with Not_found ->
          error __FUNCTION__ ("Unknown option '" ^ argv.(!i) ^ "'")
        end arg;
        incr i
      done;
      if !mandatory <> StringSet.empty then
        StringSet.iter
          (fun opt ->
            error __FUNCTION__ ("Option '" ^ opt ^ "' is mandatory"))
          !mandatory
    let parse_error ?(output = stderr) = error ~output __FUNCTION__
  end

