(* This library is
    (C) 2015-2022 Paolo Ribeca, <paolo.ribeca@gmail.com>.
   It contains several general-purpose utilities, in particular:
    * a module to parse command-line options
    * a module to arbitrarily parallelize streams following a
       reader-workers-writer model.
   Please do not redistribute, or contact the author before doing so *)

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
    val accum: string ref -> string -> unit
    val pluralize: ?plural:string -> one:'a -> string -> 'a -> string
    val pluralize_int : ?plural:string -> string -> int -> string
    val pluralize_float : ?plural:string -> string -> float -> string
  end
= struct
    include String
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
      let l = Array.length a in
      for i = l - 1 downto 0 do
        f a.(i)
      done
    let riteri f a =
      let l = Array.length a in
      for i = l - 1 downto 0 do
        f i a.(i)
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
module type TypeContainer = sig type t end
module MakeComparable (T: TypeContainer) =
  struct
    type t = T.t
    (* Informs the compiler that polymorphism is not needed *)
    let compare (a:t) (b:t) = compare a b
  end
module MakeRComparable (T: TypeContainer) =
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
module ComparableInt = MakeComparable (struct type t = int end)
module IntSet: module type of Set.Make (ComparableInt) = Set.Make (ComparableInt)
module IntMap: module type of Map.Make (ComparableInt) = Map.Make (ComparableInt)
module RComparableInt = MakeRComparable (struct type t = int end)
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

module Trie:
  sig
    type t
    val empty: t
    val add: t -> string -> t
    type result_t =
      | Not_found
      | Partial of string
      | Ambiguous of string list
      | Contained of string list
      | Unique of string
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
module type Sequable =
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
module Seq:Sequable with type 'a init_t := 'a Seq.t and type 'a ret_t := 'a =
  struct
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

module Split =
  struct
    let fasta_name_re = Str.regexp "^>"
    let as_list = Str.split
    let as_array re s = Str.split re s |> Array.of_list
    (* All splits that were previously done with one-char regexps can now be done with the following two *)
    let on_char_as_list = String.split_on_char
    let on_char_as_array c s = String.split_on_char c s |> Array.of_list
  end

(* A general module type to unify numbers *)
(* module type M = sig include module type of struct include Int end end *)
module type Number =
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

(* Encapsulated vectors based on bigarrays *)
module BA:
  sig
    module type Type =
      sig
        include Number
        type elt_t
        val elt: (t, elt_t) Bigarray.kind
      end
    module type VectorType =
      sig
        module BA1 = Bigarray.Array1
        module N: Type
        type t
        val init: int -> N.t -> t
        val empty: t
        val length: t -> int
        val get: t -> int -> N.t
        val ( .=() ): t -> int -> N.t
        val set: t -> int -> N.t -> unit
        val ( .=()<- ): t -> int -> N.t -> unit
        val incr: t -> int -> unit
        val ( .+() ): t -> int -> unit
        val incr_by: t -> int -> N.t -> unit
        val ( .+()<- ): t -> int -> N.t -> unit
        val sub: t -> int -> int -> t
        val blit: t -> t -> unit
        val fill: t -> N.t -> unit
      end
    module Vector: functor (T: Type) -> VectorType with type N.t = T.t
  end
= struct
    module type Type =
      sig
        include Number
        type elt_t
        val elt: (t, elt_t) Bigarray.kind
      end
    module type VectorType =
      sig
        module BA1 = Bigarray.Array1
        module N: Type
        type t
        val init: int -> N.t -> t
        val empty: t
        val length: t -> int
        val get: t -> int -> N.t
        val ( .=() ): t -> int -> N.t
        val set: t -> int -> N.t -> unit
        val ( .=()<- ): t -> int -> N.t -> unit
        val incr: t -> int -> unit
        val ( .+() ): t -> int -> unit
        val incr_by: t -> int -> N.t -> unit
        val ( .+()<- ): t -> int -> N.t -> unit
        val sub: t -> int -> int -> t
        val blit: t -> t -> unit
        val fill: t -> N.t -> unit
      end
    module Vector (T: Type) = (* This should work with float too *)
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
        let ( .=() ) = BA1.get
        let set = BA1.set
        let ( .=()<- ) = BA1.set
        let incr ba n =
          BA1.get ba n |> N.add N.one |> BA1.set ba n
        let ( .+() ) = incr
        let incr_by ba n v =
          BA1.get ba n |> N.add v |> BA1.set ba n
        let ( .+()<- ) = incr_by
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
    val get_parameter_float_fraction: unit -> float
    (* Consumes and returns all the parameters which are left on the command line *)
    val get_remaining_parameters: unit -> string array
    val parse: spec_t list -> unit
    (* Can be invoked from within the actions given as argument to parse() *)
    val parse_error: ?output:out_channel -> string -> unit
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
    let get_parameter_float_fraction =
      template_get "get_parameter_float_fraction" "float between 0 and 1"
      (template_filter get_parameter_float (fun x -> x >= 0. && x <= 1.))
    let get_remaining_parameters () =
      let len = Array.length argv in
      let res = Array.sub argv (!i + 1) (len - !i - 1) in
      i := len;
      res
    let parse specs =
      _usage := !_header ^ "Usage:\n  " ^ argv.(0) ^ " " ^ !_synopsis ^ "\n";
      _md := "```\n" ^ !_header ^ "```\nUsage:\n```\n" ^ argv.(0) ^ " " ^ !_synopsis ^ "\n```\n";
      let accum_md ?(escape = false) s =
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
          "| Option | Argument(s) | Effect | Note(s) |\n|-|-|-|-|\n" |> accum_md
        end
      and table = ref StringMap.empty and mandatory = ref StringSet.empty in
      List.iteri
        (fun i (opts, vl, help, clss, act) ->
          if opts = [] && help = [] then
            error "parse" ("Malformed initializer for option #" ^ string_of_int i);
          if opts = [] then begin
            accum_md "\n";
            List.iter
              (fun line ->
                accum_md ~escape:true line;
                accum_md "\n")
              help;
            (* Section headers require a new table *)
            need_table_header := true
          end else
            if help <> [] then begin
              emit_table_header_if_needed ();
              accum_md "| "
            end;
          if help <> [] then
            begin if opts <> [] then
              "  "
            else
              " "
            end |> String.accum _usage;
          List.iteri
            (fun i opt ->
              if help <> [] then begin (* The option might be hidden *)
                if i > 0 then begin
                  String.accum _usage "|";
                  accum_md "<br>"
                end;
                String.accum _usage opt;
                (* No escaping needed here, as the text is already surrounded by quotes *)
                "`" ^ opt ^ "`" |> accum_md
              end;
              if StringMap.mem opt !table then
                "Duplicate command line option '" ^ opt ^ "' in table" |> error "parse";
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
              | Some vl ->  "\n    " ^ vl
            end ^ begin
              if opts <> [] then
                "\n"
              else
                ""
            end |> String.accum _usage;
            let last_char = ref "" in
            List.iteri begin
              if opts <> [] then
                (fun i help ->
                  "   | " ^ help ^ "\n" |> String.accum _usage;
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
                (fun _ help -> help ^ "\n" |> String.accum _usage)
            end help;
            if opts <> [] && help <> [] then
              accum_md " | ";
            begin match clss with
            | Mandatory ->
              String.accum _usage "   * (mandatory)\n";
              accum_md "*(mandatory)*"
            | Optional -> ()
            | Default def ->
              "   | (default='" ^ def () ^ "')\n" |> String.accum _usage;
              accum_md "<ins>default=<mark>_";
              def () |> accum_md ~escape:true;
              accum_md "_</mark></ins>"
            end;
            if opts <> [] && help <> [] then
              accum_md " |\n"
          end)
        specs;
      let len = Array.length argv in
      while !i < len do
        let arg = argv.(!i) in
        begin try
          StringMap.find arg !table
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
  end

(* Simple wrapper around Unix sub-processes *)
module Subprocess:
  sig
    exception Some_problem_occurred of string * string
    (* Execute a simple command - the executables do not need to be a fully qualified path *)
    val spawn: ?verbose:bool -> string -> unit
    val spawn_and_read_single_line: ?verbose:bool -> string -> string
    (* The following two functions spawn a subprocess, with its output being read by the parent.
       The second function argument processes the output line-wise.
       THE EXECUTABLES MUST BE FULLY QUALIFIED PATHS *)
    val spawn_and_process_output: ?verbose:bool ->
      (unit -> unit) -> (int -> string -> unit) -> (unit -> unit) -> string -> unit
    val spawn_with_args_and_process_output: ?verbose:bool ->
      (unit -> unit) -> (int -> string -> unit) -> (unit -> unit) -> string -> string array -> unit
  end
= struct
    exception Some_problem_occurred of string * string
    (* PRIVATE *)
    let handle_termination_status command stderr_contents = function
      | Unix.WEXITED 0 -> ()
      | e ->
        Stdlib.Printf.eprintf "%s%!" stderr_contents;
        match e with
        | WEXITED n ->
          Some_problem_occurred (command, Stdlib.Printf.sprintf "Process exit status was %d" n) |> raise
        | WSIGNALED n ->
          Some_problem_occurred (command, Stdlib.Printf.sprintf "Process killed by signal %d" n) |> raise
        | WSTOPPED n ->
          Some_problem_occurred (command, Stdlib.Printf.sprintf "Process stopped by signal %d" n) |> raise
    (* PUBLIC *)
    let spawn ?(verbose = false) command =
      if verbose then
        Stdlib.Printf.eprintf "Subprocess.spawn: Executing command '%s'...\n%!" command;
      Unix.system command |> handle_termination_status command ""
    let spawn_and_read_single_line ?(verbose = false) command =
      if verbose then
        Stdlib.Printf.eprintf "Subprocess.spawn_and_read_single_line: Executing command '%s'...\n%!" command;
      let process_out = Unix.open_process_in command in
      let res =
        try
          input_line process_out
        with End_of_file -> "" in
      Unix.close_process_in process_out |> handle_termination_status command "";
      res
    let spawn_with_args_and_process_output ?(verbose = false) pre f post command args =
      if verbose then begin
        let command = Array.fold_left (fun accum arg -> accum ^ " " ^ arg) command args in
        Stdlib.Printf.eprintf "Subprocess.spawn_with_args_and_process_output: Executing command '%s'...\n%!" command
      end;
      let process_out, process_in, process_err =
        Unix.unsafe_environment () |> Unix.open_process_args_full command args in
      close_out process_in;
      if verbose then
        Stdlib.Printf.eprintf "Subprocess.spawn_with_args_and_process_output: Executing initialization function...\n%!";
      pre ();
      if verbose then
        Stdlib.Printf.eprintf "Subprocess.spawn_with_args_and_process_output: Processing contents of standard output...\n%!";
      begin try
        let line_cntr = ref 0 in
        while true do
          let line = input_line process_out in
          incr line_cntr;
          f !line_cntr line
        done
      with End_of_file -> ()
      end;
      if verbose then
        Stdlib.Printf.eprintf "Subprocess.spawn_with_args_and_process_output: Executing finalization function...\n%!";
      post ();
      (* There might be content on the stderr - we collect it here, and output it in case of error *)
      if verbose then
        Stdlib.Printf.eprintf "Subprocess.spawn_with_args_and_process_output: Collecting contents of standard error...\n%!";
      let stderr_contents = Buffer.create 1024 in
      begin try
        while true do
          input_line process_err |> Buffer.add_string stderr_contents;
          Buffer.add_char stderr_contents '\n'
        done
      with End_of_file -> ()
      end;
      close_in process_out;
      close_in process_err;
      Unix.close_process_full (process_out, process_in, process_err) |>
        handle_termination_status command (Buffer.contents stderr_contents)
    let spawn_and_process_output ?(verbose = false) pre f post command =
      spawn_with_args_and_process_output ~verbose pre f post command [||]
  end

(* Abstraction for filenames to be used in scripts *)
module QuotedFilename:
  sig
    type t = {
      unquoted : string;
      quoted : string
    }
    val none: t
    val to_string: t -> string
    val of_string: string -> t
    exception Could_not_get_quoted_name of string
    exception Executable_not_found_in_path of string
    val get_absolute : string -> t
    val get_executable : string -> t
    val concat_to : t -> string -> t
    val append_to : t -> string -> t
    val get_in_directory : t -> string -> string -> t
  end
= struct
    (* PUBLIC *)      
    type t = {
      unquoted: string;
      quoted: string
    }
    let none = { unquoted = ""; quoted = "" }
    (* Marshalling. We use double escaping of tabs *)
    let to_string { unquoted; quoted } = String.escaped unquoted ^ "\t" ^ String.escaped quoted |> String.escaped
    let of_string s =
      let s = Scanf.unescaped s |> Split.on_char_as_array '\t' in
      assert (Array.length s = 2);
      { unquoted = Scanf.unescaped s.(0); quoted = Scanf.unescaped s.(1) }
    exception Could_not_get_quoted_name of string
    exception Executable_not_found_in_path of string
    (* PRIVATE *)
    let get_from_shell exc_f s =
      try
        Subprocess.spawn_and_read_single_line s
      with _ ->
        exc_f s |> raise
    let get_quoted s =
      get_from_shell (fun s -> Could_not_get_quoted_name s) (Stdlib.Printf.sprintf "bash -c 'printf \"%%q\" \"%s\"'" s)
    let get_in_path s =
      get_from_shell (fun s -> Executable_not_found_in_path s) (Stdlib.Printf.sprintf "bash -c 'command -v \"%s\"'" s)
    (* PUBLIC *)
    let get_absolute s =
      let unquoted =
        if Filename.is_relative s then
          Filename.concat (Sys.getcwd ()) s
        else
          s in
      { unquoted; quoted = get_quoted unquoted }
    let get_executable s =
      let unquoted = get_in_path s in
      { unquoted; quoted = get_quoted unquoted }
    let concat_to fn name =
      { unquoted = Filename.concat fn.unquoted name; quoted = Filename.concat fn.quoted (get_quoted name) }
    let append_to fn suffix =
      { unquoted = fn.unquoted ^ suffix; quoted = fn.quoted ^ (get_quoted suffix) }
    let get_in_directory directory basename extension = append_to (concat_to directory basename) extension
  end

module Parallel:
  sig
    val get_nproc: unit -> int
    exception Number_of_chunks_must_be_positive of int
    exception Number_of_threads_must_be_positive of int
    val process_stream_chunkwise: ?buffered_chunks_per_thread:int ->
      (* Beware: for everything to terminate properly, f shall raise End_of_file when done.
         Side effects are propagated within f (not exported) and within h (exported) *)
      (unit -> 'a) -> ('a -> 'b) -> ('b -> unit) -> int -> unit
    val process_stream_linewise: ?buffered_chunks_per_thread:int -> ?max_memory:int -> ?string_buffer_memory:int ->
                                 ?input_line:(in_channel -> string) -> ?verbose:bool ->
      in_channel -> (Buffer.t -> int -> string -> unit) -> out_channel -> int -> unit
  end
= struct
    let get_nproc () =
      try
        Subprocess.spawn_and_read_single_line "nproc" |> int_of_string
      with _ ->
        1
    exception Number_of_chunks_must_be_positive of int
    exception Number_of_threads_must_be_positive of int
    let process_stream_chunkwise ?(buffered_chunks_per_thread = 10)
        (f:unit -> 'a) (g:'a -> 'b) (h:'b -> unit) threads =
      if buffered_chunks_per_thread < 1 then
        Number_of_chunks_must_be_positive buffered_chunks_per_thread |> raise;
      if threads < 1 then
        Number_of_threads_must_be_positive threads |> raise;
      let red_threads = threads - 1 in
      (* I am the ouptut process *)
      let close_pipe (pipe_in, pipe_out) = Unix.close pipe_in; Unix.close pipe_out in
      let close_pipes_in = Array.iter (fun (pipe_in, _) -> Unix.close pipe_in)
      and close_pipes_out = Array.iter (fun (_, pipe_out) -> Unix.close pipe_out)
      and close_pipes = Array.iter close_pipe
      and get_stuff_for_select pipes =
        let pipes = Array.map fst pipes in
        Array.to_list pipes, begin
          let dict = Hashtbl.create (Array.length pipes) in
          Array.iteri
            (fun i pipe ->
               assert (not (Hashtbl.mem dict pipe));
               Hashtbl.add dict pipe i)
            pipes;
          dict
        end
      and w_2_o_pipes = Array.init threads (fun _ -> Unix.pipe ())
      and o_2_w_pipes = Array.init threads (fun _ -> Unix.pipe ()) in
      match Unix.fork () with
      | 0 -> (* Child *)
        (* I am the input process *)
        let i_2_w_pipes = Array.init threads (fun _ -> Unix.pipe ())
        and w_2_i_pipes = Array.init threads (fun _ -> Unix.pipe ()) in
        for i = 0 to red_threads do
          match Unix.fork () with
          | 0 -> (* Child *)
            (* I am a worker.
               I only keep my own pipes open *)
            let i_2_w_pipe_in, w_2_i_pipe_out, o_2_w_pipe_in, w_2_o_pipe_out =
              let i_2_w_pipe_in = ref Unix.stdin and w_2_i_pipe_out = ref Unix.stdout
              and o_2_w_pipe_in = ref Unix.stdin and w_2_o_pipe_out = ref Unix.stdout in
              for ii = 0 to red_threads do
                if ii = i then begin
                  let pipe_in, pipe_out = i_2_w_pipes.(ii) in
                  i_2_w_pipe_in := pipe_in;
                  Unix.close pipe_out;
                  let pipe_in, pipe_out = w_2_i_pipes.(ii) in
                  Unix.close pipe_in;
                  w_2_i_pipe_out := pipe_out;
                  let pipe_in, pipe_out = o_2_w_pipes.(ii) in
                  o_2_w_pipe_in := pipe_in;
                  Unix.close pipe_out;
                  let pipe_in, pipe_out = w_2_o_pipes.(ii) in
                  Unix.close pipe_in;
                  w_2_o_pipe_out := pipe_out
                end else begin
                  close_pipe i_2_w_pipes.(ii);
                  close_pipe w_2_i_pipes.(ii);
                  close_pipe o_2_w_pipes.(ii);
                  close_pipe w_2_o_pipes.(ii)
                end
              done;
              !i_2_w_pipe_in, !w_2_i_pipe_out, !o_2_w_pipe_in, !w_2_o_pipe_out in
            let i_2_w = Unix.in_channel_of_descr i_2_w_pipe_in
            and w_2_i = Unix.out_channel_of_descr w_2_i_pipe_out
            and o_2_w = Unix.in_channel_of_descr o_2_w_pipe_in
            and w_2_o = Unix.out_channel_of_descr w_2_o_pipe_out in
            (* My protocol is:
               (1) process a chunk more from the input
               (2) notify the output process that a result is ready
               (3) when the output process asks for it, post the result *)
            let probe_output () =
              (*ignore (Unix.select [o_2_w_pipe_in] [] [] (-1.));*)
              ignore (input_byte o_2_w)
            and initial = ref true in
            while true do
              (* Try to get one more chunk.
                 Signal the input process that I am idle *)
              output_byte w_2_i 0;
              flush w_2_i;
              (* Get & process a chunk *)
              match input_byte i_2_w with
              | 0 -> (* EOF reached *)
                (* Did the output process ask for a notification? *)
                if not !initial then (* The first time, we notify anyway to avoid crashes *)
                  probe_output ();
                (* Notify that EOF has been reached *)
                output_binary_int w_2_o (-1);
                flush w_2_o;
                (* Did the output process switch me off? *)
                probe_output ();
                (* Commit suicide *)
                Unix.close i_2_w_pipe_in;
                Unix.close w_2_i_pipe_out;
                Unix.close o_2_w_pipe_in;
                Unix.close w_2_o_pipe_out;
                exit 0
              | 1 -> (* OK, one more token available *)
                (* Get the chunk *)
                let chunk_id, data = (input_value i_2_w:int * 'a) in
                (* Process the chunk *)
                let data = g data in
                (* Did the output process ask for a notification? *)
                if not !initial then (* The first time, we notify anyway to avoid crashes *)
                  probe_output ()
                else
                  initial := false;
                (* Tell the output process what we have *)
                output_binary_int w_2_o chunk_id;
                flush w_2_o;
                (* Did the output process request data? *)
                probe_output ();
                (* Send the data to output *)
                output_value w_2_o data;
                flush w_2_o
              | _ -> assert false
            done
          | _ -> () (* Parent *)
        done;
        (* I am the input process.
           I do not care about output process pipes *)
        close_pipes w_2_o_pipes;
        close_pipes o_2_w_pipes;
        close_pipes_in i_2_w_pipes;
        close_pipes_out w_2_i_pipes;
        let w_2_i_pipes_for_select, w_2_i_dict = get_stuff_for_select w_2_i_pipes
        and w_2_i = Array.map (fun (pipe_in, _) -> Unix.in_channel_of_descr pipe_in) w_2_i_pipes
        and i_2_w = Array.map (fun (_, pipe_out) -> Unix.out_channel_of_descr pipe_out) i_2_w_pipes in
        (* My protocol is:
           (1) read a chunk
           (2) read a thread id
           (3) post the chunk to the correspondng pipe. The worker will consume it *)
        let chunk_id = ref 0 and off = ref 0 in
        while !off < threads do
          let ready, _, _ = Unix.select w_2_i_pipes_for_select [] [] (-1.) in
          List.iter
            (fun ready ->
              let w_id = Hashtbl.find w_2_i_dict ready in
              ignore (input_byte w_2_i.(w_id));
              let i_2_w = i_2_w.(w_id) in
              try
                if !off > 0 then
                  raise End_of_file;
                let payload = f () in
                output_byte i_2_w 1; (* OK to transmit, we have not reached EOF yet *)
                flush i_2_w;
                output_value i_2_w (!chunk_id, payload);
                flush i_2_w;
                incr chunk_id
              with End_of_file ->
                output_byte i_2_w 0; (* Nothing to transmit *)
                flush i_2_w;
                incr off)
            ready
        done;
        (* Waiting to be switched off *)
        ignore (Unix.select [fst w_2_i_pipes.(0)] [] [] (-1.));
        close_pipes_out i_2_w_pipes;
        close_pipes_in w_2_i_pipes;
        exit 0
      | _ -> (* I am the output process *)
        close_pipes_in o_2_w_pipes;
        close_pipes_out w_2_o_pipes;
        let w_2_o_pipes_for_select, w_2_o_dict = get_stuff_for_select w_2_o_pipes
        and w_2_o = Array.map (fun (pipe_in, _) -> Unix.in_channel_of_descr pipe_in) w_2_o_pipes
        and o_2_w = Array.map (fun (_, pipe_out) -> Unix.out_channel_of_descr pipe_out) o_2_w_pipes
        and buffered_chunks = buffered_chunks_per_thread * threads
        and next = ref 0 and queue = ref IntMap.empty and buf = ref IntMap.empty
        and off = ref 0 in
        while !off < threads do
          (* Harvest new notifications *)
          let ready, _, _ = Unix.select w_2_o_pipes_for_select [] [] (-1.) in
          List.iter
            (fun ready ->
              let w_id = Hashtbl.find w_2_o_dict ready in
              let chunk_id = input_binary_int w_2_o.(w_id) in
              if chunk_id = -1 then (* EOF has been reached *)
                incr off
              else
                if not (IntMap.mem chunk_id !queue) then
                  queue := IntMap.add chunk_id w_id !queue
                else
                  assert (w_id = IntMap.find chunk_id !queue))
            ready;
          (* Fill the buffer *)
          let available = ref (buffered_chunks - IntMap.cardinal !buf) in
          assert (!available >= 0);
          (* If the needed chunk is there, we always fetch it *)
          if !queue <> IntMap.empty && fst (IntMap.min_binding !queue) = !next then
            incr available;
          while !available > 0 && !queue <> IntMap.empty do
            let chunk_id, w_id = IntMap.min_binding !queue in
            (* Tell the worker to send data *)
            output_byte o_2_w.(w_id) 0;
            flush o_2_w.(w_id);
            assert (not (IntMap.mem chunk_id !buf));
            buf := IntMap.add chunk_id (input_value w_2_o.(w_id):'b) !buf;
            (* Tell the worker to send the next notification *)
            output_byte o_2_w.(w_id) 0;
            flush o_2_w.(w_id);
            queue := IntMap.remove chunk_id !queue;
            decr available
          done;
          (* Output at most as many chunks at the number of workers *)
          available := threads;
          while !available > 0 && !buf <> IntMap.empty do
            let chunk_id, data = IntMap.min_binding !buf in
            if chunk_id = !next then begin
              h data;
              buf := IntMap.remove chunk_id !buf;
              incr next;
              decr available
            end else
              available := 0 (* Force exit from the cycle *)
          done
        done;
        (* There might be chunks left in the buffer *)
        while !buf <> IntMap.empty do
          let chunk_id, data = IntMap.min_binding !buf in
          assert (chunk_id = !next);
          h data;
          buf := IntMap.remove chunk_id !buf;
          incr next
        done;
        (* Switch off all the workers *)
        let red_threads = threads - 1 in
        for ii = 0 to red_threads do
          output_byte o_2_w.(ii) 0;
          flush o_2_w.(ii)
        done;
        close_pipes_out o_2_w_pipes;
        close_pipes_in w_2_o_pipes
    let process_stream_linewise ?(buffered_chunks_per_thread = 10)
        ?(max_memory = 1000000000) ?(string_buffer_memory = 16777216)
        ?(input_line = input_line) ?(verbose = true)
        input (f:Buffer.t -> int -> string -> unit) output threads =
      let max_block_bytes = max_memory / (buffered_chunks_per_thread * threads) in
      (* Parallel section *)
      let read = ref 0 and eof_reached = ref false
      and processing_buffer = Buffer.create string_buffer_memory and processed = ref 0
      and written = ref 0 in
      if verbose then
        Printf.teprintf "0 lines read\n";
      process_stream_chunkwise ~buffered_chunks_per_thread:buffered_chunks_per_thread
        (fun () ->
          if not !eof_reached then begin
            let bytes = ref 0 and read_base = !read and buf = ref [] in
            begin try
              while !bytes < max_block_bytes do (* We read at least one line *)
                let line = input_line input in
                List.accum buf line;
                bytes := String.length line + !bytes;
                incr read
              done
            with End_of_file ->
              eof_reached := true
            end;
            if verbose then
              Printf.teprintf "%d %s read\n" !read (String.pluralize_int "line" !read);
            read_base, !read - read_base, !buf
          end else
            raise End_of_file)
        (fun (lines_base, lines, buf) ->
          Buffer.clear processing_buffer;
          processed := 0;
          List.iter
            (fun line ->
              f processing_buffer (lines_base + !processed) line;
              incr processed)
            (List.rev buf);
          assert (!processed = lines);
          if verbose then
            Printf.teprintf "%d more %s processed\n" lines (String.pluralize_int "line" lines);
          lines_base, lines, Buffer.contents processing_buffer)
        (fun (_, buf_len, buf) ->
          written := !written + buf_len;
          Stdlib.Printf.fprintf output "%s%!" buf;
          if verbose then
            Printf.teprintf "%d %s written\n" !written (String.pluralize_int "line" !written))
        threads;
      flush output;
      if verbose then
        Printf.teprintf "%d %s out\n" !written (String.pluralize_int "line" !written)
  end
