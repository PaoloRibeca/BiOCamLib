(*
    Tools.ml -- (c) 2015-2024 Paolo Ribeca, <paolo.ribeca@gmail.com>

    This file is part of BiOCamLib, the OCaml foundations upon which
    a number of the bioinformatics tools I developed are built.

    Tools.ml implements a number of general-purpose tools useful
    to write OCaml programs. In particular, it contains several
    additional data structures.

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
    let add_set k s om =
      match KeyMap.find_opt k om with
      | None ->
        KeyMap.add k s om
      | Some ss ->
        KeyMap.replace k (ValSet.union s ss) om
    let add k v om =
      match KeyMap.find_opt k om with
      | None ->
        KeyMap.add k (ValSet.singleton v) om
      | Some s ->
        KeyMap.replace k (ValSet.add v s) om
    let mem k v om =
      match KeyMap.find_opt k om with
      | None ->
        false
      | Some s ->
        ValSet.mem v s
    let remove_set = KeyMap.remove
    let remove k v om =
      match KeyMap.find_opt k om with
      | None ->
        om
      | Some s ->
        let s = ValSet.remove v s in
        if ValSet.is_empty s then
          KeyMap.remove k om
        else
          KeyMap.replace k s om
    let find_set = KeyMap.find
    let find_set_opt = KeyMap.find_opt
    let find_min_elt k om = KeyMap.find k om |> ValSet.min_elt
    let find_min_elt_opt k om =
      match KeyMap.find_opt k om with
      | None -> None
      | Some s -> Some (ValSet.min_elt s) (* Set s cannot be empty here *)
    let find_max_elt k om = KeyMap.find k om |> ValSet.max_elt
    let find_max_elt_opt k om =
      match KeyMap.find_opt k om with
      | None -> None
      | Some s -> Some (ValSet.max_elt s) (* Set s cannot be empty here *)
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
    let min_set = KeyMap.min_binding
    let min_set_opt = KeyMap.min_binding_opt
    let min_elt om =
      let min_k, min_s = KeyMap.min_binding om in
      min_k, ValSet.min_elt min_s
    let min_elt_opt om =
      match KeyMap.min_binding_opt om with
      | None -> None
      | Some (min_k, min_s) ->
        Some (min_k, ValSet.min_elt min_s) (* Set s cannot be empty here *)
    let max_set = KeyMap.max_binding
    let max_set_opt = KeyMap.max_binding_opt
    let max_elt om =
      let max_k, max_s = KeyMap.max_binding om in
      max_k, ValSet.max_elt max_s
    let max_elt_opt om =
      match KeyMap.max_binding_opt om with
      | None -> None
      | Some (max_k, max_s) ->
        Some (max_k, ValSet.max_elt max_s) (* Set s cannot be empty here *)
    let split_set = KeyMap.split
    let split f om =
      let res_l = ref KeyMap.empty and res_c = ref KeyMap.empty and res_r = ref KeyMap.empty in
      KeyMap.iter
        (fun k s ->
          let l, c, r = ValSet.split f s in
          res_l := KeyMap.add k l !res_l;
          res_c := KeyMap.add k c !res_c;
          res_r := KeyMap.add k r !res_r)
        om;
      !res_l, !res_c, !res_r
    let map_set = KeyMap.map
    let map f = KeyMap.map (ValSet.map f)
  end

(* An indexed stack, or extensible array, with additional get() and reverse (bottom-to-top) iterators.
   The interface is compatible with that of Stdlib.Stack *)
module StackArray:
sig
  type !'a t
  exception Empty
  val create: unit -> 'a t
  val push: 'a t -> 'a -> unit (* We depart from Stdlib conventions here and swap arguments *)
  val push_array: 'a t -> 'a array -> unit
  val push_stackarray: 'a t -> 'a t -> unit
  val pop: 'a t -> 'a
  val pop_opt: 'a t -> 'a option
  val pop_n: 'a t -> int -> 'a
  val top: 'a t -> 'a
  val top_opt: 'a t -> 'a option
  val clear: 'a t -> unit
  val reset: 'a t -> unit
  val copy: 'a t -> 'a t
  val is_empty: 'a t -> bool
  val length: 'a t -> int
  val iter: ('a -> unit) -> 'a t -> unit
  val iter_top: ('a -> unit) -> 'a t -> unit
  val riter: ('a -> unit) -> 'a t -> unit
  val iter_bottom: ('a -> unit) -> 'a t -> unit
  val fold: ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
  val fold_top: ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
  val rfold: ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
  val fold_bottom: ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
  exception Not_found
  val get: 'a t -> int -> 'a
  val ( .@() ): 'a t -> int -> 'a
  val set: 'a t -> int -> 'a -> unit
  val ( .@() <- ): 'a t -> int -> 'a -> unit
  val contents: 'a t -> 'a array
end
= struct
  type 'a t = {
    mutable data: 'a array;
    mutable size: int
  }
  exception Empty
  let create () = { data = [||]; size = 0 }
  let push s el =
    let aug_length = s.size + 1 in
    if Array.length s.data >= aug_length then begin
      s.data.(s.size) <- el;
      s.size <- aug_length
    end else begin
      s.data <- Array.resize ~is_buffer:true aug_length el s.data;
      s.size <- aug_length
    end
  let push_array s a =
    let arr_lenght = Array.length a in
    if arr_lenght > 0 then begin
      let aug_length = s.size + arr_lenght in
      if Array.length s.data < aug_length then
        (* There is at least one element in the array *)
        s.data <- Array.resize ~is_buffer:true aug_length a.(0) s.data;
      Array.blit a 0 s.data s.size arr_lenght;
      s.size <- aug_length
    end
  let push_stackarray dst src =
    if src.size > 0 then begin
      let aug_length = dst.size + src.size in
      if Array.length dst.data < aug_length then
        (* There is at least one element in the source *)
        dst.data <- Array.resize ~is_buffer:true aug_length src.data.(0) dst.data;
      Array.blit src.data 0 dst.data dst.size src.size;
      dst.size <- aug_length
    end
  let pop s =
    if s.size > 0 then begin
      s.size <- s.size - 1;
      s.data.(s.size)
    end else
      raise Empty
  let pop_opt s =
    if s.size > 0 then begin
      s.size <- s.size - 1;
      Some s.data.(s.size)
    end else
      None
  let pop_n s n =
    if s.size >= n then begin
      s.size <- s.size - n;
      s.data.(s.size)
    end else
      raise Empty
  let top s =
    if s.size > 0 then
      s.data.(s.size - 1)
    else
      raise Empty
  let top_opt s =
    if s.size > 0 then
      Some s.data.(s.size - 1)
    else
      None
  let clear s =
    s.size <- 0
  let reset s =
    s.data <- [||];
    s.size <- 0
  let copy s = {
    data = Array.copy s.data;
    size = s.size
  }
  let is_empty { size; _ } =
    size = 0
  let length { size; _ } = size
  let iter f s =
    for i = s.size - 1 downto 0 do
      f s.data.(i)
    done
  let riter f s =
    for i = 0 to s.size - 1 do
      f s.data.(i)
    done
  let iter_top = iter
  let iter_bottom = riter
  let fold f start s =
    let rec _fold last rem =
      if rem = 0 then
        last
      else begin
        let red_rem = rem - 1 in
        _fold (f last s.data.(red_rem)) red_rem
      end in
    _fold start s.size
  let rfold f start s =
    let rec _fold last idx =
      if idx = s.size then
        last
      else
        _fold (f last s.data.(idx)) (idx + 1) in
    _fold start 0
  let fold_top = fold
  let fold_bottom = rfold
  exception Not_found
  let get s idx =
    if idx < s.size then
      s.data.(idx)
    else
      raise Not_found
  let ( .@() ) = get
  let set s idx el =
    if idx < s.size then
      s.data.(idx) <- el
    else
      raise Not_found
  let ( .@() <- ) = set
  let contents s =
    Array.sub s.data 0 s.size
end

(* Note that this is _not_ thread-safe *)
module Timer:
  sig
    type t
    val of_string: string -> t
    val to_string: t -> string
    val reset: t -> unit
    val start: t -> unit
    val stop: t -> unit
    exception Not_found of string
    val read: string -> float
  end
= struct
    type t = int
    let id_to_string = StackArray.create ()
    and string_to_id = ref StringMap.empty
    let counters = StackArray.create ()
    and starts = StackArray.create ()
    let ( .@() ) = StackArray.( .@() )
    and ( .@() <- ) = StackArray.( .@() <- )
    let of_string s =
      match StringMap.find_opt s !string_to_id with
      | None ->
        let id = StackArray.length id_to_string in
        StackArray.push id_to_string s;
        string_to_id := StringMap.add s id !string_to_id;
        StackArray.push counters 0.;
        StackArray.push starts 0.;
        id
      | Some id ->
        id
    let to_string n = id_to_string.@(n)
    let reset n =
      counters.@(n) <- 0.;
      starts.@(n) <- 0.
    let start n =
      if starts.@(n) = 0. then
        starts.@(n) <- Unix.gettimeofday ()
    let stop n =
      if starts.@(n) > 0. then begin
        counters.@(n) <- counters.@(n) +. (Unix.gettimeofday () -. starts.@(n));
        starts.@(n) <- 0.
      end
    exception Not_found of string
    let read s =
      match StringMap.find_opt s !string_to_id with
      | None ->
        Not_found s |> raise
      | Some n ->
        counters.@(n)
  end

module Trie:
  sig
    type t
    val create: unit -> t
    val add: t -> string -> t
    val length: t -> int
    exception Not_found
    val nth: t -> int -> string
    (* Enumerates the whole dictionary *)
    val all: t -> string array
    (* Find the longest prefix of the argument present in the dictionary.
       The length of the matched prefix is returned *)
    val longest_prefix: t -> string -> int
    (* Find the longest substring of the string argument present in the dictionary,
        starting from the 0-based index specified by the integer argument.
       The length of the match is returned *)
    val longest_substring: t -> string -> int -> int
    (* Same as find_longest_substring, but only returning complete words in the dictionary.
       Additionally, also returns for the word that was found its index in the dictionary *)
    val longest_match: t -> string -> int -> (int * int)
    type presence_t =
      (* The full query is not present in the dictionary.
         If one of its prefixes is, we return its length *)
      | Not_found of int
      (* There are no exact matches in the dictionary and precisely one, longer match *)
      | Partial of int
      (* There are no exact and several longer matches in the dictionary *)
      | Ambiguous of int list
      (* There is one exact match in the dictionary, but also other longer matches *)
      | Contained of int * int list
      (* There is one exact match and no other longer matches starting with the string *)
      | Unique of int
    val find: t -> string -> presence_t
    val find_present: t -> string -> int option
    val find_unambiguous: t -> string -> int option
    (* Printer function, mostly for diagnostic purposes *)
    val to_string: t -> string
  end
= struct
    (* Each trie node is an array of nodes indexed by char code, plus an integer
        describing whether there is a path terminating at that node.
      When there are no further descendant paths, we leave the array empty.
      If the array is not empty, it has length 256.
      So
        Node (false, [||])
        means that there are no words in the dictionary, while
        Node (true, [||])
        means that there is one word in the dictionary, the empty string, and
        Node (_, [| ... |])
        means that there is at least one non-empty word in the dictionary.
      The integer is the hashed index of the string, with -1 indicating absence *)
    type node_t = Node of int * node_t array
    type t = {
      hash: string StackArray.t;
      trie: node_t
    }
    (* Note that this is in fact immutable *)
    let empty_node = Node (-1, [||])
    let create () = {
      hash = StackArray.create ();
      trie = empty_node
    }
    let to_string t =
      let res = Buffer.create 1024 in
      let rec _to_string = function
        | Node (path_ending_here, ta) ->
          Buffer.add_char res '(';
          if path_ending_here <> -1 then
            Printf.sprintf " [%d]" path_ending_here |> Buffer.add_string res;
          Array.iteri
            (fun c t ->
              if t <> empty_node then begin
                Char.chr c |> Printf.sprintf " '%c'" |> Buffer.add_string res;
                _to_string t
              end)
            ta;
          Buffer.add_string res " )" in
      _to_string t.trie;
      Buffer.contents res
    let add t s =
      let n = String.length s in
      let rec _add i = function
        | Node (path_ending_here, ta) ->
          if i = n then
            if path_ending_here > -1 then
            (* This means that the string is already in the dictionary *)
              t.trie
            else begin
              let id = StackArray.length t.hash in
              StackArray.push t.hash s;
              Node (id, ta)
            end
          else
            let ta =
              if ta = [||] then
                Array.make 256 empty_node
              else
                ta
            and c = Char.code s.[i] in
            if ta.(c) = empty_node then begin
              let id = StackArray.length t.hash in
              StackArray.push t.hash s;
              let tail = Node (id, [||]) |> ref in
              for ii = n - 1 downto i + 1 do
                let ta = Array.make 256 empty_node in
                ta.(Char.code s.[ii]) <- !tail;
                tail := Node (-1, ta)
              done;
              ta.(c) <- !tail
            end else
              ta.(c) <- _add (i + 1) ta.(c);
            Node (path_ending_here, ta) in
      { t with trie = _add 0 t.trie }
    let length t = StackArray.length t.hash
    exception Not_found
    let nth t i =
      if i >= StackArray.length t.hash then
        raise Not_found
      else
        StackArray.(t.hash.@(i))
    let all t = StackArray.contents t.hash
    (* Remember that in all the next functions there is only exactly _one_ path to explore *)
    let _longest_substring t s idx =
      let i = ref idx and n = String.length s and current_node = ref t.trie and res = ref (idx, -1) in
      while !i <= n do
        match !current_node with
        | Node (path_ending_here, ta) ->
          (* We have to update the result if
              either a path ends here, or the array is not empty *)
          if Array.length ta = 0 then begin
            if path_ending_here > -1 then
              res := (!i, path_ending_here);
            (* Exit cycle *)
            i := n
          end else begin
            res := (!i, path_ending_here);
            (* If we are at the last iteration, we don't want to update the node *)
            if !i <> n then
              current_node := ta.(Char.code s.[!i])
          end;
          incr i
      done;
      let i, id = !res in
      i - idx, id, { t with trie = !current_node }
    let longest_substring t s idx = let (res, _, _) = _longest_substring t s idx in res
    let longest_prefix t s = let (res, _, _) = _longest_substring t s 0 in res
    let longest_match t s idx =
      let i = ref idx and n = String.length s and current_node = ref t.trie and res = ref (idx, -1) in
      while !i <= n do
        match !current_node with
        | Node (path_ending_here, ta) ->
          (* We update the result every time a path ends here *)
          if path_ending_here > -1 then
            res := (!i, path_ending_here);
          if Array.length ta = 0 then
            (* Exit cycle *)
            i := n
          else begin
            (* If we are at the last iteration, we don't want to update the node *)
            if !i <> n then
              current_node := ta.(Char.code s.[!i])
          end;
          incr i
      done;
      let i, id = !res in
      i - idx, id
    type presence_t =
      | Not_found of int
      | Partial of int
      | Ambiguous of int list
      | Contained of int * int list
      | Unique of int
    let find t s =
      (* First, we find the longest prefix *)
      let n = String.length s and l, id, tt = _longest_substring t s 0 in
      if l < n then
        Not_found l
      else begin
        let tails = ref [] in
        let rec _find_all_tails = function
          | Node (path_ending_here, ta) ->
            if path_ending_here > -1 then
              List.accum tails path_ending_here;
            (* It's OK to iterate on the array even if it is empty *)
            Array.iter _find_all_tails ta in
        _find_all_tails tt.trie;
        let tails = List.rev !tails and card = List.length !tails in
        if id = -1 then begin
          (* The string matches, but only as a prefix of longer matches *)
          assert (card > 0);
          if card = 1 then
            Partial id
          else
            Ambiguous tails
        end else begin
          if card = 0 then
            Unique id
          else
            Contained (id, tails)
        end
      end
    let find_present t s =
      match find t s with
      | Not_found _ | Partial _ | Ambiguous _ -> None
      | Contained (id, _) | Unique id -> Some id
    let find_unambiguous t s =
      match find t s with
      | Not_found _ | Ambiguous _ -> None
      | Partial id | Contained (id, _) | Unique id -> Some id
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
    let header ?(output = stderr) () = Printf.fprintf output "%s%!" !_header
    let synopsis ?(output = stderr) () = Printf.fprintf output "%s%!" !_synopsis
    let usage ?(output = stderr) () = Printf.fprintf output "%s%!" !_usage
    let markdown ?(output = stderr) () = Printf.fprintf output "%s%!" !_md_usage
    let error ?(output = stderr) f_n msg =
      let open String.TermIO in
      usage ~output ();
      Printf.fprintf output "(%s): %s\n%!" f_n (red msg);
      exit 1
    let template_get n what f =
      (fun () ->
        try
          f ()
        with _ ->
          error n ("Option '" ^ argv.(!i - 1) ^ "' needs one (more)" ^ what ^ "parameter"))
    let template_filter f g =
      (fun () ->
        let res = f () in
        if g res then
          res
        else
          raise Not_found)
    let get_parameter =
      template_get __FUNCTION__ " " (fun () -> incr i; argv.(!i))
    let get_parameter_boolean =
      template_get __FUNCTION__ " boolean " (fun () -> get_parameter () |> bool_of_string)
    let get_parameter_int =
      template_get __FUNCTION__ " integer " (fun () -> get_parameter () |> int_of_string)
    let get_parameter_float =
      template_get __FUNCTION__ " float " (fun () -> get_parameter () |> float_of_string)
    let get_parameter_int_pos =
      template_get __FUNCTION__ " positive integer as "
      (template_filter get_parameter_int (fun x -> x > 0))
    let get_parameter_float_pos =
      template_get __FUNCTION__ " positive float as "
      (template_filter get_parameter_float (fun x -> x > 0.))
    let get_parameter_int_non_neg =
      template_get __FUNCTION__ " non-negative integer as "
      (template_filter get_parameter_int (fun x -> x >= 0))
    let get_parameter_float_non_neg =
      template_get __FUNCTION__ " non-negative float as "
      (template_filter get_parameter_float (fun x -> x >= 0.))
    let get_parameter_int_percentage =
      template_get __FUNCTION__ " integer between 0 and 100 as "
      (template_filter get_parameter_int (fun x -> x >= 0 && x <= 100))
    let get_parameter_float_fraction =
      template_get __FUNCTION__ " float between 0 and 1 as "
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
      and trie = Trie.create () |> ref and table = ref StringMap.empty and mandatory = ref StringSet.empty in
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
              if Trie.find_present !trie opt <> None then
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
      begin try
        while !i < len do
          let arg = argv.(!i) in
          begin match Trie.find_unambiguous trie arg with
          | None -> error __FUNCTION__ ("Unknown option '" ^ arg ^ "'")
          | Some id ->
            (* Some exception might occur while parsing values *)
            arg |> StringMap.find (Trie.nth trie id) table
          end;
          incr i
        done
      with
      | Failure s ->
        error __FUNCTION__ s
      | e ->
        error __FUNCTION__ (Printexc.to_string e)
      end;
      if !mandatory <> StringSet.empty then
        StringSet.iter
          (fun opt ->
            error __FUNCTION__ ("Option '" ^ opt ^ "' is mandatory"))
          !mandatory
    let parse_error ?(output = stderr) = error ~output __FUNCTION__
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

