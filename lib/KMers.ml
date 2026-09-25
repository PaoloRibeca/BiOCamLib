(*
    KMers.ml -- (c) 2020-2026 Paolo Ribeca, <paolo.ribeca@gmail.com>

    This file is part of BiOCamLib, the OCaml foundations upon which
    a number of the bioinformatics tools I developed are built.

    KMers.ml implements tools to iterate over, and hash, k-mers.

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

open Better

module SlidingWindow:
  sig
    type t
    val make: string -> t
    val length: t -> int
    val add_char: t -> char -> char
    val contents: t -> string
  end
= struct
    type t = {
      drum: Bytes.t;
      mutable index: int
    }
    let make s = {
      drum = Bytes.of_string s;
      index = 0
    }
    let length w = Bytes.length w.drum
    let add_char w c =
      let res = Bytes.get w.drum w.index in
      Bytes.set w.drum w.index c;
      w.index <- (w.index + 1) mod (Bytes.length w.drum);
      res
    let contents w =
      let len = Bytes.length w.drum in
      Bytes.sub_string w.drum w.index (len - w.index) ^ Bytes.sub_string w.drum 0 w.index
  end
module DoubleSlidingWindow:
  sig
    type t
    val make: string -> t
    val support: t -> int
    val diffs: t -> int
    val add_char: t -> char -> char -> int
    val to_string: t -> string
  end
= struct
    type t = {
      one: SlidingWindow.t;
      two: SlidingWindow.t;
      mutable support: int;
      mutable diffs: int
    }
    let make s = {
      one = SlidingWindow.make s;
      two = SlidingWindow.make s;
      support = 0;
      diffs = 0
    }
    let support d = d.support
    let diffs d = d.diffs
    let add_char d c1 c2 =
      let old_c1 = SlidingWindow.add_char d.one c1
      and old_c2 = SlidingWindow.add_char d.two c2 in
      d.support <- min (d.support + 1) (SlidingWindow.length d.one);
      d.diffs <- d.diffs - (if old_c1 <> old_c2 then 1 else 0) + (if c1 <> c2 then 1 else 0);
      d.diffs
    let to_string d =
      Printf.sprintf "|%s| <-> |%s|" (SlidingWindow.contents d.one) (SlidingWindow.contents d.two)
  end

(* Auxiliary module type to generate hashes from sequences of integers *)
module type Hash_t =
  sig
    type t
    (* Compute the hash for an encoded k-mer.
       Arguments are encoded vector and 0-based starting index.
       There must be at least k symbols left from there to the end of the vector,
        and the value of the symbol must not exceed 2^(the number of bits)-1,
        or initialisation will fail *)
    val compute: int array -> int -> t (* Can fail *)
    val symbol_complement: int -> int
    (* Returns a generalised version of the reverse complement *)
    val rc: t -> t
    (* Return the hash for a k-mer obtained by adding one character to the right end
        of the argument and discarding one character at the left end *)
    val add_symbol_right: t -> int -> t
    (* Same but on the other end *)
    val add_symbol_left: t -> int -> t
    (* Suitable accumulators for values
        (Accumulator1 for regular k-mers, Accumulator2 for gapped k-mers).
       For each of them we also need fast functions to compute the minimum value *)
    module Accumulator1: Hashtbl.S with type key = t
    val min1: t -> t -> t
    module Accumulator2: Hashtbl.S with type key = t * t
    val min2: (t * t) -> (t * t) -> (t * t)
    (* Return the hash as a string, for instance in hex form *)
    val to_string: t -> string
  end
module IntHash (Bits: IntParameter_t) (K: IntParameter_t): Hash_t with type t = Int.t =
  struct
    type t = Int.t
    let bits =
      (* We need one additional bit to be able to compute the mask *)
      if Bits.n < 1 || Bits.n >= Sys.int_size then
        Exception.raise __FUNCTION__ Initialize (Printf.sprintf "Invalid number of bits %d" Bits.n);
      Bits.n
    let k =
      if K.n < 1 then
        Exception.raise __FUNCTION__ Initialize (Printf.sprintf "Invalid k-mer size %d" K.n);
      if K.n * Bits.n > Sys.int_size then
        Exception.raise __FUNCTION__ Initialize
          (Printf.sprintf "Invalid combination number of bits/k-mer size (%d, %d)" Bits.n K.n);
      K.n
    let mask = 1 lsl (bits * k) - 1
    let max_symbol = 1 lsl bits - 1
    let symbol_complement s = max_symbol - s [@@inline]
    let compute a idx =
      let l = Array.length a in
      if idx + k > l then
        Exception.raise __FUNCTION__ Algorithm
          (Printf.sprintf "Invalid index value %d (string length=%d, k-mer size=%d)" idx l k);
      let res = ref 0 in
      for i = idx to idx + k - 1 do
        let s = a.(i) in
        if s > max_symbol then
          Exception.raise __FUNCTION__ IO_Format (Printf.sprintf "Invalid symbol '%d' in input" s);
        res := (!res lsl bits) + s
      done;
      !res
    let add_symbol_right h s = ((h lsl bits) lor s) land mask [@@inline]
    let left_shift = bits * (k - 1)
    let add_symbol_left h s = (s lsl left_shift) lor (h lsr bits) [@@inline]
    let rc h =
      let h = ref h and res = ref 0 in
      for _ = 1 to k do
        res := (!res lsl bits) + (max_symbol - (!h land max_symbol));
        h := !h lsr bits
      done;
      !res
    module Accumulator1 = IntHashtbl
    let min1 = Int.min (* This is the same as (fun a b -> if a < b then a else b) *)
    type tt = t * t
    module Accumulator2 = Hashtbl.Make (MakeHashable (struct type t = tt end))
    let min2 ((a1, a2) as a) ((b1, b2) as b) =
      (* Unclear how to optimise this *)
      if a1 < b1 || (a1 = b1 && a2 < b2) then
        a
      else
        b
      [@@inline]
    let ceil_log16 = (bits * k + 3) / 4
    let to_string h =
      let res = Bytes.create ceil_log16 and rem = ref h in
      for i = ceil_log16 - 1 downto 0 do
        Bytes.(res.@(i) <- begin
          match !rem land 15 with
          | 0 -> '0'
          | 1 -> '1'
          | 2 -> '2'
          | 3 -> '3'
          | 4 -> '4'
          | 5 -> '5'
          | 6 -> '6'
          | 7 -> '7'
          | 8 -> '8'
          | 9 -> '9'
          | 10 -> 'a'
          | 11 -> 'b'
          | 12 -> 'c'
          | 13 -> 'd'
          | 14 -> 'e'
          | 15 -> 'f'          
          | _ -> assert false
        end);
        rem := !rem lsr 4
      done;
      Bytes.to_string res
  end
module IntZHash (Bits: IntParameter_t) (K: IntParameter_t): Hash_t with type t = IntZ.t =
  struct
    type t = IntZ.t
    let bits =
      if Bits.n < 1 then
        Exception.raise __FUNCTION__ Initialize (Printf.sprintf "Invalid number of bits %d" Bits.n);
      Bits.n
    let k =
      if K.n < 1 then
        Exception.raise __FUNCTION__ Initialize (Printf.sprintf "Invalid k-mer size %d" K.n);
      K.n
    let mask =
      let bits_times_k = bits * k in
      IntZ.(one lsl bits_times_k - one)
    let max_symbol = 1 lsl bits - 1
    let symbol_complement s = max_symbol - s
    let max_symbol_z = IntZ.of_int max_symbol
    let compute a idx =
      let l = Array.length a in
      if idx + k > l then
        Exception.raise __FUNCTION__ Algorithm
          (Printf.sprintf "Invalid index value %d (string length=%d, k-mer size=%d)" idx l k);
      let res = ref IntZ.zero in
      for i = idx to idx + k - 1 do
        let s = a.(i) in
        if s > max_symbol then
          Exception.raise __FUNCTION__ IO_Format (Printf.sprintf "Invalid symbol '%d' in input" s);
        res := IntZ.((!res lsl bits) + of_int s)
      done;
      !res
    let add_symbol_right h s = IntZ.(((h lsl bits) lor of_int s) land mask)
    let left_shift = bits * (k - 1)
    (* IntZ doesn't seem to have lsr, but asr should be fine as h is positive *)
    let add_symbol_left h s = IntZ.((of_int s lsl left_shift) lor (h asr bits))
    let rc h =
      let h = ref h and res = ref IntZ.zero in
      for _ = 1 to k do
        res := IntZ.((!res lsl bits) + (max_symbol_z - (!h land max_symbol_z)));
        (* IntZ doesn't seem to have lsr, but asr should be fine as h is positive *)
        h := IntZ.(!h asr bits)
      done;
      !res
    module Accumulator1 = IntZHashtbl
    let min1 = IntZ.min
    type tt = t * t
    module Accumulator2 = Hashtbl.Make (MakeHashable (struct type t = tt end))
    let min2 ((a1, a2) as a) ((b1, b2) as b) =
      (* The condition would be a1 < b1 || (a1 = b1 && a2 < b2) *)
      if IntZ.(lt a1 b1 || (equal a1 b1 && lt a2 b2)) then
        a
      else
        b
      [@@inline]
    let hex_format = Printf.sprintf "%%0%dx" ((bits * k + 3) / 4)
    let to_string = IntZ.format hex_format
  end

(* This module takes a number of parameters and generates an iterator over the k-mers
    contained in a string. K-mers are turned into numerical hashes that get presented
    to the iterator as strings.
   First parameter is an adaptor function which, depending on what the string is
    (DNA, protein, ...) performs checks on the sequence, for instance linting. Also
    this step produces flags for a filter that will be applied to all the hashes
    prior to iteration, for instance to remove duplicated DNA k-mers coming from
    reverse complementing the string.
   Second parameter is a dictionary/trie which is used to turn the input strings
    into vectors of numbers, depending on what the string is (DNA, protein, ...)
    and the encoding strategy (single-letter alphabet, byte encoding, dictionary).
    At this stage, strings containing unencodable characters get split.
   Third parameter is a function to turn the vectors of numbers into a set of k-mers
    (single sliding window of fixed length k, double gapped sliding window, ...).
    Depending on parameters (for instance, a large k or the alphabet size), a
    different integer implementation might be used to generate hashes (for instance,
    machine integers or Zarith) *)

module Iterator:
  sig
    module Content:
      sig
        module Strandedness:
          sig
            type t = Single | Double
          end
        module CaseSensitivity:
          sig
            type t = Insensitive | Sensitive
          end
        module UnknownCharAction:
          sig
            type t = Split | Ignore | Error
          end
        type t =
          | DNA of Strandedness.t * CaseSensitivity.t * UnknownCharAction.t
          | Protein of UnknownCharAction.t
          (* The third parameter is the file path at which the dictionary can be found.
             Note that case sensitivity has an effect on how such file is parsed *)
          | Text of CaseSensitivity.t * UnknownCharAction.t * string (* File path *)
        val of_string: string -> t (* Can fail *)
        val to_string: t -> string
        module Flags:
          sig
            type t = {
              unknown_char_action: UnknownCharAction.t;
              rc_symmetric_hash: bool
            }
          end
        (* The results are adaptor function and flags *)
        val make: t -> (string -> string) * Flags.t
      end
    module Encoder:
      sig
        type t =
          | DNA of Content.CaseSensitivity.t
          | Protein
          (* The second parameter is the file path at which the dictionary can be found.
             Note that case sensitivity has an effect on how such file is parsed *)
          | Dictionary of Content.CaseSensitivity.t * string (* File path *)
          | Test of string list (* No constructor from content - not really used in production *)
        val of_content: Content.t -> t
        (* The integer is the alphabet size *)
        val make: ?verbose:bool -> Content.Flags.t -> t -> int * (string -> int array list)
      end
    module Hasher:
      sig
        type t =
          | K_mers of int
          | Gapped of int * int
        val of_string: string -> t
        val to_string: t -> string
        (* Arguments are alphabet size and iterator function - and an optional
            max size parameter to determine if the accumulator should be
            periodically processed through the iterator and flushed.
           Two functions are returned, an accumulator and a finaliser.
           The accumulator can be called repeatedly on different encoded strings;
            its argument is the encoded vector. An optional argument can be provided
             to weigh k-mers based on depth.
           The finaliser applies the iterator to the hashes accumulated so far
            and deallocates storage, pretty much as what happens when flushing *)
        val make: ?verbose:bool ->
                  Content.Flags.t -> int -> t -> (string array -> int -> float -> unit) ->
                  (?weight:float -> int array -> unit) * (unit -> unit)
      end
    (* The inverse of the encoder and hasher: given the content and hasher used to
        produce them, it turns each hash string back into the corresponding
        (strand-canonical) sequence. The gap positions of gapped k-mers, which are
        not hashed, are rendered using the gap character (a dash by default) *)
    module Decoder:
      sig
        val make: ?verbose:bool -> ?gap:char -> Content.t -> Hasher.t -> (string -> string)
      end
    (* The two functions are accumulator and finaliser *)
    type t = (?weight:float -> string -> unit) * (unit -> unit)
    (* The last argument is the iterator function *)
    val make: ?verbose:bool -> Content.t -> Hasher.t -> (string array -> int -> float -> unit) -> t
  end
= struct
    module Content =
      struct
        module Strandedness =
          struct
            type t =
              | Single
              | Double
            let of_string = function
              | "ss" | "SS" | "single-stranded" -> Single
              | "ds" | "DS" | "double-stranded" -> Double
              | s ->
                Exception.raise_unrecognized_initializer __FUNCTION__ "strandedness" s
            let to_string = function
              | Single -> "single-stranded"
              | Double -> "double-stranded"
          end
        module CaseSensitivity =
          struct
            type t =
              | Insensitive
              | Sensitive
            let of_string = function
              | "ci" | "case-insensitive" -> Insensitive
              | "cs" | "case-sensitive" -> Sensitive
              | s ->
                Exception.raise_unrecognized_initializer __FUNCTION__ "case sensitivity" s
            let to_string = function
              | Insensitive -> "case-insensitive"
              | Sensitive -> "case-sensitive"
          end
        module UnknownCharAction =
          struct
            type t =
              | Split
              | Ignore
              | Error
            let of_string = function
              | "split" -> Split
              | "ignore" | "skip" -> Ignore
              | "error" | "abort" -> Error
              | s ->
                Exception.raise_unrecognized_initializer __FUNCTION__ "action" s
            let to_string = function
              | Split -> "split"
              | Ignore -> "ignore"
              | Error -> "error"
          end
        type t =
          | DNA of Strandedness.t * CaseSensitivity.t * UnknownCharAction.t
          | Protein of UnknownCharAction.t
          | Text of CaseSensitivity.t * UnknownCharAction.t * string
        let of_string_re = Str.regexp "[(,)]"
        let of_string s =
          let raise () = Exception.raise_unrecognized_initializer __FUNCTION__ "content" s in
          match Str.full_split of_string_re s with
          (* First, a few simplified options with default choices *)
          | [ Text "ss-DNA" ] | [ Text "SS-DNA" ] | [ Text "single-stranded-DNA" ] ->
            DNA (Strandedness.Single, CaseSensitivity.Insensitive, UnknownCharAction.Split)
          | [ Text "ds-DNA" ] | [ Text "DS-DNA" ] | [ Text "double-stranded-DNA" ] ->
            DNA (Strandedness.Double, CaseSensitivity.Insensitive, UnknownCharAction.Split)
          | [ Text "protein" ] ->
            Protein UnknownCharAction.Split
          (* Then, the full versions *)
          | [ Str.Text "DNA"; Delim "(";
              Text strandedness; Delim ","; Text case_sensitivity; Delim ","; Text unknown_char_action;
              Delim ")" ] ->
            begin try
              DNA (Strandedness.of_string strandedness,
                   CaseSensitivity.of_string case_sensitivity,
                   UnknownCharAction.of_string unknown_char_action)
            with _ ->
              raise ()
            end
          | [ Text "protein"; Delim "("; Text unknown_char_action; Delim ")" ] ->
            begin try
              Protein (UnknownCharAction.of_string unknown_char_action)
            with _ ->
              raise ()
            end
          (* Unfortunately it does not make much sense to specify default options here *)
          | Text "text" :: Delim "(" ::
              Text case_sensitivity :: Delim "," :: Text unknown_char_action :: Delim "," ::
              tl ->
            (* We put the path back together *)
            let tl = Array.of_list tl in
            let l = Array.length tl in
            let red_l = l - 1 in
            if l = 0 || tl.(red_l) <> Delim ")" then
              raise ();
            let path = ref "" in
            for i = 0 to red_l - 1 do
              path := !path ^ (match tl.(i) with Text s | Delim s -> s)
            done;
            begin try
              Text (CaseSensitivity.of_string case_sensitivity,
                    UnknownCharAction.of_string unknown_char_action,
                    !path)
            with _ ->
              raise ()
            end
          | _ ->
            raise ()
        let to_string = function
          | DNA (strandedness, case_sensitivity, unknown_char_action) ->
            Printf.sprintf "DNA(%s,%s,%s)"
              (Strandedness.to_string strandedness)
              (CaseSensitivity.to_string case_sensitivity)
              (UnknownCharAction.to_string unknown_char_action)
          | Protein unknown_char_action ->
            Printf.sprintf "protein(%s)" (UnknownCharAction.to_string unknown_char_action)
          | Text (case_sensitivity, unknown_char_action, path) ->
            Printf.sprintf "text(%s,%s,%s)"
              (CaseSensitivity.to_string case_sensitivity) (UnknownCharAction.to_string unknown_char_action) path
        module Flags =
          struct
            type t = {
              unknown_char_action: UnknownCharAction.t;
              rc_symmetric_hash: bool
            }
          end
        let make c =
          let case_sensitivity_to_keep_lowercase = function
            | CaseSensitivity.Insensitive -> false
            | CaseSensitivity.Sensitive -> true in
          match c with
          | DNA (Strandedness.Single, case_sensitivity, unknown_char_action) ->
            let keep_lowercase = case_sensitivity_to_keep_lowercase case_sensitivity in
            Sequences.Lint.dnaize ~keep_lowercase ~keep_dashes:false,
            { Flags.unknown_char_action; rc_symmetric_hash = false }
          | DNA (Double, case_sensitivity, unknown_char_action) ->
            let keep_lowercase = case_sensitivity_to_keep_lowercase case_sensitivity in
            Sequences.Lint.dnaize ~keep_lowercase ~keep_dashes:false,
            { unknown_char_action; rc_symmetric_hash = true }
          | Protein unknown_char_action ->
            Sequences.Lint.proteinize ~keep_lowercase:false ~keep_dashes:false,
            { unknown_char_action; rc_symmetric_hash = false }
          | Text (CaseSensitivity.Insensitive, unknown_char_action, _) ->
            String.lowercase_ascii,
            { unknown_char_action; rc_symmetric_hash = false }
          | Text (CaseSensitivity.Sensitive, unknown_char_action, _) ->
            Fun.id,
            { unknown_char_action; rc_symmetric_hash = false }
        (* We wrap the function to provide instrumentation *)
        let make c =
          let timer_id_content = Tools.Timer.of_string "KMers.Iterator.Linter" in
          let linter, flags = make c in
          (fun s ->
            Tools.Timer.start timer_id_content;
            let res = linter s in
            Tools.Timer.stop timer_id_content;
            res),
          flags
      end
    module Encoder =
      struct
        type t =
          | DNA of Content.CaseSensitivity.t
          | Protein
          | Dictionary of Content.CaseSensitivity.t * string
          | Test of string list
        let of_content = function
          | Content.DNA (_, case_sensitivity, _) -> DNA case_sensitivity
          | Protein _ -> Protein
          | Text (case_sensitivity, _, path) -> Dictionary (case_sensitivity, path)
        (* Builds the trie mapping dictionary tokens to consecutive integer ids;
            it is shared by the encoder and its inverse *)
        let make_trie ?(verbose = false) e =
          let dict =
            match e with
            | Test l ->
              l
            | DNA case_sensitivity ->
              begin match case_sensitivity with
              | Content.CaseSensitivity.Insensitive ->
                [ "A"; "C"; "G"; "T" ]
              | Content.CaseSensitivity.Sensitive ->
                [ "A"; "C"; "G"; "T"; "a"; "c"; "g"; "t" ]
              end
            | Protein ->
              [ "A"; "C"; "D"; "E"; "F"; "G"; "H"; "I"; "K"; "L"; "M";
                "N"; "O"; "P"; "Q"; "R"; "S"; "T"; "U"; "V"; "W"; "Y" ]
            | Dictionary (case_sensitivity, path) ->
              let case_adaptor =
                match case_sensitivity with
                | Content.CaseSensitivity.Insensitive ->
                  String.lowercase_ascii
                | Sensitive ->
                  Fun.id
              and file = open_in path and progr = ref 0 and dict = ref [] in
              if verbose then
                Printf.eprintf "(%s): Reading dictionary file '%s'...%!" __FUNCTION__ path;
              begin try
                while true do
                  let line = input_line file in
                  if line <> "" then
                    case_adaptor line |> List.accum dict;
                  incr progr
                done
              with End_of_file ->
                close_in file;
                if verbose then
                  Printf.eprintf " found %d %s.\n%!" !progr (String.pluralize_int "symbol" !progr)
              end;
              !dict in
          let trie = Tools.Trie.create () |> ref in
          List.iter
            (fun s ->
              trie := Tools.Trie.add !trie s)
            dict;
          !trie
        let make ?(verbose = false) flags e =
          let trie = make_trie ~verbose e in
          let timer_id_encoder = Tools.Timer.of_string "KMers.Iterator.Encoder" in
          Tools.Trie.length trie,
          (fun s ->
            Tools.Timer.start timer_id_encoder;
            let l_src = String.length s in
            let current = Array.make l_src 0
            and i_src = ref 0 and l_dst = ref 0 and res = ref [] in
            let add_current_to_res () =
              if !l_dst > 0 then begin
                Array.sub current 0 !l_dst |> List.accum res;
                l_dst := 0
              end in
            while !i_src < l_src do
              let n, id = Tools.Trie.longest_match trie s !i_src in
              if n = 0 then begin
                (* Case of no dictionary word found - what we do depends on the flags *)
                match flags.Content.Flags.unknown_char_action with
                | Content.UnknownCharAction.Split ->
                  (* We split the string and skip one character *)
                  add_current_to_res ();
                  incr i_src
                | Ignore ->
                  (* We just skip the character *)
                  incr i_src
                | Error ->
                  Exception.raise_unrecognized_initializer __FUNCTION__ "char" (string_of_char s.[!i_src])
              end else begin
                current.(!l_dst) <- id;
                i_src := !i_src + n;
                incr l_dst
              end
            done;
            add_current_to_res ();
            (*
            Printf.printf "Result has %d elements of lengths (" (List.length !res);
            List.iter
              (fun ia -> Array.length ia |> Printf.printf " %d")
              !res;
            Printf.printf " )\n%!";
            *)
            Tools.Timer.stop timer_id_encoder;
            !res)
      end
    (* Number of bits per symbol needed to encode an alphabet of the given size *)
    let n_bits_of_n_symbols n_symbols =
      assert (n_symbols > 0);
      let res = ref 1 and rem = n_symbols - 1 |> ref in
      while rem := !rem lsr 1; !rem > 0 do
        incr res
      done;
      !res
    module Hasher =
      struct
        type t =
          | K_mers of int
          | Gapped of int * int
        let of_string_re = Str.regexp "[(,)]"
        let of_string s =
          match Str.full_split of_string_re s with
          | [ Text "k-mers"; Delim "("; Text k; Delim ")" ] ->
            K_mers (int_of_string k)
          | [ Text "gapped"; Delim "("; Text k; Delim ","; Text gap_size; Delim ")" ] ->
            Gapped (int_of_string k, int_of_string gap_size)
          | _ ->
            Exception.raise_unrecognized_initializer __FUNCTION__ "hasher" s
        let to_string = function
          | K_mers k ->
            Printf.sprintf "k-mers(%d)" k
          | Gapped (k, g) ->
            Printf.sprintf "gapped(%d,%d)" k g
        let make ?(verbose = false) flags n_symbols h f =
          let impl =
            let n_bits = n_bits_of_n_symbols n_symbols
            and k = match h with K_mers k | Gapped (k, _) -> k in
            try
              let res = (module IntHash (struct let n = n_bits end) (struct let n = k end): Hash_t) in
              if verbose then
                Printf.eprintf "(%s): Initializing small encoder (bits=%d, k=%d)\n%!" __FUNCTION__ n_bits k;
              res
            with _ ->
              if verbose then
                Printf.eprintf "(%s): Initializing large encoder (bits=%d, k=%d)\n%!" __FUNCTION__ n_bits k;
              (module IntZHash (struct let n = n_bits end) (struct let n = k end): Hash_t) in
          let module Impl = (val impl: Hash_t) in
          match h with
          | K_mers k ->
            let hs = Tools.ArrayStack.empty () and res = Impl.Accumulator1.create 128 in
            let add h w =
              match Impl.Accumulator1.find_opt res h with
              | None ->
                (Tools.ArrayStack.length hs, ref w) |> Impl.Accumulator1.add res h;
                Tools.ArrayStack.push hs (Impl.to_string h)
              | Some (_, n) ->
                n := !n +. w
              [@@inline] in
            (* Accumulator *)
            let timer_id_accumulate = Tools.Timer.of_string "KMers.Iterator.Accumulator" in
            (fun ?(weight = 1.) ia ->
              Tools.Timer.start timer_id_accumulate;
              let l = Array.length ia in
              if l >= k then begin
                let current = Impl.compute ia 0 |> ref in
                let rc = Impl.rc !current |> ref in
                if flags.Content.Flags.rc_symmetric_hash then
                  add (Impl.min1 !current !rc) weight
                else
                  add !current weight;
                for i = k to l - 1 do
                  current := Impl.add_symbol_right !current ia.(i);
                  if flags.rc_symmetric_hash then begin
                    rc := Impl.symbol_complement ia.(i) |> Impl.add_symbol_left !rc;
                    add (Impl.min1 !current !rc) weight
                  end else
                    add !current weight;
                done
              end;
              Tools.Timer.stop timer_id_accumulate),
            (* Finaliser *)
            let timer_id_finalizer = Tools.Timer.of_string "KMers.Iterator.Finalizer" in
            (fun () ->
              Tools.Timer.start timer_id_finalizer;
              let hs = Tools.ArrayStack.contents hs in
              Impl.Accumulator1.iter
                (fun _ (id, n) ->
                  if !n > 0. then begin
                    f hs id !n;
                    n := 0.
                  end)
                res;
              (*Impl.Accumulator1.clear res;*)
              Tools.Timer.stop timer_id_finalizer)
          | Gapped (k, g) ->
            let hs = Tools.ArrayStack.empty () and res = Impl.Accumulator2.create 128 in
            let add ((h1, h2) as hh) w =
              match Impl.Accumulator2.find_opt res hh with
              | None ->
                (Tools.ArrayStack.length hs, ref w) |> Impl.Accumulator2.add res hh;
                Tools.ArrayStack.push hs (Impl.to_string h1 ^ "_" ^ Impl.to_string h2)
              | Some (_, n) ->
                n := !n +. w
              [@@inline] in
            (* Here we just have to simulate a longer k *)
            let eff_k = 2 * k + g and offs = k + g in
            (* Accumulator *)
            let timer_id_accumulate = Tools.Timer.of_string "KMers.Iterator.Accumulator" in
            (fun ?(weight = 1.) ia ->
              Tools.Timer.start timer_id_accumulate;
              let l = Array.length ia in
              if l >= eff_k then begin
                let current1 = Impl.compute ia 0 |> ref
                and current2 = Impl.compute ia offs |> ref in
                let rc1 = Impl.rc !current1 |> ref
                and rc2 = Impl.rc !current2 |> ref in
                if flags.rc_symmetric_hash then
                  (* Remember that rc(h1|h2) = rc(h2)|rc(h1) *)
                  add (Impl.min2 (!current1, !current2) (!rc2, !rc1)) weight
                else
                  add (!current1, !current2) weight;
                for i = eff_k to l - 1 do
                  let c1 = ia.(i - offs) and c2 = ia.(i) in
                  current1 := Impl.add_symbol_right !current1 c1;
                  current2 := Impl.add_symbol_right !current2 c2;
                  if flags.rc_symmetric_hash then begin
                    rc1 := Impl.symbol_complement c1 |> Impl.add_symbol_left !rc1;
                    rc2 := Impl.symbol_complement c2 |> Impl.add_symbol_left !rc2;
                    (* Remember that rc(h1|h2) = rc(h2)|rc(h1) *)
                    add (Impl.min2 (!current1, !current2) (!rc2, !rc1)) weight
                  end else
                    add (!current1, !current2) weight
                done
              end;
              Tools.Timer.stop timer_id_accumulate),
            (* Finaliser *)
            let timer_id_finalizer = Tools.Timer.of_string "KMers.Iterator.Finalizer" in
            (fun () ->
              Tools.Timer.start timer_id_finalizer;
              let hs = Tools.ArrayStack.contents hs in
              Impl.Accumulator2.iter
                (fun _ (id, n) ->
                  if !n > 0. then begin
                    f hs id !n;
                    n := 0.
                  end)
                res;
              (*Impl.Accumulator2.clear res;*)
              Tools.Timer.stop timer_id_finalizer)
      end
    module Decoder =
      struct
        let make ?(verbose = false) ?(gap = '-') content hasher =
          let trie = Encoder.of_content content |> Encoder.make_trie ~verbose in
          let n_symbols = Tools.Trie.length trie
          and id_to_symbol = Tools.Trie.nth trie in
          let fail message = Exception.raise __FUNCTION__ IO_Format message in
          if n_symbols = 0 then
            fail "Empty alphabet (the dictionary file contains no tokens)";
          let n_bits = n_bits_of_n_symbols n_symbols
          and k = match hasher with Hasher.K_mers k | Gapped (k, _) -> k in
          let block_width = (n_bits * k + 3) / 4
          and max_symbol = IntZ.(one lsl n_bits - one) in
          let decode_block hex =
            if String.length hex <> block_width then
              Printf.sprintf "Invalid hash block '%s' (expected %d hex digits)" hex block_width
                |> fail;
            let h =
              try
                IntZ.of_string ("0x" ^ hex)
              with _ ->
                Printf.sprintf "Invalid hash block '%s'" hex |> fail in
            let h = ref h and symbols = Array.make k "" in
            for pos = k - 1 downto 0 do
              let symbol = IntZ.(!h land max_symbol) |> IntZ.to_int in
              if symbol >= n_symbols then
                Printf.sprintf "Invalid hash block '%s' (symbol id %d not in %d-symbol alphabet)"
                  hex symbol n_symbols |> fail;
              symbols.(pos) <- id_to_symbol symbol;
              h := IntZ.(!h asr n_bits)
            done;
            if IntZ.equal !h IntZ.zero |> not then
              Printf.sprintf "Invalid hash block '%s' (extra high-order bits; do -k and -c match?)"
                hex |> fail;
            Array.to_list symbols |> String.concat "" in
          match hasher with
          | Hasher.K_mers _ ->
            decode_block
          | Gapped (_, g) ->
            let gap_run = String.make g gap in
            (fun hash ->
              match String.Split.on_char_as_array '_' hash with
              | [| hex1; hex2 |] ->
                decode_block hex1 ^ gap_run ^ decode_block hex2
              | _ ->
                Printf.sprintf "Invalid gapped hash '%s' (expected two blocks separated by '_')"
                  hash |> fail)
      end
    type t = (?weight:float -> string -> unit) * (unit -> unit)
    let make ?(verbose = false) content hasher f =
      let encoder = Encoder.of_content content
      and linter, flags = Content.make content in
      let n_symbols, encoder = Encoder.make ~verbose flags encoder in
      let accumulator, finalizer = Hasher.make ~verbose flags n_symbols hasher f in
      let timer_id_iterator = Tools.Timer.of_string "KMers.Iterator.Iterator" in
      (fun ?(weight = 1.) s ->
        Tools.Timer.start timer_id_iterator;
        linter s |>
          (fun s ->
            encoder s |>
              List.iter (accumulator ~weight));
        Tools.Timer.stop timer_id_iterator),
      finalizer
  end

(* Levenshtein balls: every k-mer within a given number of edits of a centre, which is how a k-mer
   index tolerates errors.  The ball is walked outwards one edit at a time -- a substitution, a
   deletion pulling a base in from the context on either side, an insertion pushing one out at
   either end -- and every string met is emitted, the centre included, a ball being everything
   WITHIN its radius; which is why the iterators repeat themselves.
   As for [Hash_t], there is one interface and an implementation per kind of hash:
   [IntDNALevenshteinBall] gives machine integers, and hence holds k-mers of at most 30 bases, and
   [IntZDNALevenshteinBall] gives [IntZ.t], for any k.  Both walk a centre and a radius of up to 30
   characters on integers -- the bases at 2 bits each, the first most significant as in
   [H.encode], and a mask with a bit set for every character that is a base -- where an edit is a
   few shifts; longer centres and wider radii are walked on a buffer of one byte per character,
   step by step as the implementation on strings did *)
module type DNALevenshteinBall_t =
  sig
    module H:
      sig
        type t
        val k: int
        val alphabet: string
        val encode: string -> t
        (* The k characters of the string starting at the given position, taken where they are
           rather than copied out first *)
        val encode_at: string -> int -> t
        val encode_char: char -> int
      end
    (* Iterators all have repetitions *)
    val iter: ?radius:int -> (string -> unit) -> string -> string -> string -> unit
    val iterh: ?radius:int -> (H.t -> unit) -> string -> string -> string -> unit
    val iterk: ?radius:int -> (string -> unit) -> string -> unit
    val iterkh: ?radius:int -> (H.t -> unit) -> string -> unit
    (* Constructors are repeat-free *)
    module Base = StringSet
    type t = Base.t
    val make: ?radius:int -> string -> string -> string -> t
    val makek: ?radius:int -> string -> t
  end
(* What a ball needs of its hashes: the largest k they hold, and how to extend one by a base, the
   bases already there becoming 2 bits more significant *)
module type DNALevenshteinHash_t =
  sig
    type t
    val max_k: int
    val zero: t
    val of_int: int -> t
    val add_base: t -> int -> t
  end
module MakeDNALevenshteinBall (Hash: DNALevenshteinHash_t) (K: IntParameter_t):
    DNALevenshteinBall_t with type H.t = Hash.t =
  struct
    module H =
      struct
        type t = Hash.t
        let k =
          if K.n > Hash.max_k then
            Exception.raise __FUNCTION__ Initialize
              (Printf.sprintf "Invalid argument (k must be <= %d, found %d)" Hash.max_k K.n);
          K.n
        (* There are 4 symbols in the alphabet, each one encoded as a 2-bit number *)
        let alphabet = "ACGT"
        let encode_char = function
          | 'A' | 'a' -> 0
          | 'C' | 'c' -> 1
          | 'G' | 'g' -> 2
          | 'T' | 't' -> 3
          | _ -> -1
        let encode_at s pos =
          if pos < 0 || pos + k > String.length s then
            Exception.raise __FUNCTION__ Initialize
              (Printf.sprintf
                "Invalid argument (a k-mer of length k=%d at position %d falls outside a string \
                 of length %d)"
                k pos (String.length s));
          let res = ref Hash.zero in
          for i = pos to pos + k - 1 do
            let c = s.[i] in
            let code = encode_char c in
            if code < 0 then
              Exception.raise __FUNCTION__ Initialize
                (Printf.sprintf "Invalid argument (expected character in [ACGTacgt], found '%c')"
                  c);
            res := Hash.add_base !res code
          done;
          !res
        let encode s =
          if String.length s <> k then
            Exception.raise __FUNCTION__ Initialize
              (Printf.sprintf "Invalid argument (string length must be k=%d, found %d)" k
                (String.length s));
          encode_at s 0
      end
    (* [n] fields of [b] bits each packed into an integer, the first the most significant.
       [low b n] masks the last [n] of them *)
    let low b n = (1 lsl (b * n)) - 1
    let get b n x j = (x lsr (b * (n - 1 - j))) land low b 1
    let set b n x j y =
      let shift = b * (n - 1 - j) in
      x land lnot (low b 1 lsl shift) lor (y lsl shift)
    (* Field [j] taken out, and [y] added at the end *)
    let delete_append b n x j y =
      (x lsr (b * (n - j))) lsl (b * (n - j)) lor ((x land low b (n - 1 - j)) lsl b) lor y
    (* Field [j] taken out, and [y] added at the start *)
    let delete_prepend b n x j y =
      y lsl (b * (n - 1)) lor ((x lsr (b * (n - j))) lsl (b * (n - 1 - j)))
      lor (x land low b (n - 1 - j))
    (* [y] put in at [j], the last field falling off the end *)
    let insert_drop_last b n x j y =
      (x lsr (b * (n - j))) lsl (b * (n - j)) lor (y lsl (b * (n - 1 - j)))
      lor ((x land low b (n - j)) lsr b)
    (* The first field falling off the start, and [y] put in after what was field [j + 1] *)
    let drop_first_insert b n x j y =
      ((x lsr (b * (n - 2 - j))) land low b (j + 1)) lsl (b * (n - 1 - j))
      lor (y lsl (b * (n - 2 - j))) lor (x land low b (n - 2 - j))
    (* The bases and the mask of the [n] characters of [s] starting at [lo], those lying outside
       [s] counting as characters that are not bases *)
    let pack s lo n =
      let l = String.length s and bases = ref 0 and mask = ref 0 in
      for i = lo to lo + n - 1 do
        let code = if i >= 0 && i < l then H.encode_char s.[i] else -1 in
        bases := !bases lsl 2 lor (if code < 0 then 0 else code);
        mask := !mask lsl 1 lor (if code < 0 then 0 else 1)
      done;
      !bases, !mask
    let decode n bases mask =
      String.init n (fun j -> if get 1 n mask j = 1 then H.alphabet.[get 2 n bases j] else ' ')
    (* The walk over the ball of [radius] around a centre of [n] characters, [emit] being called
       with the bases and the mask of every string met.  The contexts are the [radius] characters
       on either side of the centre, which deletions pull in and insertions push out into *)
    let walk ~radius n emit l_bases l_mask bases mask r_bases r_mask =
      let rec expand level l_bases l_mask bases mask r_bases r_mask =
        emit bases mask;
        if level > 0 then begin
          let level = level - 1 in
          (* Mismatches.  A character that is not a base is replaced by all four *)
          for j = 0 to n - 1 do
            let base = get 2 n bases j and is_base = get 1 n mask j = 1 in
            for b = 0 to 3 do
              if not is_base || b <> base then
                expand level l_bases l_mask (set 2 n bases j b) (set 1 n mask j 1) r_bases r_mask
            done
          done;
          (* Deletions, the right context moving in and then the left one *)
          for j = 0 to n - 1 do
            if get 1 radius r_mask 0 = 1 then
              expand level l_bases l_mask (delete_append 2 n bases j (get 2 radius r_bases 0))
                (delete_append 1 n mask j 1) (delete_append 2 radius r_bases 0 0)
                (delete_append 1 radius r_mask 0 0);
            if get 1 radius l_mask (radius - 1) = 1 then
              expand level (delete_prepend 2 radius l_bases (radius - 1) 0)
                (delete_prepend 1 radius l_mask (radius - 1) 0)
                (delete_prepend 2 n bases j (get 2 radius l_bases (radius - 1)))
                (delete_prepend 1 n mask j 1) r_bases r_mask
          done;
          (* Insertions, the last base falling off the end and then the first one moving into the
             left context *)
          for j = 0 to n - 2 do
            for b = 0 to 3 do
              expand level l_bases l_mask (insert_drop_last 2 n bases j b)
                (insert_drop_last 1 n mask j 1) r_bases r_mask
            done;
            let l_bases = delete_append 2 radius l_bases 0 (get 2 n bases 0)
            and l_mask = delete_append 1 radius l_mask 0 (get 1 n mask 0) in
            for b = 0 to 3 do
              expand level l_bases l_mask (drop_first_insert 2 n bases j b)
                (drop_first_insert 1 n mask j 1) r_bases r_mask
            done
          done
        end in
      expand radius l_bases l_mask bases mask r_bases r_mask
    (* A centre too long, or a radius too wide, for an integer is walked on a buffer of one byte
       per character, 0 to 3 for a base and 4 for a character that is not one, holding the left
       context, the centre and the right context; the walk follows the implementation on strings
       step by step, copying the buffer before editing it *)
    let not_base = '\004'
    let buffer s lo n =
      let l = String.length s in
      Bytes.init n
        (fun j ->
          let i = lo + j in
          let code = if i >= 0 && i < l then H.encode_char s.[i] else -1 in
          if code < 0 then not_base else Char.unsafe_chr code)
    let walk_buffer ~radius n emit centre_and_contexts =
      let hi = radius + n - 1 in
      let last = hi + radius in
      let rec expand level s =
        emit s;
        if level > 0 then begin
          let level = level - 1 and s = Bytes.copy s in
          (* Mismatches.  A character that is not a base is replaced by all four *)
          for i = radius to hi do
            let c = Bytes.get s i in
            for b = 0 to 3 do
              let base = Char.unsafe_chr b in
              if base <> c then begin
                Bytes.set s i base;
                expand level s
              end
            done;
            Bytes.set s i c
          done;
          (* Deletions, the right context moving in and then the left one *)
          for i = radius to hi do
            let c = Bytes.get s i in
            let l = last - i in
            Bytes.blit s (i + 1) s i l;
            Bytes.set s last not_base;
            if Bytes.get s hi <> not_base then
              expand level s;
            Bytes.blit s i s (i + 1) l;
            Bytes.set s i c;
            Bytes.blit s 0 s 1 i;
            Bytes.set s 0 not_base;
            if Bytes.get s radius <> not_base then
              expand level s;
            Bytes.blit s 1 s 0 i;
            Bytes.set s i c
          done;
          (* Insertions, the last base falling off the end and then the first one moving into the
             left context *)
          for i = radius to hi - 1 do
            let c = Bytes.get s hi and l = hi - i in
            Bytes.blit s i s (i + 1) l;
            for b = 0 to 3 do
              Bytes.set s i (Char.unsafe_chr b);
              expand level s
            done;
            Bytes.blit s (i + 1) s i l;
            Bytes.set s hi c;
            let c = Bytes.get s 0 and l = i + 1 in
            Bytes.blit s 1 s 0 l;
            for b = 0 to 3 do
              Bytes.set s l (Char.unsafe_chr b);
              expand level s
            done;
            Bytes.blit s 0 s 1 l;
            Bytes.set s 0 c
          done
        end in
      expand radius centre_and_contexts
    let string_of_buffer ~radius n s =
      String.init n
        (fun j ->
          let c = Bytes.get s (radius + j) in
          if c = not_base then ' ' else H.alphabet.[Char.code c])
    let hash_of_buffer ~radius f s =
      let res = ref Hash.zero and is_kmer = ref true and j = ref 0 in
      while !is_kmer && !j < H.k do
        let c = Bytes.get s (radius + !j) in
        if c = not_base then
          is_kmer := false
        else
          res := Hash.add_base !res (Char.code c);
        incr j
      done;
      if !is_kmer then
        f !res
    (* The walk around a centre given as a string with its contexts, a context shorter than the
       radius being padded with characters that are not bases and a longer one trimmed *)
    let walk_strings ~radius emit_packed emit_buffer l_ctxt s r_ctxt =
      if radius < 0 then
        Exception.raise __FUNCTION__ Algorithm (Printf.sprintf "Invalid radius %d" radius);
      let n = String.length s and l_from = String.length l_ctxt - radius in
      if n <= 30 && radius <= 30 then begin
        let l_bases, l_mask = pack l_ctxt l_from radius
        and bases, mask = pack s 0 n and r_bases, r_mask = pack r_ctxt 0 radius in
        walk ~radius n emit_packed l_bases l_mask bases mask r_bases r_mask
      end else begin
        let l_buffer = buffer l_ctxt l_from radius and r_buffer = buffer r_ctxt 0 radius in
        Bytes.concat Bytes.empty [ l_buffer; buffer s 0 n; r_buffer ]
          |> walk_buffer ~radius n emit_buffer
      end
    let iter ?(radius = 1) f l_ctxt s r_ctxt =
      let n = String.length s in
      walk_strings ~radius (fun bases mask -> decode n bases mask |> f)
        (fun b -> string_of_buffer ~radius n b |> f) l_ctxt s r_ctxt
    let iterh ?(radius = 1) f l_ctxt s r_ctxt =
      (* Only a centre of length k has a hash *)
      if String.length s = H.k then begin
        let full = if H.k <= 30 then low 1 H.k else 0 in
        walk_strings ~radius (fun bases mask -> if mask = full then f (Hash.of_int bases))
          (hash_of_buffer ~radius f) l_ctxt s r_ctxt
      end else if radius < 0 then
        Exception.raise __FUNCTION__ Algorithm (Printf.sprintf "Invalid radius %d" radius)
    (* The walk around every k-mer of [s] in turn: within 30 bases its bases and mask are rolled
       along [s], and its contexts are what lies around it there *)
    let walk_kmers ~radius emit_packed emit_buffer s =
      if radius < 0 then
        Exception.raise __FUNCTION__ Algorithm (Printf.sprintf "Invalid radius %d" radius);
      let k = H.k and l = String.length s in
      if k <= 30 && radius <= 30 then begin
        let all_bases = low 2 k and full = low 1 k and bases = ref 0 and mask = ref 0 in
        String.iteri
          (fun i c ->
            let code = H.encode_char c in
            bases := (!bases lsl 2 lor (if code < 0 then 0 else code)) land all_bases;
            mask := (!mask lsl 1 lor (if code < 0 then 0 else 1)) land full;
            if i >= k - 1 then
              if radius = 0 then
                emit_packed !bases !mask
              else begin
                let l_bases, l_mask = pack s (i - k + 1 - radius) radius
                and r_bases, r_mask = pack s (i + 1) radius in
                walk ~radius k emit_packed l_bases l_mask !bases !mask r_bases r_mask
              end)
          s
      end else
        for lo = 0 to l - k do
          buffer s (lo - radius) (k + 2 * radius) |> walk_buffer ~radius k emit_buffer
        done
    let iterk ?(radius = 1) f s =
      walk_kmers ~radius (fun bases mask -> decode H.k bases mask |> f)
        (fun b -> string_of_buffer ~radius H.k b |> f) s
    let iterkh ?(radius = 1) f s =
      let full = if H.k <= 30 then low 1 H.k else 0 in
      walk_kmers ~radius (fun bases mask -> if mask = full then f (Hash.of_int bases))
        (hash_of_buffer ~radius f) s
    module Base = StringSet
    type t = Base.t
    let make ?(radius = 1) l_ctxt s r_ctxt =
      let res = ref Base.empty in
      iter ~radius
        (fun s ->
          res := Base.add s !res)
        l_ctxt s r_ctxt;
      !res
    let makek ?(radius = 1) s =
      let res = ref Base.empty in
      iterk ~radius
        (fun s ->
          res := Base.add s !res)
        s;
      !res
  end
(* Balls whose hashes are machine integers, and hence hold k-mers of at most 30 bases *)
module IntDNALevenshteinBall (K: IntParameter_t): DNALevenshteinBall_t with type H.t = int =
  MakeDNALevenshteinBall
    (struct
      type t = int
      let max_k = 30
      let zero = 0
      let of_int h = h
      let add_base h base = h lsl 2 lor base
    end)
    (K)
(* Balls whose hashes are [IntZ.t], and hence hold k-mers of any length *)
module IntZDNALevenshteinBall (K: IntParameter_t): DNALevenshteinBall_t with type H.t = IntZ.t =
  MakeDNALevenshteinBall
    (struct
      type t = IntZ.t
      let max_k = max_int
      let zero = IntZ.zero
      let of_int = IntZ.of_int
      let add_base h base = IntZ.((h lsl 2) lor of_int base)
    end)
    (K)
(* The name existing code knows the machine-integer balls by *)
module DNALevenshteinBall = IntDNALevenshteinBall

