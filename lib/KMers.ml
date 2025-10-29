(*
    KMers.ml -- (c) 2020-2024 Paolo Ribeca, <paolo.ribeca@gmail.com>

    This file is part of BiOCamLib, the OCaml foundations upon which
    a number of the bioinformatics tools I developed are built.

    KMers.ml implements tools to iterate over, and hash, k-mers.

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
        (Accumulator1 for regular k-mers, Accumulator2 for gapped k-mers) *)
    module Accumulator1: Hashtbl.S with type key = t
    module Accumulator2: Hashtbl.S with type key = t * t
    (* Return the hash as a string, for instance in hex form *)
    val to_string: t -> string
  end
module IntHash (Bits: IntParameter_t) (K: IntParameter_t): Hash_t with type t = int =
  struct
    type t = int
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
    let symbol_complement s = max_symbol - s
    let compute a idx =
      let l = Array.length a in
      if idx + k > l then
        Exception.raise __FUNCTION__ Algorithm
          (Printf.sprintf "Invalid index value %d (string length=%d, k-mer size=%d)" idx l k);
      let res = ref 0 in
      for i = idx to idx + k - 1 do
        let s = a.(i) in
        if s > max_symbol then
          Exception.raise __FUNCTION__ Algorithm (Printf.sprintf "Invalid symbol '%d' in input" s);
        res := (!res lsl bits) + s
      done;
      !res
    let add_symbol_right h s = ((h lsl bits) lor s) land mask
    let left_shift = bits * (k - 1)
    let add_symbol_left h s = (s lsl left_shift) lor (h lsr bits)
    let rc h =
      let h = ref h and res = ref 0 in
      for _ = 1 to k do
        res := (!res lsl bits) + (max_symbol - (!h land max_symbol));
        h := !h lsr bits
      done;
      !res
    module Accumulator1 = IntHashtbl
    type tt = t * t
    module Accumulator2 = Hashtbl.Make (MakeHashable (struct type t = tt end))
    let hex_format = Scanf.format_from_string (Printf.sprintf "%%0%dx" ((bits * k + 3) / 4)) "%d"
    let to_string = Printf.sprintf hex_format
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
          Exception.raise __FUNCTION__ Algorithm (Printf.sprintf "Invalid symbol '%d' in input" s);
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
    type tt = t * t
    module Accumulator2 = Hashtbl.Make (MakeHashable (struct type t = tt end))
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
             to weigh k-mers based on coverage.
           The finaliser applies the iterator to the hashes accumulated so far
            and deallocates storage, pretty much as what happens when flushing *)
        val make: ?max_results_size:int -> ?verbose:bool ->
                  Content.Flags.t -> int -> t -> (string -> int -> unit) ->
                  (?weight:int -> int array -> unit) * (unit -> unit)
      end
    type t = ?weight:int -> string -> unit
    (* The last argument is the iterator function *)
    val make: ?max_results_size:int -> ?verbose:bool ->
              Content.t -> Hasher.t -> (string -> int -> unit) -> t
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
            (fun s -> s),
            { unknown_char_action; rc_symmetric_hash = false }
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
        let make ?(verbose = false) flags e =
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
                  (fun s -> s)
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
          let trie = !trie in
          (*let timer_id_encoder = Tools.Timer.of_string "KMers.Iterator.Encoder:encode"
          and timer_id_trie = Tools.Timer.of_string "KMers.Iterator.Encoder:trie"
          and timer_id_array = Tools.Timer.of_string "KMers.Iterator.Encoder:array" in*)
          Tools.Trie.length trie,
          (fun s ->
            (*Tools.Timer.start timer_id_encoder;*)
            let l_src = String.length s in
            let current = Array.make l_src 0
            and i_src = ref 0 and l_dst = ref 0 and res = ref [] in
            let add_current_to_res () =
              if !l_dst > 0 then begin
                Array.sub current 0 !l_dst |> List.accum res;
                l_dst := 0
              end in
            while !i_src < l_src do
              (*Tools.Timer.start timer_id_trie;*)
              let n, id = Tools.Trie.longest_match trie s !i_src in
              (*Tools.Timer.stop timer_id_trie;*)
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
                (*Tools.Timer.start timer_id_array;*)
                current.(!l_dst) <- id;
                (*Tools.Timer.stop timer_id_array;*)
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
            (*Tools.Timer.stop timer_id_encoder;*)
            !res)
      end
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
        let make ?(max_results_size = 0) ?(verbose = false) flags n_symbols h f =
          let impl =
            let n_bits =
              assert (n_symbols > 0);
              let res = ref 1 and rem = n_symbols - 1 |> ref in
              while rem := !rem lsr 1; !rem > 0 do
                incr res
              done;
              !res
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
            let res = Impl.Accumulator1.create 1024 and cntr = ref 0 in
            let finalizer () =
              let full = Impl.Accumulator1.length res > max_results_size in
              if verbose && full then
                Printf.eprintf "%s\r(%s): Maximum size (%d) reached. Outputting and removing hashes...%!"
                  String.TermIO.clear __FUNCTION__ max_results_size;
              Impl.Accumulator1.iter
                (fun h n ->
                  f (Impl.to_string h) !n)
                res;
              Impl.Accumulator1.reset res;
              if verbose && full then
                Printf.eprintf " done.\n%!" in
            let add h w =
              incr cntr;
              if max_results_size > 0 && !cntr mod 1000 = 0 && Impl.Accumulator1.length res > max_results_size then
                finalizer ();
              match Impl.Accumulator1.find_opt res h with
              | None ->
                ref w |> Impl.Accumulator1.add res h
              | Some n ->
                n := !n + w in
            (*let timer_id_accumulate = Tools.Timer.of_string "KMers.Iterator.Encoder:accumulate" in*)
            (* Accumulator *)
            (fun ?(weight = 1) ia ->
              (*Tools.Timer.start timer_id_accumulate;*)
              let l = Array.length ia in
              if l >= k then begin
                let current = Impl.compute ia 0 |> ref in
                let rc = Impl.rc !current |> ref in
                if flags.Content.Flags.rc_symmetric_hash then
                  add (min !current !rc) weight
                else
                  add !current weight;
                for i = k to l - 1 do
                  current := Impl.add_symbol_right !current ia.(i);
                  if flags.rc_symmetric_hash then begin
                    rc := Impl.symbol_complement ia.(i) |> Impl.add_symbol_left !rc;
                    add (min !current !rc) weight
                  end else
                    add !current weight;
                done
              end;
              (*Tools.Timer.stop timer_id_accumulate*)),
            (* Finaliser *)
            finalizer
          | Gapped (k, g) ->
            let res = Impl.Accumulator2.create 1024 and cntr = ref 0 in
            let finalizer () =
              let full = Impl.Accumulator2.length res > max_results_size in
              if verbose && full then
                Printf.eprintf "%s\r(%s): Maximum size (%d) reached. Outputting and removing hashes...%!"
                  String.TermIO.clear __FUNCTION__ max_results_size;
              Impl.Accumulator2.iter
                (fun (h1, h2) n ->
                  f (Impl.to_string h1 ^ "_" ^ Impl.to_string h2) !n)
                res;
              Impl.Accumulator2.reset res;
              if verbose && full then
                Printf.eprintf " done.\n%!" in
            let add hh w =
              incr cntr;
              if max_results_size > 0 && !cntr mod 1000 = 0 && Impl.Accumulator2.length res > max_results_size then
                finalizer ();
              match Impl.Accumulator2.find_opt res hh with
              | None ->
                ref w |> Impl.Accumulator2.add res hh
              | Some n ->
                n := !n + w in
            (* Here we just have to simulate a longer k *)
            let eff_k = 2 * k + g and offs = k + g in
            (* Accumulator *)
            (fun ?(weight = 1) ia ->
              let l = Array.length ia in
              if l >= eff_k then begin
                let current1 = Impl.compute ia 0 |> ref
                and current2 = Impl.compute ia offs |> ref in
                let rc1 = Impl.rc !current1 |> ref
                and rc2 = Impl.rc !current2 |> ref in
                let get_min () =
                  let h1 = !current1 and h2 = !current2 and rc1 = !rc1 and rc2 = !rc2 in
                  (* Here the equation is (h1|h2) <= rc(h1|h2) = rc(h2)|rc(h1) *)
                  if h1 < rc2 || (h1 = rc2 && h2 <= rc1) then
                    h1, h2
                  else
                    rc2, rc1 in
                if flags.rc_symmetric_hash then
                  add (get_min ()) weight
                else
                  add (!current1, !current2) weight;
                for i = eff_k to l - 1 do
                  let c1 = ia.(i - offs) and c2 = ia.(i) in
                  current1 := Impl.add_symbol_right !current1 c1;
                  current2 := Impl.add_symbol_right !current2 c2;
                  if flags.rc_symmetric_hash then begin
                    rc1 := Impl.symbol_complement c1 |> Impl.add_symbol_left !rc1;
                    rc2 := Impl.symbol_complement c2 |> Impl.add_symbol_left !rc2;
                    add (get_min ()) weight
                  end else
                    add (!current1, !current2) weight
                done
              end),
            (* Finaliser *)
            finalizer
      end
    type t = ?weight:int -> string -> unit
    let make ?(max_results_size = 0) ?(verbose = false) content hasher f =
      let encoder = Encoder.of_content content
      and content, flags = Content.make content in
      let n_symbols, encoder = Encoder.make ~verbose flags encoder in
      let accumulator, finalizer =
        Hasher.make ~max_results_size ~verbose flags n_symbols hasher f in
      (fun ?(weight = 1) s ->
        content s |>
          (fun s ->
            encoder s |>
              List.iter (accumulator ~weight));
        finalizer ())
  end

(* TODO: THIS ONE SHOULD PROBABLY BE REWRITTEN *)
module DNALevenshteinBall (K: IntParameter_t):
  sig
    module H:
      sig
        type t = int
        val k: int
        val alphabet: string
        val encode: string -> t
        val encode_char: char -> t
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
= struct
    module H =
      struct
        type t = int
        let k =
          if K.n > 30 then
            Exception.raise __FUNCTION__ Initialize
              (Printf.sprintf "Invalid argument (k must be <= 30, found %d)" K.n);
          K.n
        (* There are 4 symbols in the alphabet, each one encoded as a 2-bit number *)
        let alphabet = "ACGT"
        let encode_char = function
          | 'A' | 'a' -> 0
          | 'C' | 'c' -> 1
          | 'G' | 'g' -> 2
          | 'T' | 't' -> 3
          | _ -> -1
        let encode s =
          if String.length s <> k then
            Exception.raise __FUNCTION__ Initialize
              (Printf.sprintf "Invalid argument (string length must be k=%d, found %d)" k (String.length s));
          let res = ref 0 in
          for i = 0 to k - 1 do
            res :=
              !res lsl 2 +
                match s.[i] with
                | 'A' | 'a' -> 0
                | 'C' | 'c' -> 1
                | 'G' | 'g' -> 2
                | 'T' | 't' -> 3
                | c ->
                  Exception.raise __FUNCTION__ Initialize
                    (Printf.sprintf "Invalid argument (expected character in [ACGTacgt], found '%c')" c);
          done;
          !res
      end
    let lint s =
      (* This is not entirely general, but OK for the time being *)
      let s = String.uppercase_ascii s |> Bytes.of_string
      and encode = H.encode_char in
      Bytes.iteri
        (fun i c ->
          Bytes.(
            s.@(i) <-
              if encode c = -1 then
                ' '
              else
                c
          ))
        s;
      Bytes.to_string s
    let iter ?(radius = 1) f l_ctxt s r_ctxt =
      if radius < 0 then
        Exception.raise __FUNCTION__ Algorithm (Printf.sprintf "Invalid radius %d" radius);
      let l_ctxt, s, r_ctxt = lint l_ctxt, lint s, lint r_ctxt in
      (* We trim/pad contexts whenever needed *)
      let padding = String.make radius ' ' in
      let l_ctxt = String.sub (padding ^ l_ctxt) (String.length l_ctxt) radius
      and r_ctxt = String.sub (r_ctxt ^ padding) 0 radius in
      (* The string also includes left and right contexts *)
      let len = String.length s in
      let hi = radius + len - 1 in
      let last = hi + radius in
      let rec expand level orig_s =
        let open Bytes in
        if level = 0 then
          (* We eliminate contexts *)
          String.sub orig_s radius len |> f
        else begin
          let s = of_string orig_s in
          (* Mismatches *)
          for i = radius to hi do
            let c = s.@(i) in
            String.iter
              (fun cc ->
                if cc <> c then begin
                  s.@(i) <- cc;
                  to_string s |> expand (level - 1)
                end)
              H.alphabet;
            (* We restore the previous state *)
            s.@(i) <- c
          done;
          (* Deletions *)
          for i = radius to hi do
            let c = s.@(i) in
            (* Right-to-left deletion *)
            let l = last - i in
            blit s (i + 1) s i l;
            s.@(last) <- ' '; (* Padding *)
            if s.@(hi) <> ' ' then
              to_string s |> expand (level - 1);
            (* We restore the previous state *)
            blit s i s (i + 1) l;
            s.@(i) <- c;
            (* Left-to-right deletion *)
            blit s 0 s 1 i;
            s.@(0) <- ' '; (* Padding *)
            if s.@(radius) <> ' ' then
              to_string s |> expand (level - 1);
            (* We restore the previous state *)
            blit s 1 s 0 i;
            s.@(i) <- c
          done;
          (* Insertions *)
          for i = radius to hi - 1 do
            (* Left-to-right insertion *)
            let c = s.@(hi) in
            let l = hi - i in
            blit s i s (i + 1) l;
            String.iter
              (fun cc ->
                s.@(i) <- cc;
                to_string s |> expand (level - 1))
              H.alphabet;
            (* We restore the previous state *)
            blit s (i + 1) s i l;
            s.@(hi) <- c;
            (* Right-to-left insertion *)
            let c = s.@(0) in
            let l = i + 1 in
            blit s 1 s 0 l;
            String.iter
              (fun cc ->
                s.@(l) <- cc;
                to_string s |> expand (level - 1))
              H.alphabet;
            (* We restore the previous state *)
            blit s 0 s 1 l;
            s.@(0) <- c
          done;
          assert (to_string s = orig_s)
        end in
      (* The k-mer itself gets inserted here *)
      l_ctxt ^ s ^ r_ctxt |> expand radius
    let iterh ?(radius = 1) f =
      iter ~radius
        (fun s ->
          try
            H.encode s |> f
          with _ ->
            ())
    let iterk ?(radius = 1) f s =
      (* We begin by replacing non-alphabet characters with spaces,
          to be compatible with the conventions used by make() above *)
      let l = String.length s in
      for lo = 0 to l - H.k do
        iter ~radius f begin
          let ctxt_lo = (lo - radius) |> max 0 in
          String.sub s ctxt_lo (lo - ctxt_lo)
        end begin
          String.sub s lo H.k
        end begin
          let hi = lo + H.k in
          let ctxt_hi = (hi + radius) |> min l in
          String.sub s hi (ctxt_hi - hi)
        end
      done
    let iterkh ?(radius = 1) f =
      iterk ~radius
        (fun s ->
          try
            H.encode s |> f
          with _ ->
            ())
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

