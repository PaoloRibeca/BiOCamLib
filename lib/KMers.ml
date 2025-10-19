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
    (* Emitted when number of symbol bits and/or value of k are not OK *)
    exception Invalid_bits of int
    exception Invalid_k of int
    exception Invalid_initializers of int * int
    (* Compute the hash for an encoded k-mer.
       Arguments are encoded vector and 0-based starting index.
       There must be at least k symbols left from there to the end of the vector,
        and the value of the symbol must not exceed the number of bits *)
    exception Invalid_index of int * int * int
    exception Invalid_symbol of int
    val compute: int array -> int -> t
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
    exception Invalid_bits of int
    let bits =
      (* We need one additional bit to be able to compute the mask *)
      if Bits.n < 1 || Bits.n >= Sys.int_size then
        Invalid_bits Bits.n |> raise;
      Bits.n
    exception Invalid_k of int
    exception Invalid_initializers of int * int
    let k =
      if K.n < 1 then
        Invalid_k K.n |> raise;
      if K.n * Bits.n > Sys.int_size then
        Invalid_initializers (Bits.n, K.n) |> raise;
      K.n
    let mask = 1 lsl (bits * k) - 1
    let max_symbol = 1 lsl bits - 1
    let symbol_complement s = max_symbol - s
    exception Invalid_index of int * int * int
    exception Invalid_symbol of int
    let compute a idx =
      let l = Array.length a in
      if idx + k > l then
        Invalid_index (idx, l, k) |> raise;
      let res = ref 0 in
      for i = idx to idx + k - 1 do
        let s = a.(i) in
        if s > max_symbol then
          Invalid_symbol s |> raise;
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
    exception Invalid_bits of int
    let bits =
      if Bits.n < 1 then
        Invalid_bits Bits.n |> raise;
      Bits.n
    exception Invalid_k of int
    exception Invalid_initializers of int * int
    let k =
      if K.n < 1 then
        Invalid_k K.n |> raise;
      K.n
    let mask =
      let bits_times_k = bits * k in
      IntZ.(one lsl bits_times_k - one)
    let max_symbol = 1 lsl bits - 1
    let symbol_complement s = max_symbol - s
    let max_symbol_z = IntZ.of_int max_symbol
    exception Invalid_index of int * int * int
    exception Invalid_symbol of int
    let compute a idx =
      let l = Array.length a in
      if idx + k > l then
        Invalid_index (idx, l, k) |> raise;
      let res = ref IntZ.zero in
      for i = idx to idx + k - 1 do
        let s = a.(i) in
        if s > max_symbol then
          Invalid_symbol s |> raise;
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
    (DNA, protein, ...) turns the original input string into a set of strings
    (for instance, the sequence and its reverse complement). This step might include
    checks on the sequence, for instance linting. Also this step produces flags for
    a filter that will be applied to all the hashes prior to iteration, for instance
    to remove duplicated DNA k-mers coming from reverse complementing the string.
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
        type t =
          | DNA_ss
          | DNA_ds
          | Protein
          | Text
        exception Unknown of string
        val of_string: string -> t
        val to_string: t -> string
        module Flags:
          sig
            type t = {
              rc_add: bool;
              rc_deduplicate_k_mers: bool
            }
          end
        (* The results are adaptor function and flags *)
        val make: t -> (string -> string) * Flags.t
      end
    module Encoder:
      sig
        type t =
          | DNA
          | Protein
          | Dictionary of string (* File path *)
          | Test of string list (* No constructor from string - not really used in production *)
          exception Unknown of string
        val of_string: string -> t
        val to_string: t -> string
        (* The integer is the alphabet size *)
        val make: ?verbose:bool -> t -> int * (string -> int array list)
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
              Content.t -> Encoder.t -> Hasher.t -> (string -> int -> unit) -> t
  end
= struct
    module Content =
      struct
        type t =
          | DNA_ss
          | DNA_ds
          | Protein
          | Text
        exception Unknown of string
        let of_string = function
          | "DNA-ss" | "DNA-single-stranded" -> DNA_ss
          | "DNA-ds" | "DNA-double-stranded" -> DNA_ds
          | "protein" -> Protein
          | "text" -> Text
          | w ->
            Unknown w |> raise
        let to_string = function
          | DNA_ss -> "DNA-ss"
          | DNA_ds -> "DNA-ds"
          | Protein -> "protein"
          | Text -> "text"
        (* Note that in what follows we _do_ want to turn lowercase characters
            into uppercase ones, or the encoding might subsequently fail -
            unless we doubled the number of bits needed for the encoding,
            which would not be optimal.
           As for dashes, we could as well leave them rather than turning them
            into unknowns, but they would be stripped out later on anyway *)
          module Flags =
            struct
              type t = {
                rc_add: bool;
                rc_deduplicate_k_mers: bool
              }
            end
          let make = function
          | DNA_ss ->
            (Sequences.Lint.dnaize ~keep_lowercase:false ~keep_dashes:false),
            { Flags.rc_add = false; rc_deduplicate_k_mers = false }
          | DNA_ds ->
            (Sequences.Lint.dnaize ~keep_lowercase:false ~keep_dashes:false),
            { Flags.rc_add = true; rc_deduplicate_k_mers = true }
          | Protein ->
            (Sequences.Lint.proteinize ~keep_lowercase:false ~keep_dashes:false),
            { Flags.rc_add = false; rc_deduplicate_k_mers = false }
          | Text ->
            (fun s -> s),
            { Flags.rc_add = false; rc_deduplicate_k_mers = false }
      end
    module Encoder =
      struct
        type t =
          | DNA
          | Protein
          | Dictionary of string (* File path *)
          | Test of string list
          exception Unknown of string
        let of_string = function
          | "DNA" -> DNA
          | "protein" -> Protein
          | w ->
            begin match String.sub w 0 5 with
            | "dict:" | "file:" ->
              Dictionary (String.sub w 5 (String.length w - 5))
            | _ ->
              Unknown w |> raise
            end
        let to_string = function
          | DNA -> "DNA"
          | Protein -> "protein"
          | Dictionary w -> "dict:" ^ w
          | Test _ -> assert false
        let make ?(verbose = false) e =
          let dict =
            match e with
            | Test l ->
              l
            | DNA ->
              [ "A"; "C"; "G"; "T" ]
            | Protein ->
              [ "A"; "C"; "D"; "E"; "F"; "G"; "H"; "I"; "K"; "L"; "M";
                "N"; "O"; "P"; "Q"; "R"; "S"; "T"; "U"; "V"; "W"; "Y" ]
            | Dictionary filename ->
              let file = open_in filename and progr = ref 0 and dict = ref [] in
              if verbose then
                Printf.eprintf "(%s): Reading dictionary file '%s': Begin\n%!" __FUNCTION__ filename;
              begin try
                while true do
                  let line = input_line file in
                  if line <> "" then
                    List.accum dict line;
                  incr progr
                done
              with End_of_file ->
                close_in file;
                if verbose then
                  Printf.eprintf "(%s): Reading dictionary file '%s': End\n%!" __FUNCTION__ filename
              end;
              !dict in
          let trie = Tools.Trie.create () |> ref in
          List.iter
            (fun s ->
              trie := Tools.Trie.add !trie s)
            dict;
          let trie = !trie
          and timer_id_encoder = Tools.Timer.of_string "KMers.Iterator.Encoder:encode"
          and timer_id_trie = Tools.Timer.of_string "KMers.Iterator.Encoder:trie"
          and timer_id_stackarray = Tools.Timer.of_string "KMers.Iterator.Encoder:stackarray" in
          Tools.Trie.length trie,
          (fun s ->
            (*Tools.Timer.start timer_id_encoder;*)
            let l = String.length s and i = ref 0 and current = Tools.StackArray.create ()
            and res = ref [] in
            let add_current_to_res () =
              if Tools.StackArray.length current > 0 then begin
                Tools.StackArray.contents current |> List.accum res;
                Tools.StackArray.clear current
              end in
            while !i < l do
              (*Tools.Timer.start timer_id_trie;*)
              let n, id = Tools.Trie.longest_match trie s !i in
              (*Tools.Timer.stop timer_id_trie;*)
              if n = 0 then begin
                (* Case of no dictionary word found - we just split the string and skip one character *)
                add_current_to_res ();
                incr i
              end else begin
                (*Tools.Timer.start timer_id_stackarray;*)
                Tools.StackArray.push current id;
                (*Tools.Timer.stop timer_id_stackarray;*)
                i := !i + n
              end
            done;
            add_current_to_res ();
            (* Here we cull the StackArray in order not to leave it around unused *)
            Tools.StackArray.reset current;
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
        exception Unknown of string
        let of_string_re = Str.regexp "[(,)]"
        let of_string s =
          match Str.full_split of_string_re s with
          | [ Text "k-mers"; Delim "("; Text k; Delim ")" ] ->
            begin try
              K_mers (int_of_string k)
            with _ ->
              Unknown s |> raise
            end
          | [ Text "gapped"; Delim "("; Text k; Delim ","; Text gap_size; Delim ")" ] ->
            begin try
              Gapped (int_of_string k, int_of_string gap_size)
            with _ ->
              Unknown s |> raise
            end
          | _ ->
            Unknown s |> raise
        let to_string = function
          | K_mers k ->
            Printf.sprintf "k-mers(%d)" k
          | Gapped (k, g) ->
            Printf.sprintf "gapped(%d,%d)" k g
        let make ?(max_results_size = 0) ?(verbose = false) flags n_symbols h f =
          let impl =
            let n_bits =
              if n_symbols - 1 < 1 then
                0
              else begin
                let res = ref 0 and rem = ref n_symbols in
                while rem := !rem lsr 1; !rem > 0 do
                  incr res
                done;
                !res
              end
            and k = match h with K_mers k | Gapped (k, _) -> k in
            try
              (*Printf.printf "I am small (bits=%d, k=%d)\n%!" n_bits k;*)
              (module IntHash (struct let n = n_bits end) (struct let n = k end): Hash_t)
            with _ ->
              (*Printf.printf "I am large (bits=%d, k=%d)\n%!" n_bits k;*)
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
              let iterator =
                if flags.Content.Flags.rc_deduplicate_k_mers then
                  (fun h n ->
                    if h <= Impl.rc h then
                      f (Impl.to_string h) !n)
                else
                  (fun h n ->
                    f (Impl.to_string h) !n) in
              Impl.Accumulator1.iter iterator res;
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
            (* Accumulator *)
            (fun ?(weight = 1) ia ->
              let l = Array.length ia in
              if l >= k then begin
                let current = Impl.compute ia 0 |> ref in
                let rc = Impl.rc !current |> ref in
                add !current weight;
                if flags.rc_add then
                  add !rc weight;
                for i = k to l - 1 do
                  current := Impl.add_symbol_right !current ia.(i);
                  add !current weight;
                  if flags.rc_add then begin
                    rc := Impl.symbol_complement ia.(i) |> Impl.add_symbol_left !rc;
                    add !rc weight
                  end
                done
              end),
            (* Finaliser *)
            finalizer
          | Gapped (k, g) ->
            let res = Impl.Accumulator2.create 1024 and cntr = ref 0 in
            let finalizer () =
              let full = Impl.Accumulator2.length res > max_results_size in
              if verbose && full then
                Printf.eprintf "%s\r(%s): Maximum size (%d) reached. Outputting and removing hashes...%!"
                  String.TermIO.clear __FUNCTION__ max_results_size;
              let iterator =
                if flags.rc_deduplicate_k_mers then
                  (fun (h1, h2) n ->
                    (* Here the equation is (h1|h2) <= rc(h1|h2) = rc(h2)|rc(h1) *)
                    let rc_h2 = Impl.rc h2 in
                    if h1 < rc_h2 || (h1 = rc_h2 && h2 <= Impl.rc h1) then
                      f (Impl.to_string h1 ^ "_" ^ Impl.to_string h2) !n)
                else
                  (fun (h1, h2) n ->
                    f (Impl.to_string h1 ^ "_" ^ Impl.to_string h2) !n) in
              Impl.Accumulator2.iter iterator res;
              Impl.Accumulator2.reset res;
              if verbose && full then
                Printf.eprintf " done.\n%!" in
            let add h1 h2 w =
              incr cntr;
              if max_results_size > 0 && !cntr mod 1000 = 0 && Impl.Accumulator2.length res > max_results_size then
                finalizer ();
              let h = (h1, h2) in
              match Impl.Accumulator2.find_opt res h with
              | None ->
                ref w |> Impl.Accumulator2.add res h
              | Some n ->
                n := !n + w in
            (* Here we just have to simulate a longer k *)
            let eff_k = 2 * k + g and offs = k + g in
            (* Accumulator *)
            (fun ?(weight = 1) ia ->
              let l = Array.length ia in
              if l >= eff_k then begin
                let current_1 = Impl.compute ia 0 |> ref
                and current_2 = Impl.compute ia offs |> ref in
                add !current_1 !current_2 weight;
                let rc_1 = Impl.rc !current_1 |> ref
                and rc_2 = Impl.rc !current_2 |> ref in
                if flags.rc_add then
                  add !rc_1 !rc_2 weight;
                for i = eff_k to l - 1 do
                  current_1 := Impl.add_symbol_right !current_1 ia.(i - offs);
                  current_2 := Impl.add_symbol_right !current_2 ia.(i);
                  add !current_1 !current_2 weight;
                  if flags.rc_add then begin
                    rc_1 := Impl.symbol_complement ia.(i - offs) |> Impl.add_symbol_left !rc_1;
                    rc_2 := Impl.symbol_complement ia.(i) |> Impl.add_symbol_left !rc_2;
                    add !rc_1 !rc_2 weight
                  end
                done
              end),
            (* Finaliser *)
            finalizer
      end
    type t = ?weight:int -> string -> unit
    let make ?(max_results_size = 0) ?(verbose = false) content encoder hasher f =
      let content, flags = Content.make content
      and n_symbols, encoder = Encoder.make ~verbose encoder in
      let accumulator, finalizer =
        Hasher.make ~max_results_size ~verbose flags n_symbols hasher f in
      (fun ?(weight = 1) s ->
        content s |>
          (fun s ->
            encoder s |>
              List.iter (accumulator ~weight));
        finalizer ())
  end

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
            Printf.sprintf "(%s): Invalid argument (k must be <= 30, found %d)" __FUNCTION__ K.n |> failwith;
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
            Printf.sprintf "(%s): Invalid argument (string length must be k=%d, found %d)" __FUNCTION__ k (String.length s)
              |> failwith;
          let res = ref 0 in
          for i = 0 to k - 1 do
            res :=
              !res lsl 2 +
                match s.[i] with
                | 'A' | 'a' -> 0
                | 'C' | 'c' -> 1
                | 'G' | 'g' -> 2
                | 'T' | 't' -> 3
                | w ->
                  Printf.sprintf "(%s): Invalid argument (expected character in [ACGTacgt], found '%c')" __FUNCTION__ w
                    |> failwith
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
        Printf.sprintf "(%s): Invalid radius %d" __FUNCTION__ radius |> failwith;
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

