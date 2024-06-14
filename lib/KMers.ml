(*
    KMers.ml -- (c) 2020-2023 Paolo Ribeca, <paolo.ribeca@gmail.com>

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

(* Auxiliary module type to store and query frequencies of k-mers *)
module type HashFrequencies_t =
  sig
    type hash_t
    type t
    val create: int -> t
    val length: t -> int
    val clear: t -> unit
    (* The first parameter is the hash, the second its frequency *)
    val add: t -> hash_t -> int -> unit
    val iter: (hash_t -> int -> unit) -> t -> unit
  end
(* Implementation for integer hashes *)
module IntHashFrequencies: HashFrequencies_t with type hash_t = int
= struct
    type hash_t = int
    module H = IntHashtbl
    type t = int ref H.t
    let create = H.create
    let length = H.length
    let clear = H.clear
    let add hf key occs =
      match H.find_opt hf key with
      | None -> H.add hf key (ref occs)
      | Some r -> r := !r + occs
      [@@inline]
    let iter f = H.iter (fun key occs -> f key !occs) [@@inline]
  end

(* This base type doesn't have iterators *)
module type BaseHash_t =
  sig
    type t
    val k: int
    val alphabet: string
    val encode: string -> t
    val encode_char: (char -> t) -> char -> t
    val decode: t -> string
    val to_hex: t -> string
  end
(* The following one is the complete type *)
module type Hash_t =
  sig
    include BaseHash_t
    (* Iterates a function over all hashes that can be extracted from a string
        according to the specific method being used (for instance, in the case of DNA
        iteration can happen both on the string and its RC).
       Iteration is positional, i.e., k-mers can appear more than once *)
    type iter_t
    val iteri: (int -> iter_t -> unit) -> string -> unit
    (* Iteration with counter update *)
    module HashFrequencies: HashFrequencies_t
    val iterc: HashFrequencies.t -> string -> unit
  end
module type IntHash_t = Hash_t with type t = int and module HashFrequencies = IntHashFrequencies

module ProteinHash (K: IntParameter_t): IntHash_t with type iter_t = int
= struct
    type t = int
    let k =
      if K.n > 12 then
        Printf.sprintf "(%s): Invalid argument (k must be <= 12, found %d)" __FUNCTION__ K.n |> failwith;
      K.n
    (* There are 22 symbols in the alphabet, each one encoded as a 5-bit number *)
    let alphabet = "ACDEFGHIKLMNOPQRSTUVWY"
    let hex_format = Scanf.format_from_string (Printf.sprintf "%%0%dx" ((k * 5 + 3) / 4)) "%d"
    let to_hex = Printf.sprintf hex_format
    let encode_char err_f = function
      | 'A' | 'a' -> 0
      | 'C' | 'c' -> 1
      | 'D' | 'd' -> 2
      | 'E' | 'e' -> 3
      | 'F' | 'f' -> 4
      | 'G' | 'g' -> 5
      | 'H' | 'h' -> 6
      | 'I' | 'i' -> 7
      | 'K' | 'k' -> 8
      | 'L' | 'l' -> 9
      | 'M' | 'm' -> 10
      | 'N' | 'n' -> 11
      | 'O' | 'o' -> 12
      | 'P' | 'p' -> 13
      | 'Q' | 'q' -> 14
      | 'R' | 'r' -> 15
      | 'S' | 's' -> 16
      | 'T' | 't' -> 17
      | 'U' | 'u' -> 18
      | 'V' | 'v' -> 19
      | 'W' | 'w' -> 20
      | 'Y' | 'y' -> 21
      | w -> err_f w
    let encode s =
      if String.length s <> k then
        Printf.sprintf "(%s): Invalid argument (string length must be k=%d, found %d)" __FUNCTION__ k (String.length s)
          |> failwith;
      let res = ref 0 in
      for i = 0 to k - 1 do
        res :=
          !res lsl 5 +
          encode_char begin
            fun w ->
              Printf.sprintf "(%s): Invalid argument (expected character in [ACDEFGHIKLMNOPQRSTUVWYacdefghiklmnopqrstuvwy], found '%c')" __FUNCTION__ w
                |> failwith
          end s.[i]
      done;
      !res
    let decode hash =
      let res = Bytes.create k in
      let red_k = k - 1 and rem = ref hash in
      for i = 0 to red_k do
        res.Bytes.@(red_k - i) <-
          begin match !rem land 31 with
          | 0 -> 'A'
          | 1 -> 'C'
          | 2 -> 'D'
          | 3 -> 'E'
          | 4 -> 'F'
          | 5 -> 'G'
          | 6 -> 'H'
          | 7 -> 'I'
          | 8 -> 'K'
          | 9 -> 'L'
          | 10 -> 'M'
          | 11 -> 'N'
          | 12 -> 'O'
          | 13 -> 'P'
          | 14 -> 'Q'
          | 15 -> 'R'
          | 16 -> 'S'
          | 17 -> 'T'
          | 18 -> 'U'
          | 19 -> 'V'
          | 20 -> 'W'
          | 21 -> 'Y'
          | _ -> assert false
          end;
        rem := !rem lsr 5
      done;
      Bytes.to_string res
    (* Iterates over all k-mers.
       For each position it presents the forward hash only *)
    type iter_t = int
    let iteri f s =
      (* Our k-mer over alphabet ACDEFGHIKLMNOPQRSTUVWY
          is encoded into base-32 numbers.
         The first 12 letters at most are used.
         If there are Xs or other non-AA letters, the string is split *)
      let l = String.length s
      (* 0b--..--11..1100000 *)
      and mask = 1 lsl (5 * k) - 32 in
      let add_aa old_hash encoded =
        ((old_hash lsl 5) land mask) lor encoded in
      let rec shift start hash pos =
        if pos - start >= k then
          f (pos - k) hash;
        let incr_pos = pos + 1 in
        if incr_pos <= l then begin
          let encoded = encode_char (fun _ -> -1) s.[pos] in
          if encoded >= 0 then
            shift start (add_aa hash encoded) incr_pos
          else
            (* In this case we restart *)
            if incr_pos + k <= l then
              shift incr_pos 0 incr_pos
        end in
      shift 0 0 0
    module HashFrequencies = IntHashFrequencies
    let iterc hf s =
      iteri
        (fun _ hash ->
          HashFrequencies.add hf hash 1)
        s
  end

(* Base type without iterators, which depend on the strandedness *)
module DNABaseHash (K: IntParameter_t): BaseHash_t with type t = int
= struct
    type t = int
    let k =
      if K.n > 30 then
        Printf.sprintf "(%s): Invalid argument (k must be <= 30, found %d)" __FUNCTION__ K.n |> failwith;
      K.n
    (* There are 4 symbols in the alphabet, each one encoded as a 2-bit number *)
    let alphabet = "ACGT"
    let hex_format = Scanf.format_from_string (Printf.sprintf "%%0%dx" ((k * 2 + 3) / 4)) "%d"
    let to_hex = Printf.sprintf hex_format
    let encode_char err_f = function
      | 'A' | 'a' -> 0
      | 'C' | 'c' -> 1
      | 'G' | 'g' -> 2
      | 'T' | 't' -> 3
      | w -> err_f w
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
    let decode hash =
      let res = Bytes.create k in
      let red_k = k - 1 and rem = ref hash in
      for i = 0 to red_k do
        res.Bytes.@(red_k - i) <-
          begin match !rem land 3 with
          | 0 -> 'A'
          | 1 -> 'C'
          | 2 -> 'G'
          | 3 -> 'T'
          | _ -> assert false
          end;
        rem := !rem lsr 2
      done;
      Bytes.to_string res
  end

module DNAHashSingleStranded (K: IntParameter_t): IntHash_t with type iter_t = int
= struct
    include DNABaseHash (K)
    (* Iterates over all k-mers.
       For each position it presents the forward hash only *)
    type iter_t = int
    let iteri f s =
      (* Our k-mer over alphabet ACGT
          is encoded into base-4 numbers.
         The first 30 letters at most are used.
         If there are Ns or other non-base letters, the string is split *)
      let l = String.length s
      (* 0b--..--11..1100 *)
      and mask = 1 lsl (2 * k) - 4 in
      let add_base old_hash encoded =
        ((old_hash lsl 2) land mask) lor encoded in
      let rec shift start hash pos =
        if pos - start >= k then
          f (pos - k) hash;
        let incr_pos = pos + 1 in
        if incr_pos <= l then
          match s.[pos] with
          | 'A' | 'a' -> shift start (add_base hash 0) incr_pos
          | 'C' | 'c' -> shift start (add_base hash 1) incr_pos
          | 'G' | 'g' -> shift start (add_base hash 2) incr_pos
          | 'T' | 't' -> shift start (add_base hash 3) incr_pos
          | _ ->
            (* In this case we restart *)
            if incr_pos + k <= l then
              shift incr_pos 0 incr_pos in
      shift 0 0 0
    module HashFrequencies = IntHashFrequencies
    let iterc hf s =
      iteri
        (fun _ hash ->
          HashFrequencies.add hf hash 1)
        s
  end
module DNAHashDoubleStrandedLexicographic (K: IntParameter_t): IntHash_t with type iter_t = int * int
= struct
    include DNABaseHash (K)
    (* Iterates over all k-mers.
       For each position it presents both forward and reverse hash *)
    type iter_t = int * int
    let iteri f s =
      (* Our k-mer over alphabet ACGT and its reverse complement
          are encoded into base-4 numbers.
         The first 30 letters at most are used.
         If there are Ns or other non-base letters, the string is split *)
      let l = String.length s
      (* 0b--..--11..1100 *)
      and mask_f = 1 lsl (2 * k) - 4
      and mask_r = 1 lsl (2 * k - 2) - 1 in
      let add_base (old_hash_f, old_hash_r) encoded_f encoded_r =
        ((old_hash_f lsl 2) land mask_f) lor encoded_f,
        ((old_hash_r lsr 2) land mask_r) lor (encoded_r lsl (2 * (k - 1))) in
      let rec shift start hashes pos =
        if pos - start >= k then
          f (pos - k) hashes;
        let incr_pos = pos + 1 in
        if incr_pos <= l then
          match s.[pos] with
          | 'A' | 'a' -> shift start (add_base hashes 0 3) incr_pos
          | 'C' | 'c' -> shift start (add_base hashes 1 2) incr_pos
          | 'G' | 'g' -> shift start (add_base hashes 2 1) incr_pos
          | 'T' | 't' -> shift start (add_base hashes 3 0) incr_pos
          | _ ->
            (* In this case we restart *)
            if incr_pos + k <= l then
              shift incr_pos (0, 0) incr_pos in
      shift 0 (0, 0) 0
    module HashFrequencies = IntHashFrequencies
    let iterc hf s =
      iteri
        (fun _ (hash_f, hash_r) ->
          HashFrequencies.add hf (min hash_f hash_r) 1)
        s
  end
module DNAHashDoubleStrandedBoth (K: IntParameter_t): IntHash_t with type iter_t = int * int
= struct
    include DNAHashDoubleStrandedLexicographic (K)
    module HashFrequencies = IntHashFrequencies
    let iterc hf s =
      iteri
        (fun _ (hash_f, hash_r) ->
          HashFrequencies.add hf hash_f 1;
          HashFrequencies.add hf hash_r 1)
        s
  end

module LevenshteinBall (H: BaseHash_t with type t = int):
  sig
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
    let lint s =
      (* This is not entirely general, but OK for the time being *)
      let s = String.uppercase_ascii s |> Bytes.of_string
      and encode = H.encode_char (fun _ -> -1) in
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

