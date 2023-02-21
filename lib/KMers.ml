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

module LevenshteinBall:
  sig
    type t
    val make: ?radius:int -> ?alphabet:string -> ?uppercase:bool -> string -> string -> string -> t
    val length: t -> int
    val iter: (string -> unit) -> t -> unit
  end
= struct
    type t = Tools.StringSet.t
    let make ?(radius = 1) ?(alphabet = "ACGT") ?(uppercase = true) l_ctxt s r_ctxt =
      if radius < 0 then
        Printf.sprintf "%s: Invalid radius %d" __FUNCTION__ radius |> failwith;
      let l_ctxt, s, r_ctxt =
        if uppercase then
          String.uppercase_ascii l_ctxt, String.uppercase_ascii s, String.uppercase_ascii r_ctxt
        else
          l_ctxt, s, r_ctxt in
      (* We trim/pad contexts whenever needed *)
      let padding = String.make radius ' ' in
      let l_ctxt = String.sub (padding ^ l_ctxt) (String.length l_ctxt) radius
      and r_ctxt = String.sub (r_ctxt ^ padding) 0 radius in
      (* The string also includes left and right contexts *)
      let hi = radius + String.length s - 1 in
      let last = hi + radius in
      let expand prev =
        let res = ref Tools.StringSet.empty in
        let open Tools.Bytes in
        Tools.StringSet.iter
          (fun orig_s ->
            let s = of_string orig_s in
            (* Mismatches *)
            for i = radius to hi do
              let c = s.@(i) in
              String.iter
                (fun cc ->
                  if cc <> c then begin
                    s.@(i) <- cc;
                    res := Tools.StringSet.add (to_string s) !res
                  end)
                alphabet;
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
                res := Tools.StringSet.add (to_string s) !res;
              (* We restore the previous state *)
              blit s i s (i + 1) l;
              s.@(i) <- c;
              (* Left-to-right deletion *)
              blit s 0 s 1 i;
              s.@(0) <- ' '; (* Padding *)
              if s.@(radius) <> ' ' then
                res := Tools.StringSet.add (to_string s) !res;
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
                  res := Tools.StringSet.add (to_string s) !res)
                alphabet;
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
                  res := Tools.StringSet.add (to_string s) !res)
                alphabet;
              (* We restore the previous state *)
              blit s 0 s 1 l;
              s.@(0) <- c
            done;
            assert (to_string s = orig_s))
          prev;
        !res in
      (* The k-mer itself gets inserted here *)
      let curr = l_ctxt ^ s ^ r_ctxt |> Tools.StringSet.singleton |> ref in
      let res = ref !curr in
      for _ = 1 to radius do
        curr := expand !curr;
        res := Tools.StringSet.union !res !curr
      done;
      (* We eliminate contexts *)
      let l = String.length s and purged = ref Tools.StringSet.empty in
      Tools.StringSet.iter
        (fun s ->
          purged := Tools.StringSet.add (String.sub s radius l) !purged)
        !res;
      !purged
    let length =
      Tools.StringSet.cardinal
    let iter =
      Tools.StringSet.iter
  end

(* Auxiliary module to store and print k-mer frequencies *)
module HashFrequencies:
  sig
    type t
    val create: int -> t
    (* The first parameter is the hash, the second its frequency *)
    val add: t -> int -> int -> unit
    val iter: (int -> int -> unit) -> t -> unit
  end
= struct
    type t = int ref Tools.IntHashtbl.t
    let create = Tools.IntHashtbl.create
    let add distr key occs =
      try
        let found = Tools.IntHashtbl.find distr key in
        found := !found + occs
      with Not_found ->
        Tools.IntHashtbl.add distr key (ref occs)
    let iter f =
      Tools.IntHashtbl.iter
        (fun key freq -> f key !freq)
  end

module type Hash_t =
  sig
    type t
    val k: int
    val encode: string -> t
    val decode: t -> string
    (* Iterates a function over all hashes that can be extracted from a string
        according to the specific method being used (for instance, in the case of DNA
        iteration happens both on the string and its RC) *)
    type iter_t
    val iteri: (int -> iter_t -> unit) -> string -> unit
    (* The second argument to the function is the frequency of the k-mer *)
    val iterc: (t -> int -> unit) -> string -> unit
  end

module type IntParameter_t = sig val value: int end

module ProteinHash (K: IntParameter_t):
  Hash_t with type t = int and type iter_t = int
= struct
    type t = int
    let k =
      if K.value > 12 then
        Printf.sprintf "(%s): Invalid argument (k must be <= 12, found %d)" __FUNCTION__ K.value |> failwith;
      K.value
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
      let red_k = k - 1 and res = ref 0 in
      for i = 0 to red_k do
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
        res.Tools.Bytes.@(red_k - i) <-
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
    let iterc f s =
      let res = HashFrequencies.create 64 in
      iteri
        (fun _ hash ->
          HashFrequencies.add res hash 1)
        s;
      HashFrequencies.iter f res
  end

module DNAHash (K: IntParameter_t):
  Hash_t with type t = int and type iter_t = int * int
= struct
    type t = int
    let k =
      if K.value > 30 then
        Printf.sprintf "(%s): Invalid argument (k must be <= 30, found %d)" __FUNCTION__ K.value |> failwith;
      K.value
    let encode s =
      if String.length s <> k then
        Printf.sprintf "(%s): Invalid argument (string length must be k=%d, found %d)" __FUNCTION__ k (String.length s)
          |> failwith;
      let red_k = k - 1 and res = ref 0 in
      for i = 0 to red_k do
        res :=
          !res lsl 2 + begin
            match s.[i] with
            | 'A' | 'a' -> 0
            | 'C' | 'c' -> 1
            | 'G' | 'g' -> 2
            | 'T' | 't' -> 3
            | w ->
              Printf.sprintf "(%s): Invalid argument (expected character in [ACGTacgt], found '%c')" __FUNCTION__ w
                |> failwith
          end
      done;
      !res
    let decode hash =
      let res = Bytes.create k in
      let red_k = k - 1 and rem = ref hash in
      for i = 0 to red_k do
        res.Tools.Bytes.@(red_k - i) <-
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
    let iterc f s =
      let res = HashFrequencies.create 64 in
      iteri
        (fun _ (hash_f, hash_r) ->
          HashFrequencies.add res hash_f 1;
          HashFrequencies.add res hash_r 1)
        s;
      HashFrequencies.iter f res
  end
