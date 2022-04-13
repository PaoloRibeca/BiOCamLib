(*
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

module IntComparable = struct type t = int let compare = (-) end

module IntSet = Set.Make (IntComparable)
module IntMap = Map.Make (IntComparable)

module IntHash =
  struct
    type t = int
    let equal (i: int) (j: int) = (i - j = 0) (*(i = j)*)
    let hash (i: int) = i (* land max_int *)
  end
module IntHashtbl = Hashtbl.Make (IntHash)
let add_to_kmer_counter counter hash occs =
  try
    let found = IntHashtbl.find counter hash in
    found := !found + occs
  with Not_found ->
    IntHashtbl.add counter hash (ref occs)

module ReadStore:
  sig
    type file_t =
      | FASTA of string
      | SingleEndFASTQ of string
      | PairedEndFASTQ of string * string
    type read_t = {
      tag: string;
      seq: string;
      qua: string (* Reads from FASTA files have empty qualities *)
    }
    type template_t =
      | SingleEndRead of read_t
      | PairedEndRead of read_t * read_t
    type t = template_t array
    (* A filter is something that separates reads into (singletons, selected, leftovers) *)
    val singleton: int
    val selected: int
    val unmarked: int
    type filter_t = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
    val empty: t
    val length: t -> int
    val add_from_files: ?verbose:bool -> t -> file_t -> t
    (* Arguments to the function are store, set of read ids, name of output prefix
        (output reads can be FASTA and/or FASTQ SE and/or FASTQ PE) *)
    val to_files: ?verbose:bool -> t -> filter_t -> string -> unit
    val seq_length: t -> int
    (* Arguments to the function are read id, segment id, payload *)
    val iter: (int -> int -> read_t -> unit) -> t -> unit
  end
= struct
    type file_t =
      | FASTA of string
      | SingleEndFASTQ of string
      | PairedEndFASTQ of string * string
    type read_t = {
      tag: string;
      seq: string;
      qua: string
    }
    type template_t =
      | SingleEndRead of read_t
      | PairedEndRead of read_t * read_t
    type t = template_t array
    let singleton = 0
    let selected = 1
    let unmarked = 2
    type filter_t = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
    let empty = [||]
    let iter f =
      Array.iteri
        (fun templ_i -> function
          | SingleEndRead segm ->
            f templ_i 0 segm
          | PairedEndRead (segm1, segm2) ->
            f templ_i 0 segm1;
            f templ_i 1 segm2)
    let length = Array.length
    let seq_length store =
      let res = ref 0 in
      iter (fun _ _ segm -> res := !res + String.length segm.seq) store;
      !res
    let add_from_files ?(verbose = false) orig file =
      let res = ref [] in
      begin match file with
      | FASTA file ->
        Sequences.FASTA.iter ~verbose
          (fun _ tag seq ->
            SingleEndRead { tag; seq; qua = "" } |> Tools.List.accum res)
          file
      | SingleEndFASTQ file ->
        Sequences.FASTQ.iter_se ~verbose
          (fun _ tag seq qua ->
            SingleEndRead { tag; seq; qua } |> Tools.List.accum res)
          file
      | PairedEndFASTQ (file1, file2) ->
        Sequences.FASTQ.iter_pe ~verbose
          (fun _ tag1 seq1 qua1 tag2 seq2 qua2 ->
            PairedEndRead ({ tag = tag1; seq = seq1; qua = qua1 }, { tag = tag2; seq = seq2; qua = qua2 })
              |> Tools.List.accum res)
          file1 file2
      end;
      let res = Array.append orig (Array.of_list !res) in
      if verbose then
        Printf.eprintf "(%s): %d reads in store so far (total length %d)\n%!"
          __FUNCTION__ (Array.length res) (seq_length res);
      res
    let to_files ?(verbose = false) store filter prefix =
      let len = Array.length store in
      if Bigarray.Array1.dim filter <> len then
        Printf.sprintf "(%s): Invalid parameters (filter length must be the same as the number of reads)"
          __FUNCTION__ |> failwith;
      let print_fastq_record classification output read =
        Printf.fprintf output "@%d__%s\n%s\n+\n%s\n%!" classification read.tag read.seq read.qua
      and output0 = open_out (prefix ^ ".fasta")
      and output1 = open_out (prefix ^ "_SE.fastq")
      and output2 = [| open_out (prefix ^ "_PE_1.fastq"); open_out (prefix ^ "_PE_2.fastq") |] in
      if verbose then
        Printf.eprintf "(%s): Writing %d reads...%!" __FUNCTION__ len;
      Array.iteri
        (fun i -> function
          | SingleEndRead segm ->
            if segm.qua = "" then
              Printf.fprintf output0 ">%d__%s\n%s\n%!" filter.{i} segm.tag segm.seq
            else
              print_fastq_record filter.{i} output1 segm
          | PairedEndRead (segm1, segm2) ->
            print_fastq_record filter.{i} output2.(0) segm1;
            print_fastq_record filter.{i} output2.(1) segm2)
        store;
      close_out output0;
      close_out output1;
      close_out output2.(0);
      close_out output2.(1);
      if verbose then
        Printf.eprintf " done.\n%!"
  end

module type KMerHash =
  sig
    type t
    type strand_t =
      | Forward
      | Reverse
    val k: int
    (* Iterates a function over all hashes that can be extracted from a string
        according to the specific method being used *)
    val iter: (t -> int -> unit) -> string -> unit
    (*val decode: t -> string*)
  end

module type IntParameter =
  sig
    val value: int
  end

module EncodingHash (K: IntParameter):
  KMerHash with type t = int
= struct
    type t = int
    type strand_t =
      | Forward
      | Reverse
    let k =
      if K.value > 30 then
        Printf.sprintf "(%s): Invalid argument (k must be <= 30, found %d)" __FUNCTION__ K.value |> failwith;
      K.value
    (* This is not thread-safe, but hopefully more optimised than placing the filter into iter() *)
    let filter = IntHashtbl.create 1024
    exception Exit of int
    let iter f s =
      (* We have to take the RC into account.
         We also want to present repeated k-mers only once,
          so that we can replace sets of reads with lists *)
      let len = String.length s
      (* 0b--..--11..1100 *)
      and mask_f = 1 lsl (2 * k) - 4
      and mask_r = 1 lsl (2 * k - 2) - 1 in
      let red_len = len - 1 in
      (* This function is invoked to compute the first hash
          and start processing starting from a given position *)
      let rec accumulate_hashes pos =
        let shift (old_hash_f, old_hash_r) pos =
          let encoded_f, encoded_r =
            match s.[pos] with
            | 'A' | 'a' -> 0, 3
            | 'C' | 'c' -> 1, 2
            | 'G' | 'g' -> 2, 1
            | 'T' | 't' -> 3, 0
            | _ -> raise_notrace (Exit pos) in
          ((old_hash_f lsl 2) land mask_f) lor encoded_f,
          ((old_hash_r lsr 2) land mask_r) lor (encoded_r lsl (2 * (k - 1))) in
        (* There must be at least k letters left *)
        if len - pos < k then
          ()
        else begin
          (* Compute the first hash(es).
             Our k-mer over alphabet ACGT, and its reverse complement,
              are encoded into base-4 numbers.
              The first 30 letters at most are used.
              If there are Ns or other non-base letters, the string is split *)
          let top = pos + k - 2 and hashes = ref (0, 0) in
          try
            for i = pos to top do
              hashes := shift !hashes i
            done;
            for i = pos + k - 1 to red_len do
              hashes := shift !hashes i;
              let hash_f, hash_r = !hashes in
              if hash_f <= hash_r then
                add_to_kmer_counter filter hash_f 1
              else
                add_to_kmer_counter filter hash_r 1
            done
          with Exit p -> accumulate_hashes (p + 1)
        end in
      IntHashtbl.clear filter;
      accumulate_hashes 0;
      IntHashtbl.iter (fun hash cntr -> f hash !cntr) filter

  end
