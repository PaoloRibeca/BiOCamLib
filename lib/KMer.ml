(*
    KMer.ml -- (c) 2020-2022 Paolo Ribeca, <paolo.ribeca@gmail.com>

    This file is part of BiOCamLib, the OCaml foundations upon which
    a number of the bioinformatics tools I developed are built.

    KMer.ml implements tools to iterate over, and hash, k-mers present
    in FASTA and SE/PE FASTQ files. Reads can be processed immediately
    or stored for future use when needed.

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

module FileType =
  struct
    type t =
      | FASTA of string
      | SingleEndFASTQ of string
      | PairedEndFASTQ of string * string
      | InterleavedFASTQ of string
      | Tabular of string
  end

module ReadFiles:
  sig
    type t
    type read_t = {
      tag: string;
      seq: string;
      qua: string (* Reads from FASTA files have empty qualities *)
    }
    val empty: t
    val length: t -> int
    val add_from_files: t -> FileType.t -> t
    (* Arguments to the function are read id, segment id, payload *)
    val iter: ?linter:(string -> string) -> ?verbose:bool -> (int -> int -> read_t -> unit) -> t -> unit
  end
= struct
    type read_t = {
      tag: string;
      seq: string;
      qua: string (* Reads from FASTA files have empty qualities *)
    }
    type t = FileType.t array
    let empty = [||]
    let length = Array.length
    let add_from_files files file =
      Array.append files [| file |]
    let iter ?(linter = Sequences.Lint.dnaize ~keep_dashes:false) ?(verbose = false) f =
      Array.iter
        (function
          | FileType.FASTA file ->
            Sequences.FASTA.iter ~linter ~verbose
              (fun i tag seq ->
                f i 0 { tag; seq; qua = "" })
              file
          | SingleEndFASTQ file ->
            Sequences.FASTQ.iter_se ~linter ~verbose
              (fun i tag seq qua ->
                f i 0 { tag; seq; qua })
              file
          | PairedEndFASTQ (file1, file2) ->
            Sequences.FASTQ.iter_pe ~linter ~verbose
              (fun i tag1 seq1 qua1 tag2 seq2 qua2 ->
                f i 0 { tag = tag1; seq = seq1; qua = qua1 };
                f i 1 { tag = tag2; seq = seq2; qua = qua2 })
              file1 file2
          | InterleavedFASTQ file ->
            Sequences.FASTQ.iter_il ~linter ~verbose
              (fun i tag1 seq1 qua1 tag2 seq2 qua2 ->
                f i 0 { tag = tag1; seq = seq1; qua = qua1 };
                f i 1 { tag = tag2; seq = seq2; qua = qua2 })
              file
          | Tabular file ->
            Sequences.Tabular.iter ~linter ~verbose
              (fun i tag seq qua ->
                f i 0 { tag; seq; qua })
              (fun i tag1 seq1 qua1 tag2 seq2 qua2 ->
                f i 0 { tag = tag1; seq = seq1; qua = qua1 };
                f i 1 { tag = tag2; seq = seq2; qua = qua2 })
              file)
  end

module ReadStore:
  sig
    type t
    type read_t = {
      tag: string;
      seq: string;
      qua: string (* Reads from FASTA files have empty qualities *)
    }
    (* A filter is something that separates reads into (singletons, selected, leftovers) *)
    val singleton: int
    val selected: int
    val unmarked: int
    type filter_t = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
    val empty: t
    val length: t -> int
    val add_from_files: ?linter:(string -> string) -> ?verbose:bool -> t -> FileType.t -> t
    (* Arguments to the function are store, optional filter (can be empty), name of output prefix
        (output reads can be FASTA and/or FASTQ SE and/or FASTQ PE) *)
    val to_fast: ?verbose:bool -> t -> filter_t -> string -> unit
    val to_tabular: ?verbose:bool -> t -> filter_t -> string -> unit
    val seq_length: t -> int
    (* Arguments to the function are read id, segment id, payload *)
    val iter: (int -> int -> read_t -> unit) -> t -> unit
  end
= struct
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
    let add_from_files ?(linter = Sequences.Lint.dnaize ~keep_dashes:false) ?(verbose = false) orig file =
      let res = ref [] in
      begin match file with
      | FileType.FASTA file ->
        Sequences.FASTA.iter ~linter ~verbose
          (fun _ tag seq ->
            SingleEndRead { tag; seq; qua = "" } |> Tools.List.accum res)
          file
      | SingleEndFASTQ file ->
        Sequences.FASTQ.iter_se ~linter ~verbose
          (fun _ tag seq qua ->
            SingleEndRead { tag; seq; qua } |> Tools.List.accum res)
          file
      | PairedEndFASTQ (file1, file2) ->
        Sequences.FASTQ.iter_pe ~linter ~verbose
          (fun _ tag1 seq1 qua1 tag2 seq2 qua2 ->
            PairedEndRead ({ tag = tag1; seq = seq1; qua = qua1 }, { tag = tag2; seq = seq2; qua = qua2 })
              |> Tools.List.accum res)
          file1 file2
      | InterleavedFASTQ file ->
        Sequences.FASTQ.iter_il ~linter ~verbose
          (fun _ tag1 seq1 qua1 tag2 seq2 qua2 ->
            PairedEndRead ({ tag = tag1; seq = seq1; qua = qua1 }, { tag = tag2; seq = seq2; qua = qua2 })
              |> Tools.List.accum res)
          file
      | Tabular file ->
        Sequences.Tabular.iter ~linter ~verbose
          (fun _ tag seq qua ->
            SingleEndRead { tag; seq; qua } |> Tools.List.accum res)
          (fun _ tag1 seq1 qua1 tag2 seq2 qua2 ->
            PairedEndRead ({ tag = tag1; seq = seq1; qua = qua1 }, { tag = tag2; seq = seq2; qua = qua2 })
              |> Tools.List.accum res)
          file
      end;
      let res = Array.append orig (Array.of_list !res) in
      if verbose then
        Printf.eprintf "(%s): %d reads in store so far (total length %d)\n%!"
          __FUNCTION__ (Array.length res) (seq_length res);
      res
    let to_fast ?(verbose = false) store filter prefix =
      let len = Array.length store and f_len = Bigarray.Array1.dim filter in
      (* The filter can be empty *)
      if f_len <> len && f_len <> 0 then
        Printf.sprintf
          "(%s): Invalid parameters (filter must be empty or have as many elements as the number of reads)"
          __FUNCTION__ |> failwith;
      let print_fastq_record_filtered classification output read =
        Printf.fprintf output "@%d__%s\n%s\n+\n%s\n" classification read.tag read.seq read.qua
      and print_fastq_record output read =
        Printf.fprintf output "@%s\n%s\n+\n%s\n" read.tag read.seq read.qua
      and output0 = open_out (prefix ^ ".fasta")
      and output1 = open_out (prefix ^ "_SE.fastq")
      and output2 = [| open_out (prefix ^ "_PE_1.fastq"); open_out (prefix ^ "_PE_2.fastq") |] in
      if verbose then
        Printf.eprintf "(%s): Writing %d reads...%!" __FUNCTION__ len;
      Array.iteri begin
        if f_len <> 0 then
          (fun i -> function
            | SingleEndRead segm ->
              if segm.qua = "" then
                Printf.fprintf output0 ">%d__%s\n%s\n" filter.{i} segm.tag segm.seq
              else
                print_fastq_record_filtered filter.{i} output1 segm
            | PairedEndRead (segm1, segm2) ->
              print_fastq_record_filtered filter.{i} output2.(0) segm1;
              print_fastq_record_filtered filter.{i} output2.(1) segm2)
        else
          (fun _ -> function
            | SingleEndRead segm ->
              if segm.qua = "" then
                Printf.fprintf output0 ">%s\n%s\n" segm.tag segm.seq
              else
                print_fastq_record output1 segm
            | PairedEndRead (segm1, segm2) ->
              print_fastq_record output2.(0) segm1;
              print_fastq_record output2.(1) segm2)
        end store;
      close_out output0;
      close_out output1;
      close_out output2.(0);
      close_out output2.(1);
      if verbose then
        Printf.eprintf " done.\n%!"
    let to_tabular ?(verbose = false) store filter fname =
      let len = Array.length store and f_len = Bigarray.Array1.dim filter in
      (* The filter can be empty *)
      if f_len <> len && f_len <> 0 then
        Printf.sprintf
          "(%s): Invalid parameters (filter must be empty or have as many elements as the number of reads)"
          __FUNCTION__ |> failwith;
      let output = open_out fname in
      if verbose then
        Printf.eprintf "(%s): Writing %d reads...%!" __FUNCTION__ len;
      Array.iteri begin
        if f_len <> 0 then
          (fun i -> function
            | SingleEndRead segm ->
              if segm.qua = "" then
                Printf.fprintf output "%d__%s\t%s\n" filter.{i} segm.tag segm.seq
              else
                Printf.fprintf output "%d__%s\t%s\t%s\n" filter.{i} segm.tag segm.seq segm.qua
            | PairedEndRead (segm1, segm2) ->
              Printf.fprintf output "%d__%s\t%s\t%s\t%d__%s\t%s\t%s\n"
                filter.{i} segm1.tag segm1.seq segm1.qua filter.{i} segm2.tag segm2.seq segm2.qua)
        else
          (fun _ -> function
            | SingleEndRead segm ->
              if segm.qua = "" then
                Printf.fprintf output "%s\t%s\n" segm.tag segm.seq
              else
                Printf.fprintf output "%s\t%s\t%s\n" segm.tag segm.seq segm.qua
            | PairedEndRead (segm1, segm2) ->
              Printf.fprintf output "%s\t%s\t%s\t%s\t%s\t%s\n"
                segm1.tag segm1.seq segm1.qua segm2.tag segm2.seq segm2.qua)
        end store;
      close_out output;
      if verbose then
        Printf.eprintf " done.\n%!"
  end

module type KMerHash =
  sig
    type t
    val k: int
    val encode: string -> t
    val decode: t -> string
    (* Iterates a function over all hashes that can be extracted from a string
        according to the specific method being used (for instance, in the case of DNA
        iteration happens both on the string and its RC).
       The second argument to the function is the frequency of the k-mer *)
    val iter: (t -> int -> unit) -> string -> unit
  end

module type IntParameter =
  sig
    val value: int
  end

module ProteinEncodingHash (K: IntParameter):
  KMerHash with type t = int
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
              Printf.sprintf "(%s): Invalid argument (expected character in [ACGTacgt], found '%c')" __FUNCTION__ w
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
    (* This is not thread-safe, but hopefully more optimised than placing the filter into iter() *)
    let filter = IntHashtbl.create 1024
    exception Exit of int
    let iter f s =
      let len = String.length s
      (* 0b--..--11..1100000 *)
      and mask = 1 lsl (5 * k) - 32 in
      let red_len = len - 1 in
      (* This function is invoked to compute the first hash
          and start processing from a given position *)
      let rec accumulate_hashes pos =
        let shift old_hash pos =
          let encoded = encode_char (fun _ -> raise_notrace (Exit pos)) s.[pos] in
          ((old_hash lsl 5) land mask) lor encoded in
        (* There must be at least k letters left *)
        if len - pos < k then
          ()
        else begin
          (* Compute the first hash(es).
             Our k-mer over alphabet ACDEFGHIKLMNOPQRSTUVWY is encoded into base-32 numbers.
              The first 12 letters at most are used.
              If there are Xs or other non-AA letters, the string is split *)
          let top = pos + k - 2 and hash = ref 0 in
          try
            for i = pos to top do
              hash := shift !hash i
            done;
            for i = pos + k - 1 to red_len do
              hash := shift !hash i;
              let hash = !hash in
              add_to_kmer_counter filter hash 1
            done
          with Exit p -> accumulate_hashes (p + 1)
        end in
      IntHashtbl.clear filter;
      accumulate_hashes 0;
      IntHashtbl.iter (fun hash cntr -> f hash !cntr) filter
  end

module DNAEncodingHash (K: IntParameter):
  KMerHash with type t = int
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
          and start processing from a given position *)
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
             Our k-mer over alphabet ACGT and its reverse complement
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

