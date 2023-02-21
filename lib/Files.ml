(*
    Files.ml -- (c) 2020-2023 Paolo Ribeca, <paolo.ribeca@gmail.com>

    This file is part of BiOCamLib, the OCaml foundations upon which
    a number of the bioinformatics tools I developed are built.

    Files.ml implements tools to iterate over sequences present in
    FASTA and SE/PE FASTQ files.
    Reads can be processed immediately or stored for future use when needed.

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

(* Abstraction for filenames to be used in scripts *)
module QuotedName:
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
      let s = Scanf.unescaped s |> Tools.Split.on_char_as_array '\t' in
      assert (Array.length s = 2);
      { unquoted = Scanf.unescaped s.(0); quoted = Scanf.unescaped s.(1) }
    exception Could_not_get_quoted_name of string
    exception Executable_not_found_in_path of string
    (* PRIVATE *)
    let get_from_shell exc_f s =
      try
        Processes.Subprocess.spawn_and_read_single_line s
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

module FASTA:
  sig
    val iter: ?linter:(string -> string) -> ?verbose:bool -> (int -> string -> string -> unit) -> string -> unit
    val parallel_iter: ?linter:(string -> string) -> ?buffered_chunks_per_thread:int ->
                       ?max_memory:int -> ?verbose:bool ->
      string -> (int -> (string * string) list -> 'a) -> ('a -> unit) -> int -> unit
  end
= struct
    let iter ?(linter = Sequences.Lint.dnaize ~keep_dashes:false) ?(verbose = false) f filename =
      let file = open_in filename and progr = ref 0 and current = ref "" and seq = Buffer.create 1048576 in
      if verbose then
        Printf.eprintf "(%s): Reading FASTA file '%s'...%!" __FUNCTION__ filename;
      let process_current () =
        if !current <> "" then begin
          f !progr !current (Buffer.contents seq);
          incr progr
        end;
        Buffer.clear seq in
      try
        while true do
          let line = input_line file in
          if line <> "" then begin
            if line.[0] = '>' then begin
              process_current ();
              current := String.sub line 1 (String.length line - 1)
            end else
              linter line |> Buffer.add_string seq
          end
        done
      with End_of_file ->
        process_current ();
        close_in file;
        if verbose then
          Printf.eprintf " done.\n%!"
    let parallel_iter ?(linter = Sequences.Lint.dnaize ~keep_dashes:false) ?(buffered_chunks_per_thread = 10)
                      ?(max_memory = 1000000000) ?(verbose = false)
                      filename (f:int -> (string * string) list -> 'a) (g:'a -> unit) threads =
      let max_block_bytes = max_memory / (buffered_chunks_per_thread * threads) in
      (* Parallel section *)
      let file = open_in filename and current = ref "" and seq = Buffer.create 1048576
      and read = ref 0 and seqs = ref 0 and eof_reached = ref false in
      let read_up_to_next_sequence_name () =
        let res = ref "" in
        Buffer.clear seq;
        while not !eof_reached && !res = "" do
          try
            let line = input_line file in
            incr read;
            if line <> "" then begin
              if line.[0] = '>' then
                res := String.sub line 1 (String.length line - 1)
              else
                linter line |> Buffer.add_string seq
            end
          with End_of_file ->
            eof_reached := true;
            close_in file
        done;
        current := !res in
      if verbose then
        Tools.Printf.teprintf "0 lines read%!\n";
      read_up_to_next_sequence_name ();
      if Buffer.contents seq <> "" then
        Buffer.contents seq |>
          Printf.sprintf "(%s): On line %d: Malformed FASTA file '%s' (found content '%s' before sequence name)"
            __FUNCTION__ !read filename
          |> failwith;
      if not !eof_reached then
        Processes.Parallel.process_stream_chunkwise ~buffered_chunks_per_thread:buffered_chunks_per_thread
          (fun () ->
            (* Read more sequences, up to max_block_bytes. Must be re-entrant.
              As invariant, we assume we have always just read the name of the next sequence *)
            if not !eof_reached then begin
              let bytes = ref 0 and seqs_base = !seqs and chunk = ref [] in
              while not !eof_reached && !bytes < max_block_bytes do (* We read at least one sequence *)
                let current = !current in
                read_up_to_next_sequence_name ();
                let seq = Buffer.contents seq in
                if seq = "" then begin
                  Printf.sprintf "(%s): On line %d: Malformed FASTA file '%s' (sequence '%s' is empty)"
                    __FUNCTION__ !read current filename
                  |> failwith
                end;
                incr seqs;
                Tools.List.accum chunk (current, seq);
                bytes := !bytes + String.length current + String.length seq
              done;
              if verbose then
                Tools.Printf.teprintf "%d %s read%!\n" !read (Tools.String.pluralize_int "line" !read);
              seqs_base, !chunk
            end else
              raise End_of_file)
          (fun (seqs_base, chunk) ->
            let res = List.rev chunk |> f seqs_base in
            if verbose then begin
              let seqs = List.length chunk in
              Tools.Printf.teprintf "%d more %s processed%!\n" seqs (Tools.String.pluralize_int "sequence" seqs)
            end;
            res)
          g threads
  end

module FASTQ:
  sig
    val iter_se: ?linter:(string -> string) -> ?verbose:bool -> (int -> string -> string -> string -> unit) -> string -> unit
    val iter_pe: ?linter:(string -> string) -> ?verbose:bool ->
      (int -> string -> string -> string -> string -> string -> string -> unit) -> string -> string -> unit
    (* Interleaved file *)
    val iter_il: ?linter:(string -> string) -> ?verbose:bool ->
      (int -> string -> string -> string -> string -> string -> string -> unit) -> string -> unit
  end
= struct
    let iter_se ?(linter = Sequences.Lint.dnaize ~keep_dashes:false) ?(verbose = false) f file =
      let read = ref 0 and input = open_in file in
      if verbose then
        Printf.eprintf "(%s): Reading SE FASTQ file '%s'...%!" __FUNCTION__ file;
      begin try
        while true do
          let tag = input_line input in
          let seq = input_line input in
          let tmp = input_line input in
          let qua = input_line input in
          read := !read + 4;
          if tag.[0] <> '@' || tmp.[0] <> '+' then
            Printf.sprintf "(%s): On line %d: Malformed FASTQ file '%s'" __FUNCTION__ !read file |> failwith;
          f (!read / 4) (String.sub tag 1 (String.length tag - 1)) (linter seq) qua
        done
      with End_of_file -> ()
      end;
      close_in input;
      if verbose then
        Printf.eprintf " done.\n%!"
    let iter_pe ?(linter = Sequences.Lint.dnaize ~keep_dashes:false) ?(verbose = false) f file1 file2 =
      let read = ref 0 and input1 = open_in file1 and input2 = open_in file2 in
      if verbose then
        Printf.eprintf "(%s): Reading PE FASTQ files '%s' and '%s'...%!" __FUNCTION__ file1 file2;
      begin try
        while true do
          let tag1 = input_line input1 in
          let seq1 = input_line input1 in
          let tmp1 = input_line input1 in
          let qua1 = input_line input1 in
          let tag2 = input_line input2 in
          let seq2 = input_line input2 in
          let tmp2 = input_line input2 in
          let qua2 = input_line input2 in
          read := !read + 8;
          if tag1.[0] <> '@' || tmp1.[0] <> '+' || tag2.[0] <> '@' || tmp2.[0] <> '+' then
            Printf.sprintf "(%s): On line %d: Malformed FASTQ file(s) '%s' and/or '%s'"
              __FUNCTION__ !read file1 file2 |> failwith;
          f (!read / 8)
            (String.sub tag1 1 (String.length tag1 - 1)) (linter seq1) qua1
            (String.sub tag2 1 (String.length tag2 - 1)) (linter seq2) qua2
        done
      with End_of_file -> ()
      end;
      close_in input1;
      close_in input2;
      if verbose then
        Printf.eprintf " done.\n%!"
    let iter_il ?(linter = Sequences.Lint.dnaize ~keep_dashes:false) ?(verbose = false) f file =
      let read = ref 0 and input = open_in file in
      if verbose then
        Printf.eprintf "(%s): Reading interleaved PE FASTQ file '%s'...%!" __FUNCTION__ file;
      begin try
        while true do
          let tag1 = input_line input in
          let seq1 = input_line input in
          let tmp1 = input_line input in
          let qua1 = input_line input in
          let tag2 = input_line input in
          let seq2 = input_line input in
          let tmp2 = input_line input in
          let qua2 = input_line input in
          read := !read + 8;
          if tag1.[0] <> '@' || tmp1.[0] <> '+' || tag2.[0] <> '@' || tmp2.[0] <> '+' then
            Printf.sprintf "(%s): On line %d: Malformed FASTQ file '%s'" __FUNCTION__ !read file |> failwith;
          f (!read / 8)
            (String.sub tag1 1 (String.length tag1 - 1)) (linter seq1) qua1
            (String.sub tag2 1 (String.length tag2 - 1)) (linter seq2) qua2
        done
      with End_of_file -> ()
      end;
      close_in input;
      if verbose then
        Printf.eprintf " done.\n%!"
  end

module Tabular:
  sig
    val iter:
      ?linter:(string -> string) -> ?verbose:bool ->
      (int -> string -> string -> string -> unit) ->
      (int -> string -> string -> string -> string -> string -> string -> unit) -> string -> unit
  end
= struct
    let iter ?(linter = Sequences.Lint.dnaize ~keep_dashes:false) ?(verbose = false) f g filename =
      let file = open_in filename and progr = ref 0 in
      if verbose then
        Printf.eprintf "(%s): Reading tabular file '%s'...%!" __FUNCTION__ filename;
      try
        while true do
          let line = input_line file |> Tools.Split.on_char_as_array '\t' in
          incr progr;
          match Array.length line with
          | 2 -> (* FASTA *)
            f !progr line.(0) (linter line.(1)) ""
          | 3 -> (* SE FASTQ *)
            f !progr line.(0) (linter line.(1)) line.(2)
          | 6 -> (* PE FASTQ *)
            g !progr line.(0) (linter line.(1)) line.(2) line.(3) (linter line.(4)) line.(5)
          | n ->
            Printf.sprintf "(%s): On line %d: Malformed tabular file '%s' (found %d fields, expected 2, 3, or 6)"
                __FUNCTION__ !progr filename n
              |> failwith;
        done
      with End_of_file ->
        close_in file;
        if verbose then
          Printf.eprintf " done.\n%!"
  end

module Type =
  struct
    type t =
      | FASTA of string
      | SingleEndFASTQ of string
      | PairedEndFASTQ of string * string
      | InterleavedFASTQ of string
      | Tabular of string
  end

module ReadsIterate:
  sig
    type t
    type read_t = {
      tag: string;
      seq: string;
      qua: string (* Reads from FASTA files have empty qualities *)
    }
    val empty: t
    val length: t -> int
    val add_from_files: t -> Type.t -> t
    (* Arguments to the function are read id, segment id, payload *)
    val iter: ?linter:(string -> string) -> ?verbose:bool -> (int -> int -> read_t -> unit) -> t -> unit
  end
= struct
    type read_t = {
      tag: string;
      seq: string;
      qua: string (* Reads from FASTA files have empty qualities *)
    }
    type t = Type.t array
    let empty = [||]
    let length = Array.length
    let add_from_files files file =
      Array.append files [| file |]
    let iter ?(linter = Sequences.Lint.dnaize ~keep_dashes:false) ?(verbose = false) f =
      Array.iter
        (function
          | Type.FASTA file ->
            FASTA.iter ~linter ~verbose
              (fun i tag seq ->
                f i 0 { tag; seq; qua = "" })
              file
          | SingleEndFASTQ file ->
            FASTQ.iter_se ~linter ~verbose
              (fun i tag seq qua ->
                f i 0 { tag; seq; qua })
              file
          | PairedEndFASTQ (file1, file2) ->
            FASTQ.iter_pe ~linter ~verbose
              (fun i tag1 seq1 qua1 tag2 seq2 qua2 ->
                f i 0 { tag = tag1; seq = seq1; qua = qua1 };
                f i 1 { tag = tag2; seq = seq2; qua = qua2 })
              file1 file2
          | InterleavedFASTQ file ->
            FASTQ.iter_il ~linter ~verbose
              (fun i tag1 seq1 qua1 tag2 seq2 qua2 ->
                f i 0 { tag = tag1; seq = seq1; qua = qua1 };
                f i 1 { tag = tag2; seq = seq2; qua = qua2 })
              file
          | Tabular file ->
            Tabular.iter ~linter ~verbose
              (fun i tag seq qua ->
                f i 0 { tag; seq; qua })
              (fun i tag1 seq1 qua1 tag2 seq2 qua2 ->
                f i 0 { tag = tag1; seq = seq1; qua = qua1 };
                f i 1 { tag = tag2; seq = seq2; qua = qua2 })
              file)
  end

module ReadsStore:
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
    val add_from_files: ?linter:(string -> string) -> ?verbose:bool -> t -> Type.t -> t
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
      | Type.FASTA file ->
        FASTA.iter ~linter ~verbose
          (fun _ tag seq ->
            SingleEndRead { tag; seq; qua = "" } |> Tools.List.accum res)
          file
      | SingleEndFASTQ file ->
        FASTQ.iter_se ~linter ~verbose
          (fun _ tag seq qua ->
            SingleEndRead { tag; seq; qua } |> Tools.List.accum res)
          file
      | PairedEndFASTQ (file1, file2) ->
        FASTQ.iter_pe ~linter ~verbose
          (fun _ tag1 seq1 qua1 tag2 seq2 qua2 ->
            PairedEndRead ({ tag = tag1; seq = seq1; qua = qua1 }, { tag = tag2; seq = seq2; qua = qua2 })
              |> Tools.List.accum res)
          file1 file2
      | InterleavedFASTQ file ->
        FASTQ.iter_il ~linter ~verbose
          (fun _ tag1 seq1 qua1 tag2 seq2 qua2 ->
            PairedEndRead ({ tag = tag1; seq = seq1; qua = qua1 }, { tag = tag2; seq = seq2; qua = qua2 })
              |> Tools.List.accum res)
          file
      | Tabular file ->
        Tabular.iter ~linter ~verbose
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

