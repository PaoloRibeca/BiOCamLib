(*
    Files.ml -- (c) 2020-2025 Paolo Ribeca, <paolo.ribeca@gmail.com>

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

open Better

module Exception =
  struct
    include Exception
    let raise_malformed ?(comment = "") ?(path2 = "") __FUNCTION__ line kind path =
      let files =
        if path2 = "" then
          "file '" ^ path ^ "'"
        else
          "file(s) '" ^ path ^ "' and/or '" ^ path2 ^ "'"
      and comment =
        if comment <> "" then
          " (" ^ comment ^ ")"
        else
          comment in
      Exception.raise __FUNCTION__ IO_Format
        (Printf.sprintf "On line %d: Malformed %s %s%s" line kind files comment)
  end

(* Abstraction for paths to be used in scripts *)
module QuotedPath:
  sig
    type t = {
      unquoted : string;
      quoted : string
    }
    val none: t
    val to_string: t -> string
    val of_string: string -> t
    (* All the following functions can fail *)
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
      let s = Scanf.unescaped s |> String.Split.on_char_as_array '\t' in
      assert (Array.length s = 2);
      { unquoted = Scanf.unescaped s.(0); quoted = Scanf.unescaped s.(1) }
    (* PRIVATE *)
    let get_from_shell exc_f s =
      try
        Processes.Subprocess.spawn_and_read_single_line s
      with _ ->
        exc_f s
    let get_quoted s =
      get_from_shell
        (fun s -> Exception.raise __FUNCTION__ IO_Format (Printf.sprintf "Could not get quoted name for '%s'" s))
        (Printf.sprintf "bash -c 'printf \"%%q\" \"%s\"'" s)
    let get_in_path s =
      get_from_shell
        (fun s -> Exception.raise __FUNCTION__ IO_Format (Printf.sprintf "Executable '%s' not found in $PATH" s))
        (Printf.sprintf "bash -c 'command -v \"%s\"'" s)
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

module Base =
  struct
    module Read =
      struct
        type t = {
          tag: string;
          seq: string;
          qua: string (* Reads from FASTA files have empty qualities *)
        }
        let empty = { tag = ""; seq = ""; qua = "" } (* Immutable *)
      end
    type linter_t = string -> string
    (* C++-style iterators *)
    module Iterator =
      struct
        (* Initialiser type is linter and path *)
        type init_t = linter_t * string
        (* Return type is read ID, segment ID, and (tag, sequence, qualities) *)
        type ret_t = int * int * Read.t
        module type Type_t =
          sig
            include Iterator_t with type init_t := init_t and type ret_t := ret_t
            (* Returns file path and number of parsed lines *)
            val info: t -> string * int
            (* Same as get(), but applies different functions to SE and PE records.
               Helpful when processing files with unknown or variable format *)
            val get_se_pe: t -> (ret_t -> unit) -> (ret_t -> ret_t -> unit) -> unit
            val get_se_pe_and_incr: t -> (ret_t -> unit) -> (ret_t -> ret_t -> unit) -> unit
          end
        (* Here type 'a could be a string (path) or a format abstraction *)
        type 'a t = ?linter:linter_t -> ?verbose:bool -> (ret_t -> unit) -> 'a -> unit
        type 'a se_pe_t = ?linter:linter_t -> ?verbose:bool ->
                          (ret_t -> unit) -> (ret_t -> ret_t -> unit) -> 'a -> unit
      end
  end

module FASTA:
  sig
    (* C++-style iterators *)
    module Iterator: Base.Iterator.Type_t (* Iterator.t is opaque *)
    (* OCaml-style iterator *)
    val iter: string Base.Iterator.t
    (* This file format also has a specialised parallelised high-throughput implementation *)
    val parallel_iter: ?linter:Base.linter_t -> ?buffered_chunks_per_thread:int ->
                       ?max_memory:int -> ?verbose:bool ->
                       string -> (int -> (string * string) list -> 'a) -> ('a -> unit) -> int -> unit
  end
= struct
    module Iterator: Base.Iterator.Type_t =
      struct
        type t = {
          linter: string -> string;
          path: string;
          input: in_channel;
          mutable lines: int; (* Number of lines read so far *)
          mutable eof_reached: bool;
          (* *)
          mutable progr: int; (* Sequence progressive (from 0) *)
          mutable tag: string; (* Tag of sequence currently in buffer *)
          mutable next: string; (* Tag of the next sequence *)
          buf: Buffer.t;
          mutable seq: string
        }
        let empty () = {
          linter = Fun.id;
          path = "";
          input = stdin;
          lines = 0;
          eof_reached = true;
          progr = -1;
          tag = "";
          next = "";
          buf = Buffer.create 128;
          seq = ""
        }
        let info it = it.path, it.lines
        let is_empty it =
          (* Here we have to be careful as the last sequence will be consumed
              _after_ the stream has ended *)
          it.eof_reached && it.tag = ""
        let delete it =
          close_in it.input;
          it.eof_reached <- true;
          Buffer.reset it.buf
          [@@inline]
        let incr it =
          if it.eof_reached then
            (* Here we have to be careful as the last sequence will be consumed
                _after_ the stream has ended *)
            if it.tag <> "" then begin
              it.tag <- "";
              it.seq <- ""
            end else
              ()
          else begin            
            try
              (* The invariant is to have the label of the new sequence loaded in it.next
                  and an empty buffer *)
              it.tag <- it.next;
              it.progr <- it.progr + 1;
              let line = ref "" in
              while line := input_line it.input; it.lines <- it.lines + 1; !line = "" || !line.[0] <> '>' do
                it.linter !line |> Buffer.add_string it.buf
              done;
              it.next <- String.sub !line 1 (String.length !line - 1);
              it.seq <- Buffer.contents it.buf;
              Buffer.clear it.buf              
            with End_of_file ->
              it.next <- "";
              it.seq <- Buffer.contents it.buf;
              delete it
          end
          [@@inline]
        let create (linter, path) =
          let res = { (empty ()) with linter; path; input = open_in path; eof_reached = false } in
          begin try
            (* The invariant is to have the label of the new sequence loaded in it.next
                and an empty buffer *)
            let line = ref "" in
            while line := input_line res.input; res.lines <- res.lines + 1; !line = "" || !line.[0] <> '>' do
              if !line <> "" then
                Exception.raise_malformed __FUNCTION__ res.lines "FASTA" path ~comment:"header expected"
            done;
            res.next <- String.sub !line 1 (String.length !line - 1);
            (* Now that we've got the header, we parse the sequence and the next header *)
            incr res
          with End_of_file ->
            delete res
          end;
          res
        let get it f =
          if it.tag = "" then
            () (* This can also happen at the beginning *)
          else
            f (it.progr, 0, { Base.Read.tag = it.tag; seq = it.seq; qua = "" })
          [@@inline]
        let get_se_pe it f _ = get it f [@@inline]
        let get_and_incr it f =
          let res = get it f in
          incr it;
          res
          [@@inline]
        let get_se_pe_and_incr it f _ = get_and_incr it f [@@inline]
      end
    let iter ?(linter = Sequences.Lint.dnaize ~keep_lowercase:false ~keep_dashes:false) ?(verbose = false) f path =
      if verbose then
        Printf.eprintf "(%s): Reading FASTA file '%s': Begin\n%!" __FUNCTION__ path;
      let it = Iterator.create (linter, path) in
      while Iterator.is_empty it |> not do
        Iterator.get_and_incr it f (* This also calls Iterator.delete() *)
      done;
      if verbose then
        Printf.eprintf "(%s): Reading FASTA file '%s': End\n%!" __FUNCTION__ path
    let parallel_iter ?(linter = Sequences.Lint.dnaize ~keep_lowercase:false ~keep_dashes:false)
                      ?(buffered_chunks_per_thread = 10) ?(max_memory = 1000000000) ?(verbose = false)
                      path (f:int -> (string * string) list -> 'a) (g:'a -> unit) threads =
      let max_block_bytes = max_memory / (buffered_chunks_per_thread * threads) in
      (* Parallel section *)
      let input = open_in path and current = ref "" and seq = Buffer.create 1048576
      and read = ref 0 and seqs = ref 0 and eof_reached = ref false in
      let read_up_to_next_sequence_name () =
        let res = ref "" in
        Buffer.clear seq;
        while not !eof_reached && !res = "" do
          try
            let line = input_line input in
            incr read;
            if line <> "" then begin
              if line.[0] = '>' then
                res := String.sub line 1 (String.length line - 1)
              else
                linter line |> Buffer.add_string seq
            end
          with End_of_file ->
            eof_reached := true;
            close_in input
        done;
        current := !res in
      if verbose then
        Printf.teprintf "0 lines read%!\n";
      read_up_to_next_sequence_name ();
      if Buffer.contents seq <> "" then
        Exception.raise_malformed __FUNCTION__ !read "FASTA" path
          ~comment:(Buffer.contents seq |> Printf.sprintf "found contents '%s' before sequence name");
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
                if seq = "" then
                  Exception.raise_malformed __FUNCTION__ !read "FASTA" path
                    ~comment:(Printf.sprintf "sequence '%s' is empty" current);
                incr seqs;
                List.accum chunk (current, seq);
                bytes := !bytes + String.length current + String.length seq
              done;
              if verbose then
                Printf.teprintf "%d %s read%!\n" !read (String.pluralize_int "line" !read);
              seqs_base, !chunk
            end else
              raise End_of_file)
          (fun (seqs_base, chunk) ->
            let res = List.rev chunk |> f seqs_base in
            if verbose then begin
              let seqs = List.length chunk in
              Printf.teprintf "%d more %s processed%!\n" seqs (String.pluralize_int "sequence" seqs)
            end;
            res)
          g threads
  end

(* At this level we do not care whether the FASTQ is SE, PE or interleaved *)
module FASTQ:
  sig
    (* C++-style iterators *)
    module Iterator: Base.Iterator.Type_t (* Iterator.t is opaque *)
    (* OCaml-style iterator *)
    val iter: string Base.Iterator.t
  end
= struct
    (* We don't constrain the signature here so that the rest of the module can see the type *)
    module Iterator =
      (* Iterator over a single-end non-interleaved FASTQ file *)
      struct
        type t = {
          linter: string -> string;
          path: string;
          input: in_channel;
          mutable lines: int; (* Number of lines read so far *)
          mutable eof_reached: bool;
          (* *)
          mutable progr: int; (* Sequence progressive (from 0) *)
          mutable read: Base.Read.t
        }
        let empty () = {
          linter = Fun.id;
          path = "";
          input = stdin;
          lines = 0;
          eof_reached = true;
          progr = -1;
          read = Base.Read.empty
        }
        let info it = it.path, it.lines
        let is_empty it = it.eof_reached
        let delete it =
          close_in it.input;
          it.eof_reached <- true
          [@@inline]
        let create (linter, path) =
          { (empty ()) with linter; path; input = open_in path; eof_reached = false }
        let incr it =
          if it.eof_reached then
            ()
          else begin
            try
              let tag = input_line it.input in
              it.progr <- it.progr + 1;
              let seq = input_line it.input |> it.linter in
              let tmp = input_line it.input in
              let qua = input_line it.input in
              it.lines <- it.lines + 4;
              if tag = "" || tag.[0] <> '@' || tmp = "" || tmp.[0] <> '+' then
                Exception.raise_malformed __FUNCTION__ it.lines "FASTQ" it.path;
              it.read <- { tag = String.sub tag 1 (String.length tag - 1); seq; qua }
            with End_of_file ->
              if it.lines <> (4 * (it.progr + 1)) then
                (* Last line is truncated *)
                Exception.raise_malformed __FUNCTION__ it.lines "FASTQ" it.path
                  ~comment:"file is truncated";
              delete it
          end
          [@@inline]
        let get it f =
          if it.read.tag = "" then
            () (* This can also happen at the beginning *)
          else
            (* By default, segment ID is zero.
               In case of PE reads we'll manually change this with an adaptor function *)
            f (it.progr, 0, it.read)
          [@@inline]
        let get_se_pe it f _ = get it f [@@inline]
        let get_and_incr it f =
          let res = get it f in
          incr it;
          res
          [@@inline]
        let get_se_pe_and_incr it f _ = get_and_incr it f [@@inline]
      end
    let iter ?(linter = Sequences.Lint.dnaize ~keep_lowercase:false ~keep_dashes:false) ?(verbose = false) f path =
      if verbose then
        Printf.eprintf "(%s): Reading FASTQ file '%s': Begin\n%!" __FUNCTION__ path;
      let it = Iterator.create (linter, path) in
      while Iterator.is_empty it |> not do (* This also calls Iterator.delete() *)
        Iterator.get_and_incr it f
      done;
      if verbose then
        Printf.eprintf "(%s): Reading FASTQ file '%s': End\n%!" __FUNCTION__ path
  end

module Tabular:
  sig
    (* C++-style iterators *)
    module Iterator: Base.Iterator.Type_t (* Iterator.t is opaque *)
    (* OCaml-style iterators *)
    val iter: string Base.Iterator.t
    val iter_se_pe: string Base.Iterator.se_pe_t
  end
= struct
    (* We don't constrain the signature here so that the rest of the module can see the type *)
    module Iterator =
      struct
        type t = {
          linter: string -> string;
          path: string;
          input: in_channel;
          mutable eof_reached: bool;
          (* *)
          mutable progr: int; (* Sequence progressive (from 0) *)
          mutable line: string array
        }
        let empty () = {
          linter = Fun.id;
          path = "";
          input = stdin;
          eof_reached = true;
          progr = -1;
          line = [||]
        }
        let info it = it.path, it.progr + 1
        let is_empty it = it.eof_reached
        let delete it =
          close_in it.input;
          it.eof_reached <- true
          [@@inline]
        let create (linter, path) =
          { (empty ()) with linter; path; input = open_in path; eof_reached = false }
        let incr it =
          if it.eof_reached then
            ()
          else begin
            try
              it.line <- input_line it.input |> String.Split.on_char_as_array '\t';
              it.progr <- it.progr + 1;
              begin match Array.length it.line with
              | 2 | 3 -> (* FASTA or SE FASTQ *)
                it.line.(1) <- it.linter it.line.(1)
              | 6 -> (* PE FASTQ *)
                it.line.(1) <- it.linter it.line.(1);
                it.line.(4) <- it.linter it.line.(4)
              | n ->
                Exception.raise_malformed __FUNCTION__ it.progr "tabular" it.path
                  ~comment:(Printf.sprintf "found %d fields, expected 2, 3, or 6" n)
              end
            with End_of_file ->
              delete it
          end
          [@@inline]
        let get_se_pe it f g =
          if it.line = [||] then
            () (* This can also happen at the beginning *)
          else match Array.length it.line with
          | 2 -> (* FASTA *)
            f (it.progr, 0, { Base.Read.tag = it.line.(0); seq = it.line.(1); qua = "" })
          | 3 -> (* SE FASTQ *)
            f (it.progr, 0, { tag = it.line.(0); seq = it.line.(1); qua = it.line.(2) })
          | 6 -> (* PE FASTQ *)
            g (it.progr, 0, { Base.Read.tag = it.line.(0); seq = it.line.(1); qua = it.line.(2) })
              (it.progr, 1, { Base.Read.tag = it.line.(3); seq = it.line.(4); qua = it.line.(5) })
          | _ -> assert false
          [@@inline]
        let get it f = get_se_pe it f (fun one two -> f one; f two) [@@inline]
        let get_se_pe_and_incr it f g =
          let res = get_se_pe it f g in
          incr it;
          res
          [@@inline]
        let get_and_incr it f = get_se_pe_and_incr it f (fun one two -> f one; f two) [@@inline]
      end
    let iter_se_pe ?(linter = Sequences.Lint.dnaize ~keep_lowercase:false ~keep_dashes:false) ?(verbose = false)
                   f g path =
      if verbose then
        Printf.eprintf "(%s): Reading tabular file '%s': Begin\n%!" __FUNCTION__ path;
      let it = Iterator.create (linter, path) in
      while Iterator.is_empty it |> not do (* This also calls Iterator.delete() *)
        Iterator.get_se_pe_and_incr it f g
      done;
      if verbose then
        Printf.eprintf "(%s): Reading tabular file '%s': End\n%!" __FUNCTION__ path
    let iter ?(linter = Sequences.Lint.dnaize ~keep_lowercase:false ~keep_dashes:false) ?(verbose = false) f =
      iter_se_pe ~linter ~verbose f (fun one two -> f one; f two) [@@inline]
  end

(* An abstraction of FAST* and tabular files.
   Note that not only do we provide regular OCaml-like iterators, but also C++-like ones
    allowing to read from the file(s) and process one record at the time *)
module Reads:
  sig
    type t =
      | FASTA of string
      | SingleEndFASTQ of string
      | PairedEndFASTQ of string * string
      | InterleavedFASTQ of string
      | Tabular of string
    module Iterator: (* C++-style iterators *)
      sig
        (* Initialiser type is linter and type *)
        type init_t = Base.linter_t * t
        (* Return type is read ID, segment ID, and (tag, sequence, qualities) *)
        type ret_t = int * int * Base.Read.t
        include Iterator_t with type init_t := init_t and type ret_t := ret_t
      end
    (* OCaml-style iterators *)
    val iter: t Base.Iterator.t
    val iter_se_pe: t Base.Iterator.se_pe_t
  end
= struct
    type t =
      | FASTA of string
      | SingleEndFASTQ of string
      | PairedEndFASTQ of string * string
      | InterleavedFASTQ of string
      | Tabular of string
    module Iterator =
      struct
        type init_t = Base.linter_t * t
        type ret_t = int * int * Base.Read.t
        type t =
          | ItEmpty
          | ItFASTA of FASTA.Iterator.t
          | ItSingleEndFASTQ of FASTQ.Iterator.t
          | ItPairedEndFASTQ of FASTQ.Iterator.t * FASTQ.Iterator.t
          (* Only the second of the two ends is stored in the iterator *)
          | ItInterleavedFASTQ of Base.Read.t ref * FASTQ.Iterator.t
          | ItTabular of Tabular.Iterator.t
        let empty () = ItEmpty
        let is_empty = function
          | ItEmpty -> true
          | ItFASTA it ->
            FASTA.Iterator.is_empty it
          | ItSingleEndFASTQ it ->
            FASTQ.Iterator.is_empty it
          | ItPairedEndFASTQ (it1, it2) ->
            let is_empty1 = FASTQ.Iterator.is_empty it1 and is_empty2 = FASTQ.Iterator.is_empty it2 in
            assert (is_empty1 = is_empty2);
            is_empty1
          | ItInterleavedFASTQ (read, _) ->
            !read.tag = ""
          | ItTabular it ->
            Tabular.Iterator.is_empty it
        let delete = function
          | ItEmpty -> ()
          | ItFASTA it ->
            FASTA.Iterator.delete it
          | ItSingleEndFASTQ it | ItInterleavedFASTQ (_, it) ->
            FASTQ.Iterator.delete it
          | ItPairedEndFASTQ (it1, it2) ->
            FASTQ.Iterator.delete it1;
            FASTQ.Iterator.delete it2
          | ItTabular it ->
            Tabular.Iterator.delete it
        let create (linter, t) =
          match t with
          | FASTA path ->
            ItFASTA (FASTA.Iterator.create (linter, path))
          | SingleEndFASTQ path ->
            ItSingleEndFASTQ (FASTQ.Iterator.create (linter, path))
          | PairedEndFASTQ (path1, path2) ->
            ItPairedEndFASTQ (FASTQ.Iterator.create (linter, path1),
                              FASTQ.Iterator.create (linter, path2))
          | InterleavedFASTQ path ->
            ItInterleavedFASTQ (ref Base.Read.empty, FASTQ.Iterator.create (linter, path))
          | Tabular path ->
            ItTabular (Tabular.Iterator.create (linter, path))
        let incr = function
          | ItEmpty -> ()
          | ItFASTA it -> FASTA.Iterator.incr it
          | ItSingleEndFASTQ it -> FASTQ.Iterator.incr it
          | ItPairedEndFASTQ (it1, it2) ->
            FASTQ.Iterator.incr it1;
            FASTQ.Iterator.incr it2;
            if FASTQ.Iterator.is_empty it1 <> FASTQ.Iterator.is_empty it2 then
              let path, lines = FASTQ.Iterator.info it2 in
              Exception.raise_malformed __FUNCTION__ lines "FASTQ" path
                ~comment:"files have a different length"
          | ItInterleavedFASTQ (read, it) ->
            FASTQ.Iterator.get_and_incr it (fun (_, _, r) -> read := r);
            FASTQ.Iterator.incr it
          | ItTabular it ->
            Tabular.Iterator.incr it
          [@@inline]
        let get_se_pe it f g =
          match it with
          | ItEmpty -> ()
          | ItFASTA it ->
            FASTA.Iterator.get it f
          | ItSingleEndFASTQ it ->
            FASTQ.Iterator.get it f
          | ItPairedEndFASTQ (it1, it2) ->
            let read1 = ref Base.Read.empty in
            FASTQ.Iterator.get it1 (fun (_, _, read) -> read1 := read);
            FASTQ.Iterator.get it2 (fun (progr, _, read2) -> g (progr, 0, !read1) (progr, 1, read2))
          | ItInterleavedFASTQ (read1, it) ->
            let _, lines = FASTQ.Iterator.info it in
            let progr = lines / 8 - 1 in
            FASTQ.Iterator.get it (fun (_, _, read2) -> g (progr, 0, !read1) (progr, 1, read2))
          | ItTabular it ->
            Tabular.Iterator.get_se_pe it f g
          [@@inline]
        let get it f = get_se_pe it f (fun one two -> f one; f two)
        let get_se_pe_and_incr it f g =
          let res = get_se_pe it f g in
          incr it;
          res
          [@@inline]
        let get_and_incr it f = get_se_pe_and_incr it f (fun one two -> f one; f two) [@@inline]
      end
    let iter_se_pe ?(linter = Sequences.Lint.dnaize ~keep_lowercase:false ~keep_dashes:false) ?(verbose = false)
                   f g file =
      let what =
        match file with
        | FASTA path ->
          Printf.sprintf "FASTA file '%s'" path
        | SingleEndFASTQ path ->
          Printf.sprintf "single-end FASTQ file '%s'" path
        | PairedEndFASTQ (path1, path2) ->
          Printf.sprintf "paired-end FASTQ files '%s' and '%s'" path1 path2
        | InterleavedFASTQ path ->
          Printf.sprintf "interleaved FASTQ file '%s'" path
        | Tabular path ->
          Printf.sprintf "tabular file '%s'" path in
      if verbose then
        Printf.eprintf "(%s): Reading %s: Begin\n%!" __FUNCTION__ what;
      let it = Iterator.create (linter, file) in
      while Iterator.is_empty it |> not do
        Iterator.get_se_pe_and_incr it f g (* This also calls Iterator.delete() *)
      done;
      if verbose then
        Printf.eprintf "(%s): Reading %s: End\n%!" __FUNCTION__ what
    let iter ?(linter = Sequences.Lint.dnaize ~keep_lowercase:false ~keep_dashes:false) ?(verbose = false) f =
      iter_se_pe ~linter ~verbose f (fun one two -> f one; f two) [@@inline]
    module [@warning "-32"] Store:
      sig
        type reads_t := t
        type t
        (* A filter is something that separates reads into (singletons, selected, leftovers) *)
        val singleton: int
        val selected: int
        val unmarked: int
        type filter_t = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
        val empty: t
        val length: t -> int
        val add_from_file: ?linter:Base.linter_t -> ?verbose:bool -> t -> reads_t -> unit
        (* Arguments to the function are store, optional filter (can be empty), name of output prefix
            (output reads can be FASTA and/or FASTQ SE and/or FASTQ PE) *)
        val to_fast: ?verbose:bool -> t -> filter_t -> string -> unit
        val to_tabular: ?verbose:bool -> t -> filter_t -> string -> unit
        val seq_length: t -> int
        (* Arguments to the function are read id, segment id, payload *)
        val iter: (Iterator.ret_t -> unit) -> t -> unit
      end
    = struct
        type template_t =
          | SingleEndRead of Base.Read.t
          | PairedEndRead of Base.Read.t * Base.Read.t
        type t = template_t Tools.ArrayStack.t
        let singleton = 0
        let selected = 1
        let unmarked = 2
        type filter_t = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
        let empty = Tools.ArrayStack.create ()
        let iter f =
          Tools.ArrayStack.riteri
            (fun templ_i -> function
              | SingleEndRead segm ->
                f (templ_i, 0, segm)
              | PairedEndRead (segm1, segm2) ->
                f (templ_i, 0, segm1);
                f (templ_i, 1, segm2))
        let length = Tools.ArrayStack.length
        let seq_length store =
          let res = ref 0 in
          iter (fun (_, _, segm) -> res := !res + String.length segm.seq) store;
          !res
        let add_from_file ?(linter = Sequences.Lint.dnaize ~keep_lowercase:false ~keep_dashes:false) ?(verbose = false)
                          store file =
          iter_se_pe ~linter ~verbose
            (fun (_, _, read) ->
              SingleEndRead read |> Tools.ArrayStack.push store)
            (fun (_, _, read1) (_, _, read2) ->
              PairedEndRead (read1, read2) |> Tools.ArrayStack.push store)
            file;
          if verbose then
            Printf.eprintf "(%s): %d reads in store so far (total length %d)\n%!"
              __FUNCTION__ (Tools.ArrayStack.length store) (seq_length store)
        let raise_invalid_filter_length __FUNCTION__ num_reads len =
          Exception.raise __FUNCTION__ Algorithm
            (Printf.sprintf
              "Filter length must be zero or the same as the number of reads, %d (found %d)" num_reads len)
        let to_fast ?(verbose = false) store filter prefix =
          let len = Tools.ArrayStack.length store and f_len = Bigarray.Array1.dim filter in
          (* The filter can be empty *)
          if f_len <> len && f_len <> 0 then
            raise_invalid_filter_length __FUNCTION__ len f_len;
          let print_fastq_record_filtered classification output read =
            Printf.fprintf output "@%d__%s\n%s\n+\n%s\n" classification read.Base.Read.tag read.seq read.qua
          and print_fastq_record output read =
            Printf.fprintf output "@%s\n%s\n+\n%s\n" read.Base.Read.tag read.seq read.qua
          and output0 = open_out (prefix ^ ".fasta")
          and output1 = open_out (prefix ^ "_SE.fastq")
          and output2 = [| open_out (prefix ^ "_PE_1.fastq"); open_out (prefix ^ "_PE_2.fastq") |] in
          if verbose then
            Printf.eprintf "(%s): Writing %d reads...%!" __FUNCTION__ len;
          Tools.ArrayStack.riteri begin
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
        let to_tabular ?(verbose = false) store filter path =
          let len = Tools.ArrayStack.length store and f_len = Bigarray.Array1.dim filter in
          (* The filter can be empty *)
          if f_len <> len && f_len <> 0 then
            raise_invalid_filter_length __FUNCTION__ len f_len;
          let output = open_out path in
          if verbose then
            Printf.eprintf "(%s): Writing %d reads...%!" __FUNCTION__ len;
          Tools.ArrayStack.riteri begin
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
  end

