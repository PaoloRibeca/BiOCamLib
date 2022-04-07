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

module Misc:
  sig
    val dnaize_bytes: ?keep_dashes:bool -> bytes -> bytes
    val rc_bytes: ?keep_dashes:bool -> bytes -> bytes

    val dnaize: ?keep_dashes:bool -> string -> string
    val rc: ?keep_dashes:bool -> string -> string

  end
= struct
    let dnaize_bytes ?(keep_dashes = false) b =
      let len = Bytes.length b in
      let red_len = len - 1 in
      for i = 0 to red_len do
        Bytes.set b i begin
          match Bytes.get b i with
          | 'A' | 'a' -> 'A'
          | 'C' | 'c' -> 'C'
          | 'G' | 'g' -> 'G'
          | 'T' | 't' -> 'T'
          | '-' when keep_dashes -> '-'
          | _ -> 'N'
        end
      done;
      b
    let dnaize ?(keep_dashes = false) s =
      Bytes.to_string (dnaize_bytes ~keep_dashes (Bytes.of_string s))
    let rc_bytes ?(keep_dashes = false) b =
      let compl = function
        | 'A' | 'a' -> 'T'
        | 'C' | 'c' -> 'G'
        | 'G' | 'g' -> 'C'
        | 'T' | 't' -> 'A'
        | '-' when keep_dashes -> '-'
        | _ -> 'N' in
      let len = Bytes.length b in
      let red_len = len - 1 in
      let half_len = red_len / 2 in
      for i = 0 to half_len do
        let idx = red_len - i in
        let c = Bytes.get b i in
        Bytes.set b i (compl (Bytes.get b idx));
        Bytes.set b idx (compl c)
      done;
      b
    let rc ?(keep_dashes = false) s =
      Bytes.to_string (rc_bytes ~keep_dashes (Bytes.of_string s))


  end

module FASTA:
  sig
    val iter: ?linter:(string -> string) -> ?verbose:bool -> (int -> string -> string -> unit) -> string -> unit
    val parallel_iter: ?linter:(string -> string) -> ?buffered_chunks_per_thread:int ->
                       ?max_memory:int -> ?verbose:bool ->
      string -> (int -> (string * string) list -> 'a) -> ('a -> unit) -> int -> unit
  
  end
= struct
    let iter ?(linter = Misc.dnaize ~keep_dashes:false) ?(verbose = true) f filename =
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
          if line.[0] = '>' then begin
            process_current ();
            current := String.sub line 1 (String.length line - 1)
          end else
            linter line |> Buffer.add_string seq
        done
      with End_of_file ->
        process_current ();
        close_in file;
        if verbose then
          Printf.eprintf " done.\n%!"
      let parallel_iter ?(linter = Misc.dnaize ~keep_dashes:false) ?(buffered_chunks_per_thread = 10)
                      ?(max_memory = 1000000000) ?(verbose = true)
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
          Printf.sprintf "(%s): On line %d: Malformed FASTA file (found content '%s' before sequence name)" __FUNCTION__ !read
          |> failwith;
      if not !eof_reached then
        Tools.Parallel.process_stream_chunkwise ~buffered_chunks_per_thread:buffered_chunks_per_thread
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
                  Printf.sprintf "(%s): On line %d: Malformed FASTA file (sequence '%s' is empty)" __FUNCTION__ !read current
                  |> failwith
                end;
                incr seqs;
                Tools.Misc.accum chunk (current, seq);
                bytes := !bytes + String.length current + String.length seq
              done;
              if verbose then
                Tools.Printf.teprintf "%d %s read%!\n" !read (Tools.Misc.pluralize_int "line" !read);
              seqs_base, !chunk
            end else
              raise End_of_file)
          (fun (seqs_base, chunk) ->
            let res = List.rev chunk |> f seqs_base in
            if verbose then begin
              let seqs = List.length chunk in
              Tools.Printf.teprintf "%d more %s processed%!\n" seqs (Tools.Misc.pluralize_int "sequence" seqs)
            end;
            res)
          g threads

  end

module FASTQ:
  sig
    val iter_se: ?linter:(string -> string) -> ?verbose:bool -> (int -> string -> string -> string -> unit) -> string -> unit
    val iter_pe: ?linter:(string -> string) -> ?verbose:bool ->
      (int -> string -> string -> string -> string -> string -> string -> unit) -> string -> string -> unit

  end
= struct
    let iter_se ?(linter = Misc.dnaize ~keep_dashes:false) ?(verbose = true) f file =
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
            Printf.sprintf "(%s): On line %d: Malformed FASTQ file" __FUNCTION__ !read |> failwith;
          f (!read / 4) (String.sub tag 1 (String.length tag - 1)) (linter seq) qua
        done
      with End_of_file -> ()
      end;
      close_in input;
      if verbose then
        Printf.eprintf " done.\n%!"
    let iter_pe ?(linter = Misc.dnaize ~keep_dashes:false) ?(verbose = true) f file1 file2 =
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
          read := !read + 4;
          if tag1.[0] <> '@' || tmp1.[0] <> '+' || tag2.[0] <> '@' || tmp2.[0] <> '+' then
            Printf.sprintf "(%s): On line %d: Malformed FASTQ file" __FUNCTION__ !read |> failwith;
          f (!read / 4)
            (String.sub tag1 1 (String.length tag1 - 1)) (linter seq1) qua1
            (String.sub tag2 1 (String.length tag2 - 1)) (linter seq2) qua2
        done
      with End_of_file -> ()
      end;
      close_in input1;
      close_in input2;
      if verbose then
        Printf.eprintf " done.\n%!"

  end

module Types =
  struct
    (* All coordinates are zero-based internally and one-based externally *)
    type zero_based_coord_t = int
    let coord_of_string: string -> zero_based_coord_t =
      fun s -> int_of_string s - 1
    let string_of_coord (i:zero_based_coord_t) =
      string_of_int (i + 1)
    (* We define a generic type to give things a direction *)
    type 'a stranded_t = Forward of 'a | Reverse of 'a (*| None of 'a*)
    type strand_t = unit stranded_t
    let forward = Forward ()
    let reverse = Reverse ()
    let strand_of_string = function
      | "F" | "f" | "+" | "forward" | "Forward" -> forward
      | "R" | "r" | "-" | "reverse" | "Reverse" -> reverse
      | s -> failwith ("GenomeTypes.strand_of_string: Unrecognized strand '" ^ s ^ "'")
    let string_of_strand = function
      | Forward _ -> "+"
      | Reverse _ -> "-"
    let split_of_stranded = function
      | Forward unstranded -> forward, unstranded
      | Reverse unstranded -> reverse, unstranded
    let stranded_of_split strand unstranded =
      match strand with
      | Forward () -> Forward unstranded
      | Reverse () -> Reverse unstranded
    let string_of_stranded_string ss =
      let strand, name = split_of_stranded ss in
      Printf.sprintf "%s:%s" name (string_of_strand strand)
    type pointer_t = { name: string;
                       position: zero_based_coord_t }
    (* We silently convert from 0- to 1-based *)
    let string_of_pointer obj =
      Printf.sprintf "%s:%s" obj.name (string_of_coord obj.position)
    type stranded_pointer_t = { name: string stranded_t;
                                position: zero_based_coord_t }
    (* We silently convert from 0- to 1-based *)
    let string_of_stranded_pointer str_ptr =
      let str, name = split_of_stranded str_ptr.name in
      Printf.sprintf "%s:%s:%s" name (string_of_strand str) (string_of_coord str_ptr.position)
    (* We silently convert from 1- to 0-based *)
    let stranded_pointer_of_string s =
      try
        let fields = Tools.Split.on_char_as_array ':' s in
        if Array.length fields <> 3 then
          raise Exit;
        let str = strand_of_string fields.(1)
        and pos = coord_of_string fields.(2) in
        if pos < 0 then
          raise Exit;
        { name = stranded_of_split str fields.(0); position = pos }
      with _ -> failwith "Sequences.Types.stranded_pointer_of_string: Wrong syntax"
    (*
    module StrandedPointerSet =
      Set.Make (Tools.MakeComparable (struct type t = stranded_pointer_t end))
    module StrandedPointerMap =
      Map.Make (Tools.MakeComparable (struct type t = stranded_pointer_t end))
    *)
    type simple_interval_t = { low: zero_based_coord_t;
                               length: int }
    type interval_t = { low: pointer_t;
                        length: int }
    (* We silently convert from 0- to 1-based *)
    let string_of_interval obj =
      Printf.sprintf "%s:%s:%d" obj.low.name (string_of_coord obj.low.position) obj.length
    type stranded_interval_t = { low: stranded_pointer_t;
                                 length: int }
    let make_stranded_interval stranded_name position length = {
      low = { name = stranded_name; position = position };
      length = length
    }
    (* We silently convert from 0- to 1-based *)
    let string_of_stranded_interval str_ivl =
      let str, name = split_of_stranded str_ivl.low.name in
      Printf.sprintf "%s:%s:%s:%d"
        name (string_of_strand str) (string_of_coord str_ivl.low.position) str_ivl.length
    (* The module of things associated with a set of stranded sequence names *)
    module StrandedStringMap = Map.Make (Tools.MakeComparable (struct type t = string stranded_t end))

  end

module Junctions:
  sig
    val parse: ?default_coverage:float -> (int -> string Types.stranded_t -> int -> int -> float -> unit) -> string -> unit
  end
= struct
    (* Helper function to parse what is produced by the GEM pipeline *)
    let parse ?(default_coverage = 0.) f introns =
      let introns = open_in introns and cntr = ref 0 in
      try
        while true do
          let line = Tools.Split.on_char_as_array '\t' (input_line introns) in
          incr cntr;
          let error what =
            Printf.sprintf "Sequences.Introns.parse: On line %d: %s\n%!" !cntr what |> failwith in
          (* Format is: <name_1> <str_1> <pos_1> <name_2> <str_2> <pos_2> [<cov>],
              or:       <name> <str> <pos_1> <pos_2> [<cov>] *)
          let stranded_name, pos_don, pos_acc, cov =
            let len = Array.length line in
            match len with
            | 4 | 5 ->
              begin try
                let dir = Types.strand_of_string line.(1)
                and pos_don = int_of_string line.(2)
                and pos_acc = int_of_string line.(3)
                and cov =
                  if len = 5 then
                    float_of_string line.(4)
                  else
                    default_coverage in
                (Types.stranded_of_split dir line.(0)), pos_don, pos_acc, cov            
              with _ ->
                error "Incorrect syntax"
              end
            | 6 | 7 ->
              begin try
                let dir_don = Types.strand_of_string line.(1)
                and pos_don = int_of_string line.(2)
                and dir_acc = Types.strand_of_string line.(4)
                and pos_acc = int_of_string line.(5)
                and cov =
                  if len = 7 then
                    float_of_string line.(6)
                  else
                    default_coverage in
                if line.(0) <> line.(3) || dir_don <> dir_acc then
                  raise Exit;
                (Types.stranded_of_split dir_don line.(0)), pos_don, pos_acc, cov
              with _ ->
                error "Incorrect syntax"
              end
            | _ ->
              Printf.sprintf "Invalid number of fields (%d)" len |> error in
          if pos_don < 0 || pos_acc < 0 || cov < 0. then
            error "Negative coordinates or coverage";
          f !cntr stranded_name pos_don pos_acc cov
        done
      with End_of_file -> close_in introns
  end
  
module Translation:
  sig
    type t =
      | Table_1 | Table_2 | Table_3 | Table_4 | Table_5 | Table_6
      | Table_9 | Table_10 | Table_11 | Table_12 | Table_13 | Table_14
      | Table_16 | Table_21 | Table_22 | Table_23 | Table_24 | Table_25
      | Table_26 | Table_27 | Table_28 | Table_29 | Table_30 | Table_31
    val of_string: string -> t
    val get_stops: ?frames:int list -> t -> string -> Tools.IntSet.t
    val get_starts:
          ?frames:int list -> ?get_alternative_start_codons:bool -> t -> string -> Tools.IntSet.t
    val get_translations:
          ?get_alternative_start_codons: bool ->
          ?replace_alternative_start_codons_with_methionine:bool ->
          (* If several start codons are present before a stop
              we just return the largest possible translation *)
          ?only_largest_product:bool -> ?min_length:int ->
          t -> string -> (int * string) array
  end =
  struct
    type t =
      | Table_1 | Table_2 | Table_3 | Table_4 | Table_5 | Table_6
      | Table_9 | Table_10 | Table_11 | Table_12 | Table_13 | Table_14
      | Table_16 | Table_21 | Table_22 | Table_23 | Table_24 | Table_25
      | Table_26 | Table_27 | Table_28 | Table_29 | Table_30 | Table_31
    let of_string = function
      | "1" | "Table1" | "Table_1" -> Table_1
      | "2" | "Table2" | "Table_2" -> Table_2
      | "3" | "Table3" | "Table_3" -> Table_3
      | "4" | "Table4" | "Table_4" -> Table_4
      | "5" | "Table5" | "Table_5" -> Table_5
      | "6" | "Table6" | "Table_6" -> Table_6
      | "9" | "Table9" | "Table_9" -> Table_9
      | "10" | "Table10" | "Table_10" -> Table_10
      | "11" | "Table11" | "Table_11" -> Table_11
      | "12" | "Table12" | "Table_12" -> Table_12
      | "13" | "Table13" | "Table_13" -> Table_13
      | "14" | "Table14" | "Table_14" -> Table_14
      | "16" | "Table16" | "Table_16" -> Table_16
      | "21" | "Table21" | "Table_21" -> Table_21
      | "22" | "Table22" | "Table_22" -> Table_22
      | "23" | "Table23" | "Table_23" -> Table_23
      | "24" | "Table24" | "Table_24" -> Table_24
      | "25" | "Table25" | "Table_25" -> Table_25
      | "26" | "Table26" | "Table_26" -> Table_26
      | "27" | "Table27" | "Table_27" -> Table_27
      | "28" | "Table28" | "Table_28" -> Table_28
      | "29" | "Table29" | "Table_29" -> Table_29
      | "30" | "Table30" | "Table_30" -> Table_30
      | "31" | "Table31" | "Table_31" -> Table_31
      | w -> failwith ("Invalid translation table '" ^ w ^ "'")
    let [@warning "-32"] describe = function
      | Table_1 -> "Standard"
      | Table_2 -> "VertebrateMitochondrial"
      | Table_3 -> "YeastMitochondrial"
      | Table_4 -> "MoldMitochondrial|ProtozoanMitochondrial|CoelenterateMitochondrial|Mycoplasma|Spiroplasma"
      | Table_5 -> "InvertebrateMitochondrial"
      | Table_6 -> "CiliateNuclear|DasycladaceanNuclear|HexamitaNuclear"
      | Table_9 -> "EchinodermMitochondrial|FlatwormMitochondrial"
      | Table_10 -> "EuplotidNuclear"
      | Table_11 -> "Bacterial|Archaeal|PlantPlastid"
      | Table_12 -> "AlternativeYeastNuclear"
      | Table_13 -> "AscidianMitochondrial"
      | Table_14 -> "AlternativeFlatwormMitochondrial"
      | Table_16 -> "ChlorophyceanMitochondrial"
      | Table_21 -> "TrematodeMitochondrial"
      | Table_22 -> "Scenedesmus.obliquusMitochondrial"
      | Table_23 -> "ThraustochytriumMitochondrial"
      | Table_24 -> "PterobranchiaMitochondrial"
      | Table_25 -> "CandidateDivisionSR1|Gracilibacteria"
      | Table_26 -> "Pachysolen.tannophilusNuclear"
      | Table_27 -> "KaryorelictNuclear"
      | Table_28 -> "CondylostomaNuclear"
      | Table_29 -> "MesodiniumNuclear"
      | Table_30 -> "PeritrichNuclear"
      | Table_31 -> "BlastocrithidiaNuclear"
    let iterate ?(frames = [0; 1; 2]) f s =
      let max_pos = String.length s - 3 in
      List.iter
        (fun frame ->
          let i = ref frame in
          while !i <= max_pos do
            f !i (String.sub s !i 3);
            i := !i + 3
          done)
        frames
    module IntSet = Tools.IntSet
    let get_starts ?(frames = [0; 1; 2]) ?(get_alternative_start_codons = false) table s =
      let starts = ref IntSet.empty in
      let add pos =
        starts := IntSet.add pos !starts in
      iterate ~frames
        (fun pos triplet ->
          match table with
          | Table_1 ->
            begin match triplet, get_alternative_start_codons with
            | "ATG", _ | "CTG", true | "TTG", true -> add pos
            | _ -> ()
            end
          | _ ->
            failwith "Translation table not yet implemented, sorry")
        s;
      !starts
    let get_stops ?(frames = [0; 1; 2]) table s =
      let stops = ref IntSet.empty in
      let add pos =
        stops := IntSet.add pos !stops in
      iterate ~frames
        (fun pos ->
          (* To avoid problems with long stretches of Ns
              it is necessary to add NNN to all cases *)
          match table with
          | Table_1 | Table_11 | Table_12 | Table_26 | Table_28 ->
            begin function
            | "TAA" | "TAG" | "TGA" | "NNN" -> add pos
            | _ -> ()
            end
          | Table_2 ->
            begin function
            | "TAA" | "TAG" | "AGA" | "AGG" | "NNN" -> add pos
            | _ -> ()
            end
          | Table_3 | Table_4 | Table_5 | Table_9 | Table_10 | Table_13
          | Table_21 | Table_24 | Table_25 | Table_31 ->
            begin function
            | "TAA" | "TAG" | "NNN" -> add pos
            | _ -> ()
            end
          | Table_6 | Table_27 | Table_29 | Table_30 ->
            begin function
            | "TGA" | "NNN" -> add pos
            | _ -> ()
            end
          | Table_14 ->
            begin function
            | "TAG" | "NNN" -> add pos
            | _ -> ()
            end
          | Table_16 ->
            begin function
            | "TAA" | "TGA" | "NNN" -> add pos
            | _ -> ()
            end
          | Table_22 ->
            begin function
            | "TCA" | "TAA" | "TGA" | "NNN" -> add pos
            | _ -> ()
            end
          | Table_23 ->
            begin function
            | "TTA" | "TAA" | "TAG" | "TGA" | "NNN" -> add pos
            | _ -> ()
            end)
        s;
      !stops
    module IntMap = Tools.IntMap
    type feature_t =
      | StartCodon
      | StopCodon
    (* The translation is only done in frame *)
    let get_translations
        ?(get_alternative_start_codons = false)
        ?(replace_alternative_start_codons_with_methionine = false)
        ?(only_largest_product = true) ?(min_length = 0) table s =
      let starts = get_starts ~frames:[0] ~get_alternative_start_codons table s
      and stops = get_stops ~frames:[0] table s in
      let starts =
        if only_largest_product then begin
          let res = ref IntSet.empty in
          (* We need to do the selection frame by frame *)
          List.iter
            (fun frame ->
              let sorted = ref IntMap.empty in
              IntSet.iter
                (fun start ->
                  if start mod 3 = frame then
                    sorted := IntMap.add start StartCodon !sorted)
                starts;
              IntSet.iter
                (fun stop ->
                  if stop mod 3 = frame then
                    sorted := IntMap.add stop StopCodon !sorted)
                stops;
              let last = ref None in
              IntMap.iter
                (fun pos what ->
                  begin match !last, what with
                  | Some StartCodon, StartCodon ->
                    (* If the start codon comes after another one we do not add it *)
                    ()
                  | _, StartCodon ->
                    res := IntSet.add pos !res
                  | _ -> ()
                  end;
                  last := Some what)
                !sorted)
            [0];
          !res
        end else
          starts in
(*
Printf.eprintf "String=%s\n%!" s;
IntSet.iter (Printf.eprintf "Stop=%d\n%!") stops;
*)
      let buf = Buffer.create 1024 and results = ref [] in
      IntSet.iter
        (fun start ->
(*
Printf.eprintf "<<<Start=%d\n%!" start;
*)
          (* For each start there is one possible translation *)
          Buffer.clear buf;
          try
            iterate ~frames:[0]
              (fun pos triplet ->
                if pos = 0 && replace_alternative_start_codons_with_methionine then
                  Buffer.add_char buf 'M'
                else
                  begin match table with
                  | Table_1 -> Buffer.add_char buf
                    begin match triplet with
                    | "TAA" | "TAG" | "TGA" -> '*'
                    | "GCA" | "GCC" | "GCG" | "GCT" -> 'A'
                    | "TGC" | "TGT" -> 'C'
                    | "GAC" | "GAT" -> 'D'
                    | "GAA" | "GAG" -> 'E'
                    | "TTC" | "TTT" -> 'F'
                    | "GGA" | "GGC" | "GGG" | "GGT" -> 'G'
                    | "CAC" | "CAT" -> 'H'
                    | "ATA" | "ATC" | "ATT" -> 'I'
                    | "AAA" | "AAG" -> 'K'
                    | "CTA" | "CTC" | "CTG" | "CTT" | "TTA" | "TTG" -> 'L'
                    | "ATG" -> 'M'
                    | "AAC" | "AAT" -> 'N'
                    | "CCA" | "CCC" | "CCG" | "CCT" -> 'P'
                    | "CAA" | "CAG" -> 'Q'
                    | "AGA" | "AGG" | "CGA" | "CGC" | "CGG" | "CGT" -> 'R'
                    | "AGC" | "AGT" | "TCA" | "TCC" | "TCG" | "TCT" -> 'S'
                    | "ACA" | "ACC" | "ACG" | "ACT" -> 'T'
                    | "GTA" | "GTC" | "GTG" | "GTT" -> 'V'
                    | "TGG" -> 'W'
                    | "TAC" | "TAT" -> 'Y'
                    (* An assertion will not work here, because of course
                        there can be some junk in the assembly *)
                    | _ -> 'X' (*assert false*)
                    end
                  | _ -> failwith "Translation table not yet implemented"
                  end;
(*
Printf.eprintf "%d>>>%s\n%!" pos (Buffer.contents buf);
*)
                if IntSet.mem (start + pos) stops then
                  raise Exit)
              (String.sub s start (String.length s - start))
          with Exit ->
(*
Printf.eprintf "!!!%s\n%!" (Buffer.contents buf);
*)
            let contents = Buffer.contents buf in
            if String.length contents >= min_length then
              Tools.Misc.accum results (start, contents))
        starts;
      Array.of_list (List.rev !results)
  end

(* A map (stranded name->string, translation table) from one or more multi-FASTA file,
    and a few additional things *)
module Reference:
  sig
    type t
    val empty: t
    (* The optional argument is a file containing translation tables.
        If they are absent, Table_1 (Standard) is assumed *)
    val add_from_fasta: ?tables:string -> t -> string -> t
    val find: t -> string Types.stranded_t -> string * Translation.t
    val length: t -> string Types.stranded_t -> int
    val get_sequence: t -> Types.stranded_interval_t -> string
    val get_table: t -> Types.stranded_interval_t -> Translation.t
    val get_sequence_and_table: t -> Types.stranded_interval_t -> string * Translation.t
  end
= struct
    (* We explicitly separate forward and reverse sequences *)
    module StrandedStringMap = Types.StrandedStringMap
    (* To each sequence name we associate a sequence and a translation table *)
    type t = (string * Translation.t) StrandedStringMap.t
    let empty = StrandedStringMap.empty
    module StringMap = Map.Make(String)
    let add_from_fasta ?(tables = "") obj input =
      let tables =
        let res = ref StringMap.empty in
        if tables <> "" then begin
          let tables = open_in tables and cntr = ref 0 in
          try
            while true do
              let line = Tools.Split.on_char_as_array '\t' (input_line tables) in
              incr cntr;
              (* Format is <name> <table> *)
              let name, table =
                try
                  let len = Array.length line in
                  if len <> 2 then
                    raise Exit;
                  let table = Translation.of_string line.(1) in
                  line.(0), table
                with _ ->
                  failwith begin
                    "Reference.add_from_fasta: On line " ^ string_of_int !cntr ^
                    ": Incorrect translation table file syntax"
                  end in
              res := StringMap.add name table !res
            done
          with End_of_file -> ()
        end;
        !res in
      let input = open_in input
      and name = ref "" and seq = Buffer.create 128 and res = ref obj in
      let process_seq () =
        if !name <> "" && Buffer.length seq > 0 then begin
          let table =
            if StringMap.cardinal tables = 0 then
              Translation.Table_1
            else
              try
                StringMap.find !name tables
              with _ ->
                failwith begin
                  "Reference.add_from_fasta: Unknown translation table for sequence '" ^ !name ^ "'"
                end in
          let seq = Buffer.contents seq in
          res := StrandedStringMap.add (Types.Forward !name) (seq, table) !res;
          res := StrandedStringMap.add (Types.Reverse !name) (Misc.rc seq, table) !res
        end in
      begin try
        while true do
          let line = input_line input in
          let line =
            let red_len = String.length line - 1 in
            if line.[red_len] = '\r' then
              String.sub line 0 red_len
            else
              line in
          if Str.string_match Tools.Split.fasta_name_re line 0 then begin
            process_seq ();
            name := String.sub line 1 (String.length line - 1);
            Buffer.clear seq
          end else
            Buffer.add_bytes seq (Misc.dnaize_bytes (Bytes.of_string line))
        done
      with End_of_file ->
        process_seq ();
        close_in input
      end;
      !res
    let find obj str_name =
      try
        StrandedStringMap.find str_name obj
      with Not_found ->
        let _, name = Types.split_of_stranded str_name in
        failwith ("Sequences.Reference.find: Unknown sequence '" ^ name ^ "'")
    let length obj str_name =
        let seq, _ = find obj str_name in
        String.length seq
    let get_sequence obj str_ivl =
      let { Types.low = { Types.name; position = lo }; length = len } = str_ivl in
      let seq, _ = find obj name in
      let hi = lo + len in
      if lo < 0 then
        failwith("Sequences.Reference.get_sequence: Low coordinate '" ^ string_of_int lo ^
                 "' is out of range");
      if hi > String.length seq then
        failwith("Sequences.Reference.get_sequence: High coordinate '" ^ string_of_int hi ^
                 "' is out of range");
      (*let lo = max 0 lo and hi = min hi (String.length seq) in
      let len = hi - lo in*)
      if len = 0 then
        ""
      else begin
        let res = Bytes.create len in
        Bytes.blit_string seq lo res 0 len;
        Bytes.to_string res
      end
    let get_table obj str_ivl =
      let { Types.low = { Types.name; _ }; _ } = str_ivl in
      let _, table = find obj name in
      table
    let get_sequence_and_table obj str_ivl =
      get_sequence obj str_ivl, get_table obj str_ivl
  end
