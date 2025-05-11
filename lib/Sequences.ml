(*
    Sequences.ml -- (c) 2018-2023 Paolo Ribeca, <paolo.ribeca@gmail.com>

    This file is part of BiOCamLib, the OCaml foundations upon which
    a number of the bioinformatics tools I developed are built.

    Sequences.ml implements tools to reverse, complement, or translate
    sequences. Support for intervals over a sequence (as in exons or
    transcripts) and a set of reference sequences is also provided.

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

module Lint:
  sig
    val none: 'a -> 'a
    (* The _bytes functions modify content in place *)
    val dnaize_bytes: ?keep_lowercase:bool -> ?keep_dashes:bool -> bytes -> unit
    val rc_bytes: bytes -> unit
    val proteinize_bytes: ?keep_lowercase:bool -> ?keep_dashes:bool -> bytes -> unit
    (* *)
    val dnaize: ?keep_lowercase:bool -> ?keep_dashes:bool -> string -> string
    val rc: string -> string
    val proteinize: ?keep_lowercase:bool -> ?keep_dashes:bool -> string -> string
  end
= struct
    let none w = w
    let dnaize_bytes ?(keep_lowercase = false) ?(keep_dashes = false) b =
      let dnaize =
        if keep_lowercase then
          function
          | 'A' -> 'A' | 'a' -> 'a'
          | 'C' -> 'C' | 'c' -> 'c'
          | 'G' -> 'G' | 'g' -> 'g'
          | 'T' -> 'T' | 't' -> 't'
          | '-' when keep_dashes -> '-'
          | _ -> 'N'
        else
          function
          | 'A' | 'a' -> 'A'
          | 'C' | 'c' -> 'C'
          | 'G' | 'g' -> 'G'
          | 'T' | 't' -> 'T'
          | '-' when keep_dashes -> '-'
          | _ -> 'N' in
      let open Bytes in
      for i = 0 to length b - 1 do
        b.@(i) <- dnaize b.@(i)
      done
    let dnaize ?(keep_lowercase = false) ?(keep_dashes = false) s =
      (* This function allocates memory *)
      let b = Bytes.of_string s in
      dnaize_bytes ~keep_lowercase ~keep_dashes b;
      Bytes.unsafe_to_string b
    let rc_bytes b =
      let open Bytes in
      rev b;
      iteri
        (fun i c ->
          b.@(i) <- begin
            match c with
            | 'A' -> 'T' | 'a' -> 't'
            | 'C' -> 'G' | 'c' -> 'g'
            | 'G' -> 'C' | 'g' -> 'c'
            | 'T' -> 'A' | 't' -> 'a'
            | c -> c
          end)
        b
    let rc s =
      (* This function allocates memory *)
      let b = Bytes.of_string s in
      rc_bytes b;
      Bytes.unsafe_to_string b
    let proteinize_bytes ?(keep_lowercase = false) ?(keep_dashes = false) b =
      let proteinize =
        if keep_lowercase then
          function
          | 'A' -> 'A' | 'a' -> 'a'
          | 'C' -> 'C' | 'c' -> 'c'
          | 'D' -> 'D' | 'd' -> 'd'
          | 'E' -> 'E' | 'e' -> 'e'
          | 'F' -> 'F' | 'f' -> 'f'
          | 'G' -> 'G' | 'g' -> 'g'
          | 'H' -> 'H' | 'h' -> 'h'
          | 'I' -> 'I' | 'i' -> 'i'
          | 'K' -> 'K' | 'k' -> 'k'
          | 'L' -> 'L' | 'l' -> 'l'
          | 'M' -> 'M' | 'm' -> 'm'
          | 'N' -> 'N' | 'n' -> 'n'
          | 'O' -> 'O' | 'o' -> 'o'
          | 'P' -> 'P' | 'p' -> 'p'
          | 'Q' -> 'Q' | 'q' -> 'q'
          | 'R' -> 'R' | 'r' -> 'r'
          | 'S' -> 'S' | 's' -> 's'
          | 'T' -> 'T' | 't' -> 't'
          | 'U' -> 'U' | 'u' -> 'u'
          | 'V' -> 'V' | 'v' -> 'v'
          | 'W' -> 'W' | 'w' -> 'w'
          | 'Y' -> 'Y' | 'y' -> 'y'
          | '*' -> '*'
          | '-' when keep_dashes -> '-'
          | _ -> 'X'
        else
          function
          | 'A' | 'a' -> 'A'
          | 'C' | 'c' -> 'C'
          | 'D' | 'd' -> 'D'
          | 'E' | 'e' -> 'E'
          | 'F' | 'f' -> 'F'
          | 'G' | 'g' -> 'G'
          | 'H' | 'h' -> 'H'
          | 'I' | 'i' -> 'I'
          | 'K' | 'k' -> 'K'
          | 'L' | 'l' -> 'L'
          | 'M' | 'm' -> 'M'
          | 'N' | 'n' -> 'N'
          | 'O' | 'o' -> 'O'
          | 'P' | 'p' -> 'P'
          | 'Q' | 'q' -> 'Q'
          | 'R' | 'r' -> 'R'
          | 'S' | 's' -> 'S'
          | 'T' | 't' -> 'T'
          | 'U' | 'u' -> 'U'
          | 'V' | 'v' -> 'V'
          | 'W' | 'w' -> 'W'
          | 'Y' | 'y' -> 'Y'
          | '*' -> '*'
          | '-' when keep_dashes -> '-'
          | _ -> 'X' in
      let open Bytes in
      for i = 0 to length b - 1 do
        b.@(i) <- proteinize b.@(i)
      done
    let proteinize ?(keep_lowercase = false) ?(keep_dashes = false) s =
      (* This function allocates memory *)
      let b = Bytes.of_string s in
      proteinize_bytes ~keep_lowercase ~keep_dashes b;
      Bytes.unsafe_to_string b
  end

module Coordinate:
  sig
    type t = private int
    type based_t =
      | Zero
      | One
    val of_int: ?based:based_t -> int -> t
    val to_int: ?based:based_t -> t -> int
    val of_string: ?based:based_t -> string -> t
    val to_string: ?based:based_t -> t -> string
    module Map: module type of Map.Make (MakeComparable (struct type tt = t type t = tt end))
  end
= struct
    (* Internally, we represent coordinates as zero-based *)
    type t = int
    type based_t =
      | Zero
      | One
    let of_int ?(based = One) n =
      let res =
        match based with
        | Zero -> n
        | One -> n - 1 in
      if res < 0 then
        Printf.sprintf "(%s): Invalid argument '%d'" __FUNCTION__ n |> failwith;
      res
    let of_string ?(based = One) s =
      try
        int_of_string s |> of_int ~based
      with _ ->
        Printf.sprintf "(%s): Invalid argument '%s'" __FUNCTION__ s |> failwith
    let to_int ?(based = One) n =
      match based with
      | Zero -> n
      | One -> n + 1
    let to_string ?(based = One) n = to_int ~based n |> string_of_int
    module Map = Map.Make (MakeComparable (struct type tt = t type t = tt end))
  end
(* A coordinate anchored to a sequence *)
module Pointer =
  struct
    type t = {
      sequence_name: string;
      coordinate: Coordinate.t
    }
    let to_string ?(based = Coordinate.One) p =
      Printf.sprintf "%s:%d" p.sequence_name (Coordinate.to_int ~based p.coordinate)    
    module Map = Map.Make (MakeComparable (struct type tt = t type t = tt end))
  end
module Interval =
  struct
    type t = {
      coordinate_lo: Coordinate.t;
      length: int
    }
    let to_string ?(based = Coordinate.One) a =
      Printf.sprintf "%d:%d" (Coordinate.to_int ~based a.coordinate_lo) a.length
    module Map = Map.Make (MakeComparable (struct type tt = t type t = tt end))
  end
(* An interval anchored to a sequence *)
module Annotation =
  struct
    type t = {
      sequence_name: string;
      coordinate_lo: Coordinate.t;
      length: int
    }
    let to_string ?(based = Coordinate.One) a =
      Printf.sprintf "%s:%d:%d" a.sequence_name (Coordinate.to_int ~based a.coordinate_lo) a.length
    module Map = Map.Make (MakeComparable (struct type tt = t type t = tt end))
  end

module BaseStranded =
  struct
    (* We define a generic type to give things a direction *)
    type 'a t =
      | Forward of 'a
      | Reverse of 'a
      (*| None of 'a*)
  end
module Stranded =
  struct
    include BaseStranded
    module Strand =
      struct
        type t = unit BaseStranded.t
        let forward = Forward ()
        let reverse = Reverse ()
        let of_string = function
          | "F" | "f" | "+" | "forward" | "Forward" -> forward
          | "R" | "r" | "-" | "reverse" | "Reverse" -> reverse
          | s ->
            Printf.sprintf "(%s): Unrecognized strand '%s'" __FUNCTION__ s |> failwith
        let to_string = function
          | Forward _ -> "+"
          | Reverse _ -> "-"
      end
    (* General functions *)
    let split = function
      | Forward unstranded -> Strand.forward, unstranded
      | Reverse unstranded -> Strand.reverse, unstranded
    let join s payload =
      match s with
      | Forward () -> Forward payload
      | Reverse () -> Reverse payload
    let strand_to_string = function
      | Forward _ -> "+"
      | Reverse _ -> "-"
    (* A coordinate anchored to a stranded sequence *)
    module Pointer =
      struct
        type t = Pointer.t BaseStranded.t
        let to_string ?(based = Coordinate.One) sp =
          let s, p = split sp in
          Printf.sprintf "%s:%s:%d"
            p.Pointer.sequence_name (Strand.to_string s) (Coordinate.to_int ~based p.coordinate)
        let of_string ?(based = Coordinate.One) s =
          try
            let fields = String.Split.on_char_as_array ':' s in
            if Array.length fields <> 3 then
              Printf.sprintf "(%s): Invalid number of fields in argument '%s'" __FUNCTION__ s |> failwith;
            join (Strand.of_string fields.(1)) {
              Pointer.sequence_name = fields.(0);
              coordinate = Coordinate.of_string ~based fields.(2)
            }
          with _ ->
            Printf.sprintf "(%s): Syntax error in argument '%s'" __FUNCTION__ s |> failwith      
        module Map = Map.Make (MakeComparable (struct type tt = t type t = tt end))
      end
    (* A stranded interval not associated with a specific sequence *)
    module Interval =
      struct
        type t = Interval.t BaseStranded.t
        module Map = Map.Make (MakeComparable (struct type tt = t type t = tt end))
      end
    (* An interval anchored to a stranded sequence *)
    module Annotation =
      struct
        type t = Annotation.t BaseStranded.t
        let to_string ?(based = Coordinate.One) sa =
          let s, a = split sa in
          Printf.sprintf "%s:%s:%d:%d"
            a.Annotation.sequence_name (Strand.to_string s) (Coordinate.to_int ~based a.coordinate_lo) a.length

        module Map = Map.Make (MakeComparable (struct type tt = t type t = tt end))
      end
    (* We redefine these ones last so as not to shadow the ones in the parent scope *)
    module Coordinate =
      struct
        type t = Coordinate.t BaseStranded.t

        module Map = Map.Make (MakeComparable (struct type tt = t type t = tt end))
      end
    module String =
      struct
        type t = string BaseStranded.t
        let to_string ss =
          let strand, name = split ss in
          Printf.sprintf "%s:%s" name (Strand.to_string strand)

        (* Association tables with stranded sequence names *)
        module Map = Map.Make (MakeComparable (struct type tt = t type t = tt end))
      end
  end

module Junctions:
  sig
    val parse: ?default_coverage:float -> (int -> Stranded.String.t -> int -> int -> float -> unit) -> string -> unit
  end
= struct
    (* Helper function to parse what is produced by the GEM pipeline *)
    let parse ?(default_coverage = 0.) f introns =
      let introns = open_in introns and cntr = ref 0 in
      try
        while true do
          let line = String.Split.on_char_as_array '\t' (input_line introns) in
          incr cntr;
          let error what =
            Printf.sprintf "(%s): On line %d: %s\n%!" __FUNCTION__ !cntr what |> failwith in
          (* Format is: <name_1> <str_1> <pos_1> <name_2> <str_2> <pos_2> [<cov>],
              or:       <name> <str> <pos_1> <pos_2> [<cov>] *)
          let stranded_name, pos_don, pos_acc, cov =
            let len = Array.length line in
            match len with
            | 4 | 5 ->
              begin try
                let dir = Stranded.Strand.of_string line.(1)
                and pos_don = int_of_string line.(2)
                and pos_acc = int_of_string line.(3)
                and cov =
                  if len = 5 then
                    float_of_string line.(4)
                  else
                    default_coverage in
                (Stranded.join dir line.(0)), pos_don, pos_acc, cov
              with _ ->
                error "Incorrect syntax"
              end
            | 6 | 7 ->
              begin try
                let dir_don = Stranded.Strand.of_string line.(1)
                and pos_don = int_of_string line.(2)
                and dir_acc = Stranded.Strand.of_string line.(4)
                and pos_acc = int_of_string line.(5)
                and cov =
                  if len = 7 then
                    float_of_string line.(6)
                  else
                    default_coverage in
                if line.(0) <> line.(3) || dir_don <> dir_acc then
                  raise Exit;
                (Stranded.join dir_don line.(0)), pos_don, pos_acc, cov
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
      | Table_15 | Table_16 | Table_21 | Table_22 | Table_23 | Table_24
      | Table_25 | Table_26 | Table_27 | Table_28 | Table_29 | Table_30
      | Table_31 | Table_33
    val of_string: string -> t
    val get_stops: ?frames:int list -> t -> string -> IntSet.t
    val get_starts:
          ?frames:int list -> ?get_alternative_start_codons:bool -> t -> string -> IntSet.t
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
    | Table_15 | Table_16 | Table_21 | Table_22 | Table_23 | Table_24
    | Table_25 | Table_26 | Table_27 | Table_28 | Table_29 | Table_30
    | Table_31 | Table_33
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
      | "15" | "Table15" | "Table_15" -> Table_15
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
      | "33" | "Table33" | "Table_33" -> Table_33
      | w ->
        Printf.sprintf "(%s): Invalid translation table '%s'" __FUNCTION__ w |> failwith
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
      | Table_15 -> "BlepharismaNuclear"
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
      | Table_33 -> "CephalodiscidaeMitochondrial"
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
            Printf.sprintf "(%s): Translation table not yet implemented, sorry" __FUNCTION__ |> failwith)
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
          | Table_14 | Table_33 ->
            begin function
            | "TAG" | "NNN" -> add pos
            | _ -> ()
            end
          | Table_15 | Table_16 ->
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
                    | "GCA" | "GCC" | "GCG" | "GCT" | "GCN" -> 'A'
                    | "TGC" | "TGT" -> 'C'
                    | "GAC" | "GAT" -> 'D'
                    | "GAA" | "GAG" -> 'E'
                    | "TTC" | "TTT" -> 'F'
                    | "GGA" | "GGC" | "GGG" | "GGT" | "GGN" -> 'G'
                    | "CAC" | "CAT" -> 'H'
                    | "ATA" | "ATC" | "ATT" -> 'I'
                    | "AAA" | "AAG" -> 'K'
                    | "CTA" | "CTC" | "CTG" | "CTT" | "CTN" | "TTA" | "TTG" -> 'L'
                    | "ATG" -> 'M'
                    | "AAC" | "AAT" -> 'N'
                    | "CCA" | "CCC" | "CCG" | "CCT" | "CCN" -> 'P'
                    | "CAA" | "CAG" -> 'Q'
                    | "AGA" | "AGG" | "CGA" | "CGC" | "CGG" | "CGT" | "CGN" -> 'R'
                    | "AGC" | "AGT" | "TCA" | "TCC" | "TCG" | "TCT" | "TCN" -> 'S'
                    | "ACA" | "ACC" | "ACG" | "ACT" | "ACN" -> 'T'
                    | "GTA" | "GTC" | "GTG" | "GTT" | "GTN" -> 'V'
                    | "TGG" -> 'W'
                    | "TAC" | "TAT" -> 'Y'
                    (* An assertion will not work here, because of course
                        there can be some junk in the sequence *)
                    | _ -> 'X' (*assert false*)
                    end
                  | _ ->
                    Printf.sprintf "(%s): Translation table not yet implemented, sorry" __FUNCTION__ |> failwith
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
              List.accum results (start, contents))
        starts;
      Array.of_list (List.rev !results)
  end

(* A map (stranded name->string, translation table) from one or more multi-FASTA file,
    and a few additional things *)
module Reference:
  sig
    type t
    val empty: t
    (* The last optional argument is a file containing translation tables.
        If they are absent, Table_1 (Standard) is assumed *)
    val add_from_fasta: ?linter:(string -> string) -> ?tables:string -> t -> string -> t
    val find: t -> Stranded.String.t -> string * Translation.t
    val length: t -> Stranded.String.t -> int
    val get_sequence: t -> Stranded.Annotation.t -> string
    val get_table: t -> Stranded.Annotation.t -> Translation.t
    val get_sequence_and_table: t -> Stranded.Annotation.t -> string * Translation.t
  end
= struct
    (* We explicitly separate forward and reverse sequences *)
    module StrandedStringMap = Stranded.String.Map
    (* To each sequence name we associate a sequence and a translation table *)
    type t = (string * Translation.t) StrandedStringMap.t
    let empty = StrandedStringMap.empty
    let fasta_name_re = Str.regexp "^>"
    let add_from_fasta ?(linter = Lint.dnaize ~keep_lowercase:false ~keep_dashes:false) ?(tables = "")
                       obj input =
      let tables =
        let res = ref StringMap.empty in
        if tables <> "" then begin
          let tables = open_in tables and cntr = ref 0 in
          try
            while true do
              let line = String.Split.on_char_as_array '\t' (input_line tables) in
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
                  Printf.sprintf "(%s): On line %d: Incorrect translation table file syntax" __FUNCTION__ !cntr
                    |> failwith in
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
                Printf.sprintf "(%s): Unknown translation table for sequence '%s'" __FUNCTION__ !name |> failwith in
          let seq = Buffer.contents seq in
          res := StrandedStringMap.add (Stranded.Forward !name) (seq, table) !res;
          res := StrandedStringMap.add (Stranded.Reverse !name) (Lint.rc seq, table) !res
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
          if Str.string_match fasta_name_re line 0 then begin
            process_seq ();
            name := String.sub line 1 (String.length line - 1);
            Buffer.clear seq
          end else
            Buffer.add_string seq (linter line)
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
        let _, name = Stranded.split str_name in
        Printf.sprintf "(%s): Unknown sequence '%s'" __FUNCTION__ name |> failwith
    let length obj str_name =
        let seq, _ = find obj str_name in
        String.length seq
    let get_sequence obj sa =
      let str, { Annotation.sequence_name; coordinate_lo; length } = Stranded.split sa in
      (* In this case we are accessing a string so we keep the coordinate zero-based *)
      let str_name = Stranded.join str sequence_name and lo = Coordinate.to_int ~based:Zero coordinate_lo in
      let seq, _ = find obj str_name in
      let hi = lo + length in
      if lo < 0 then
        Printf.sprintf "(%s): Low coordinate '%d' is out of range" __FUNCTION__ lo |> failwith;
      if hi > String.length seq then
        Printf.sprintf "(%s): High coordinate '%d' is out of range" __FUNCTION__ hi |> failwith;
      (*let lo = max 0 lo and hi = min hi (String.length seq) in
      let length = hi - lo in*)
      if length = 0 then
        ""
      else begin
        let res = Bytes.create length in
        Bytes.blit_string seq lo res 0 length;
        Bytes.to_string res
      end
    let get_table obj sa =
      let str, ann = Stranded.split sa in
      let _, table = Stranded.join str ann.Annotation.sequence_name |> find obj in
      table
    let get_sequence_and_table obj str_ivl =
      get_sequence obj str_ivl, get_table obj str_ivl
  end

