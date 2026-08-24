(*
    Tests_Sequences.ml -- (c) 2026 Paolo Ribeca, <paolo.ribeca@gmail.com>

    This file is part of BiOCamLib, the OCaml foundations upon which
    a number of the bioinformatics tools I developed are built.

    Tests_Sequences.ml exercises the Sequences module: the linting
    filters, the coordinate and strand vocabulary of [Types], the NCBI
    translation tables, and the reference store.  Between them these
    carry every coordinate and every base the annotation subsystem
    reads, so an error here surfaces as a wrong protein several layers
    up rather than as a failure on the spot.

    Inputs are built inline rather than read from test/ so that each
    check states the exact sequence it is about.

    This program was designed and developed by the author(s),
    with the assistance of the following AI tool(s):
      2026 Claude (Anthropic).
    The final logic and implementation were reviewed and verified in
    their entirety by the author(s).

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

open BiOCamLib
open Better

module L = Sequences.Lint
module T = Sequences.Types
module Tr = Sequences.Translation
module R = Sequences.Reference

(* Helpers. *)

let reference_of name seq =
  R.add_from_fasta_string ~linter:Fun.id R.empty (Printf.sprintf ">%s\n%s\n" name seq)

(* Linting.  The default filters normalise case and fold anything outside the
   alphabet, which is why a reference loaded without an explicit linter comes
   back N-masked -- the defect that made feature extraction hand back Ns. *)

let test_lint () =
  Testing.section "Sequence linting" (fun () ->
    Testing.check_string "the identity filter leaves a sequence alone"
      ~expected:"acgtRYn-." (L.none "acgtRYn-.");
    Testing.check_string "dnaize upper-cases ACGT and folds everything else to N"
      ~expected:"ACGTNNNN" (L.dnaize "acgtRY-.");
    Testing.check_string "dnaize keeps the case of ACGT when asked"
      ~expected:"acgtNN" (L.dnaize ~keep_lowercase:true "acgtRY");
    Testing.check_string "dnaize keeps gaps when asked"
      ~expected:"ACGT-N" (L.dnaize ~keep_dashes:true "acgt-R");
    (* Both flags are independent, and a gap is not a lowercase base. *)
    Testing.check_string "the two dnaize flags compose"
      ~expected:"acgt-N" (L.dnaize ~keep_lowercase:true ~keep_dashes:true "acgt-R");
    Testing.check_string "proteinize leaves the twenty standard residues alone"
      ~expected:"ACDEFGHIKLMNPQRSTVWY" (L.proteinize "ACDEFGHIKLMNPQRSTVWY");
    Testing.check_string "proteinize upper-cases"
      ~expected:"ACDEF" (L.proteinize "acdef");
    (* rc_bytes works in place; rc allocates.  The two must agree. *)
    Testing.check_string "rc_bytes reverse-complements in place"
      ~expected:"ACGT"
      (let b = Bytes.of_string "ACGT" in
       L.rc_bytes b;
       Bytes.to_string b);
    Testing.check_string "rc agrees with rc_bytes"
      ~expected:(L.rc "ACGTRYKMBVDH")
      (let b = Bytes.of_string "ACGTRYKMBVDH" in
       L.rc_bytes b;
       Bytes.to_string b);
    (* U is RNA but complements to A, as it would in DNA. *)
    Testing.check_string "uracil complements to adenine"
      ~expected:"A" (L.rc "U");
    Testing.check_string "the self-complementary codes are their own complement"
      ~expected:"NWS" (L.rc "SWN"))

(* Coordinates and strands.  Everything in memory is 0-based half-open and
   everything on the wire is 1-based inclusive; these two functions are the
   whole of the boundary. *)

let test_types () =
  Testing.section "Sequence coordinates and strands" (fun () ->
    Testing.check_int "a coordinate is read from 1-based into 0-based"
      ~expected:0 (T.coord_of_string "1");
    Testing.check_string "and written back as 1-based"
      ~expected:"1" (T.string_of_coord 0);
    Testing.check_string "the two are inverse"
      ~expected:"12345" (T.string_of_coord (T.coord_of_string "12345"));
    Testing.check_raises "a coordinate that is not a number is refused"
      (fun () -> ignore (T.coord_of_string "not-a-number"));
    Testing.check_string "the forward strand round-trips through its symbol"
      ~expected:"+" (T.string_of_strand (T.strand_of_string "+"));
    Testing.check_string "the reverse strand round-trips through its symbol"
      ~expected:"-" (T.string_of_strand (T.strand_of_string "-"));
    Testing.check_raises "an unrecognised strand symbol is refused"
      (fun () -> ignore (T.strand_of_string "?"));
    (* [split_of_stranded] and [stranded_of_split] take a value in and out of
       its direction without looking at what the value is. *)
    Testing.check_string "a stranded value survives being split and rejoined"
      ~expected:"chr1:-"
      (let s = T.Reverse "chr1" in
       let strand, name = T.split_of_stranded s in
       T.string_of_stranded_string (T.stranded_of_split strand name));
    Testing.check_string "a forward stranded name renders name-first"
      ~expected:"chr1:+" (T.string_of_stranded_string (T.Forward "chr1")))

(* Translation tables.  The alternative-start rewrite and the stop handling are
   what [Annotation.validate_translation] compares a GenBank /translation
   against, so their edges matter. *)

let test_translation () =
  Testing.section "Translation tables" (fun () ->
    let t1 = Tr.of_string "1" in
    Testing.check_string "a table round-trips through its NCBI number"
      ~expected:"11" (Tr.to_string (Tr.of_string "11"));
    Testing.check "every table describes itself"
      (fun () -> Tr.describe t1 <> "");
    Testing.check_raises "an unknown table number is refused"
      (fun () -> ignore (Tr.of_string "99"));
    Testing.check_string "translation stops at the first in-frame stop, which is not emitted"
      ~expected:"MK" (Tr.translate t1 "ATGAAATAAGGG");
    Testing.check_string "with the flag cleared every stop is emitted as a star"
      ~expected:"MK*G" (Tr.translate ~stop_on_first_stop:false t1 "ATGAAATAAGGG");
    Testing.check_string "an unknown codon translates to X"
      ~expected:"MXK" (Tr.translate ~stop_on_first_stop:false t1 "ATGNNNAAA");
    Testing.check_string "a trailing partial codon is dropped"
      ~expected:"MK" (Tr.translate ~stop_on_first_stop:false t1 "ATGAAAG");
    Testing.check_string "phase drops bases from the 5' end"
      ~expected:"MK" (Tr.translate ~phase:2 t1 "GGATGAAATAA");
    (* TTG is a start codon in table 1 but codes leucine everywhere else in the
       sequence, which is exactly the distinction the flag has to make. *)
    Testing.check_string "an alternative start codon is translated normally by default"
      ~expected:"LK" (Tr.translate t1 "TTGAAATAA");
    Testing.check_string "the alternative-start rewrite touches the first codon"
      ~expected:"MK"
      (Tr.translate ~replace_alternative_start_codons_with_methionine:true t1 "TTGAAATAA");
    Testing.check_string "and only the first"
      ~expected:"MLK"
      (Tr.translate ~replace_alternative_start_codons_with_methionine:true t1
         "TTGTTGAAATAA");
    (* Table 2 is the vertebrate mitochondrial code: ATA is methionine there and
       isoleucine under the standard code. *)
    let t2 = Tr.of_string "2" in
    Testing.check_string "the mitochondrial table reads ATA as methionine"
      ~expected:"M" (Tr.translate ~stop_on_first_stop:false t2 "ATA");
    Testing.check_string "the standard table reads the same codon as isoleucine"
      ~expected:"I" (Tr.translate ~stop_on_first_stop:false t1 "ATA");
    (* AGA is arginine under table 1 and a stop under table 2. *)
    Testing.check_string "AGA is arginine under the standard code"
      ~expected:"R" (Tr.translate ~stop_on_first_stop:false t1 "AGA");
    Testing.check_string "AGA is a stop under the mitochondrial code"
      ~expected:"*" (Tr.translate ~stop_on_first_stop:false t2 "AGA");
    Testing.check_int "stops finds the one in-frame stop"
      ~expected:1 (IntSet.cardinal (Tr.stops ~frames:[ 0 ] t1 "ATGAAATAA"));
    Testing.check_int "an out-of-frame stop is not in frame 0"
      ~expected:0 (IntSet.cardinal (Tr.stops ~frames:[ 0 ] t1 "GATGAAATAA"));
    Testing.check "the empty sequence translates to nothing"
      (fun () -> Tr.translate t1 "" = ""))

(* The reference store.  Both strands are held, so a minus-strand lookup is a
   plain fetch rather than a reverse-complement at the call site -- which is
   why the coordinates of a reverse interval are in the reverse frame. *)

let test_reference () =
  Testing.section "Reference store" (fun () ->
    let r = reference_of "chr1" "ATGCCCGGGTAA" in
    Testing.check_string "a sequence comes back off the forward strand"
      ~expected:"ATGCCCGGGTAA" (fst (R.find r (T.Forward "chr1")));
    Testing.check_string "and reverse-complemented off the reverse strand"
      ~expected:"TTACCCGGGCAT" (fst (R.find r (T.Reverse "chr1")));
    Testing.check_int "length is the same on either strand"
      ~expected:12 (R.length r (T.Forward "chr1"));
    Testing.check_int "length agrees on the reverse strand"
      ~expected:12 (R.length r (T.Reverse "chr1"));
    Testing.check_raises "an unknown sequence name is refused"
      (fun () -> ignore (R.find r (T.Forward "nope")));
    Testing.check_string "the default table is the standard code"
      ~expected:"1" (Tr.to_string (snd (R.find r (T.Forward "chr1"))));
    (* iter walks the forward strand only: both are stored, but they are one
       sequence and a caller writing the reference out wants it once. *)
    Testing.check_string "iter yields each sequence once, forward"
      ~expected:"chr1=ATGCCCGGGTAA"
      (let acc = ref [] in
       R.iter (fun ~name ~seq ~table:_ ~description:_ -> List.accum acc (name ^ "=" ^ seq)) r;
       List.rev !acc |> String.concat ",");
    (* A FASTA header is a name and then free text.  Reading the whole line as
       the name made an ordinary reference unusable: '>chr1 Homo sapiens
       chromosome 1' was stored under all of that, and an annotation speaking
       of 'chr1' could not find it. *)
    let described =
      R.add_from_fasta_string ~linter:Fun.id R.empty
        ">chr1 Homo sapiens chromosome 1\nACGT\n>plain\nTTTT\n" in
    Testing.check_string "a sequence is found by the first word of its header"
      ~expected:"ACGT" (fst (R.find described (T.Forward "chr1")));
    Testing.check_string "and the rest of the header is kept as its description"
      ~expected:"Homo sapiens chromosome 1"
      (R.description described (T.Forward "chr1"));
    Testing.check_string "a header with no description has none" ~expected:""
      (R.description described (T.Forward "plain"));
    Testing.check_raises "and the whole header is not a name"
      (fun () -> ignore (R.find described (T.Forward "chr1 Homo sapiens chromosome 1")));
    Testing.check_string "iter hands the description back with the sequence"
      ~expected:"chr1|Homo sapiens chromosome 1 plain|"
      (let acc = ref [] in
       R.iter (fun ~name ~seq:_ ~table:_ ~description ->
         List.accum acc (name ^ "|" ^ description)) described;
       List.rev !acc |> String.concat " ");
    Testing.check_int "two sequences can be loaded from one string"
      ~expected:2
      (let two =
         R.add_from_fasta_string ~linter:Fun.id R.empty ">a\nACGT\n>b\nTTTT\n" in
       let n = ref 0 in
       R.iter (fun ~name:_ ~seq:_ ~table:_ ~description:_ -> incr n) two;
       !n);
    (* A wrapped FASTA and a flat one are the same sequence. *)
    Testing.check_string "a wrapped record is joined on the way in"
      ~expected:"ACGTACGTAC"
      (fst (R.find (R.add_from_fasta_string ~linter:Fun.id R.empty ">w\nACGTA\nCGTAC\n")
              (T.Forward "w")));
    Testing.check_string "the linter is applied on the way in"
      ~expected:"ACGTNN"
      (fst (R.find (R.add_from_fasta_string ~linter:L.dnaize R.empty ">d\nacgtRY\n")
              (T.Forward "d")));
    (* get_sequence takes a stranded interval, 0-based and half-open. *)
    Testing.check_string "a forward interval is a plain slice"
      ~expected:"ATG"
      (R.get_sequence r (T.make_stranded_interval (T.Forward "chr1") 0 3));
    Testing.check_string "an interior forward interval"
      ~expected:"CCC"
      (R.get_sequence r (T.make_stranded_interval (T.Forward "chr1") 3 3));
    Testing.check_string "a reverse interval is read in the reverse frame"
      ~expected:"TTA"
      (R.get_sequence r (T.make_stranded_interval (T.Reverse "chr1") 0 3));
    Testing.check_int "a zero-length interval yields the empty string"
      ~expected:0
      (String.length
         (R.get_sequence r (T.make_stranded_interval (T.Forward "chr1") 4 0))))

(* Junction files, as the GEM pipeline writes them.  Two shapes are accepted --
   one naming the sequence once, one naming it at both ends -- and the second is
   legal only when the two names and the two strands agree, a junction between
   two different sequences being something this format cannot mean.  The parser
   takes a path rather than a string, so the fixtures go through a file. *)

let test_junctions () =
  Testing.section "Junctions" (fun () ->
    let parse ?default_coverage text =
      let path = Filename.temp_file "BiOCamLib_Tests_" ".junctions" in
      let oc = open_out path in
      output_string oc text;
      close_out oc;
      Fun.protect ~finally:(fun () -> Sys.remove path) (fun () ->
        let acc = ref [] in
        Sequences.Junctions.parse ?default_coverage
          (fun n s lo hi cov ->
            List.accum acc
              (Printf.sprintf "%d %s %d %d %g" n (T.string_of_stranded_string s) lo hi cov))
          path;
        List.rev !acc |> String.concat " | ") in
    Testing.check_string "the short form names the sequence once"
      ~expected:"1 chr1:+ 100 200 0" (parse "chr1\t+\t100\t200\n");
    Testing.check_string "and takes a coverage as a fifth field"
      ~expected:"1 chr1:+ 100 200 7.5" (parse "chr1\t+\t100\t200\t7.5\n");
    Testing.check_string "an absent coverage is the caller's default"
      ~expected:"1 chr1:+ 100 200 2.5"
      (parse ~default_coverage:2.5 "chr1\t+\t100\t200\n");
    Testing.check_string "the long form names it at both ends"
      ~expected:"1 chr1:- 100 200 0" (parse "chr1\t-\t100\tchr1\t-\t200\n");
    Testing.check_string "and takes a coverage as a seventh field"
      ~expected:"1 chr1:- 100 200 3" (parse "chr1\t-\t100\tchr1\t-\t200\t3\n");
    Testing.check_string "lines are numbered as they are read"
      ~expected:"1 chr1:+ 1 2 0 | 2 chr2:+ 3 4 0"
      (parse "chr1\t+\t1\t2\nchr2\t+\t3\t4\n");
    (* What it refuses, and where it says the trouble is: a junction file runs
       to millions of lines, so the number in the message is the whole of the
       diagnosis. *)
    Testing.check_raises ~re:"Invalid number of fields"
      "a line of the wrong width is refused"
      (fun () -> ignore (parse "chr1\t+\t100\n"));
    Testing.check_raises ~re:"On line 2" "and the message says which line"
      (fun () -> ignore (parse "chr1\t+\t1\t2\nchr1\t+\t3\n"));
    Testing.check_raises ~re:"Incorrect syntax"
      "a junction between two sequences is refused"
      (fun () -> ignore (parse "chr1\t+\t100\tchr2\t+\t200\n"));
    Testing.check_raises ~re:"Incorrect syntax" "as is one between two strands"
      (fun () -> ignore (parse "chr1\t+\t100\tchr1\t-\t200\n"));
    Testing.check_raises ~re:"Negative" "as is a negative coordinate"
      (fun () -> ignore (parse "chr1\t+\t-1\t200\n"));
    Testing.check_raises ~re:"Negative" "and a negative coverage"
      (fun () -> ignore (parse "chr1\t+\t1\t2\t-3\n"));
    Testing.check_raises ~re:"Input file not found" "a missing file is refused as such"
      (fun () ->
        Sequences.Junctions.parse (fun _ _ _ _ _ -> ())
          "/nonexistent/BiOCamLib_Tests_missing.junctions"))


let run () =
  test_lint ();
  test_types ();
  test_translation ();
  test_reference ();
  test_junctions ()
