(*
    Tests_Annotations.ml -- (c) 2026 Paolo Ribeca, <paolo.ribeca@gmail.com>

    This file is part of BiOCamLib, the OCaml foundations upon which
    a number of the bioinformatics tools I developed are built.

    Tests_Annotations.ml exercises the Annotations subsystem and the
    parts of Sequences that AnnoTools leans on.  Checks carrying a
    [~known_bug] marker pin a defect diagnosed in the tabular-mode
    design note (DocsYard/BiOCamLib/docs/BiOCamLib-annotations-tabular.tex);
    they are expected to fail today, and the harness turns one of them
    passing into an error so that a fix cannot land unnoticed.

    Inputs are built inline rather than read from test/ so that each
    check states the exact record it is about.

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

module A = Annotations
module T = Sequences.Types

(* Helpers. *)

let lines = String.concat "\n"

(* Render a parsed LOCATION as "lo..hi,lo..hi" in 1-based inclusive form, with a
   zero-length interval shown as "lo^" so the degenerate case stays visible
   instead of collapsing into a reversed range. *)
let intervals_to_string ivs =
  List.map (fun ((_ : string option), (i : T.simple_interval_t)) ->
    if i.length = 0 then Printf.sprintf "%d^" i.low
    else Printf.sprintf "%d..%d" (i.low + 1) (i.low + i.length)) ivs
  |> String.concat ","

let strand_to_string = function
  | Some (T.Forward _) -> "+"
  | Some (T.Reverse _) -> "-"
  | None -> "."

let location s = A.GenBankLocation.of_string s |> A.GenBankLocation.intervals

(* Wrap a FEATURES block into a minimal but complete GenBank record. *)
let genbank ?(seq = "atgcccgggtaagcgactagcgcatcgtca") feature_lines =
  lines ([
    "LOCUS       demo01     30 bp    DNA              circular UNK";
    "DEFINITION  Synthetic test entry.";
    "ACCESSION   demo01";
    "FEATURES             Location/Qualifiers";
    "     source          1..30";
    "                     /organism=\"synthetic\"" ]
    @ feature_lines
    @ [ "ORIGIN      ";
        Printf.sprintf "        1 %s" seq;
        "//";
        "" ])

let gff3 rows = lines ("##gff-version 3" :: rows @ [ "" ])

(* Every feature of the annotation as (path, feature) pairs, in iteration
   order. *)
let features ann =
  let acc = ref [] in
  A.Annotation.iter_paths (fun ~path feature -> List.accum acc (path, feature)) ann;
  List.rev !acc

let feature_at ann category =
  List.find_opt (fun (path, _) -> List.mem category path) (features ann)

let count_substring needle haystack =
  let n = String.length needle and l = String.length haystack in
  let rec walk i acc =
    if i + n > l then acc
    else if String.sub haystack i n = needle then walk (i + n) (acc + 1)
    else walk (i + 1) acc in
  if n = 0 then 0 else walk 0 0

(* GenBank LOCATION parsing. *)

let test_locations () =
  Testing.section "GenBank LOCATION parsing" (fun () ->
    Testing.check_string "a plain range is 0-based half-open internally"
      ~expected:"1..15" (location "1..15" |> fst |> intervals_to_string);
    Testing.check_string "complement() sets the feature strand"
      ~expected:"-" (location "complement(16..30)" |> snd |> strand_to_string);
    Testing.check_string "join() yields one interval per part"
      ~expected:"1..9,20..28" (location "join(1..9,20..28)" |> fst |> intervals_to_string);
    Testing.check_string "complement(join(...)) keeps the parts in genomic order"
      ~expected:"1..9,20..28"
      (location "complement(join(1..9,20..28))" |> fst |> intervals_to_string);
    (* The reader is right here: a site between 1-based bases 100 and 101 is the
       0-based half-open interval [100, 100).  The defect is in the writers,
       which have no zero-length case and emit the reversed range 101..100. *)
    Testing.check_string "a between-bases site is stored as a zero-length interval"
      ~expected:"100^" (location "100^101" |> fst |> intervals_to_string);
    (* An origin-spanning join on a circular chromosome is stored in feature
       order, NOT sorted.  This check exists to stop anyone "fixing" the
       per-part strand bug below by normalising intervals to ascending order,
       which would silently produce the wrong protein here. *)
    Testing.check_string "an origin-spanning join keeps its parts in feature order"
      ~expected:"30..35,1..5"
      (location "complement(join(30..35,1..5))" |> fst |> intervals_to_string);
    (* [Remote] is [accession * version option * t], so the version is parsed
       off into its own slot; [intervals] projects only the accession. *)
    Testing.check_string "a remote reference records the foreign accession"
      ~expected:"J00194"
      (match location "J00194.1:100..202" |> fst with
       | (Some acc, _) :: _ -> acc
       | _ -> "(none)");
    (* Trans-splicing: one part forward, one part reverse.  Today the outer
       strand is passed down to every part and only the LAST part's strand is
       kept, so this whole feature comes back as Reverse and the forward part is
       silently reverse-complemented.  Representing it properly needs a strand
       per interval rather than one per feature. *)
    Testing.check
      ~known_bug:"Annotations.ml:368 keeps only the last part's strand"
      "a mixed-strand join does not report the whole feature as reverse"
      (fun () -> location "join(1..9,complement(20..28))" |> snd <> Some T.reverse))

(* GenBank record to AST. *)

let test_genbank_records () =
  Testing.section "GenBank record -> AST" (fun () ->
    let ann =
      genbank [
        "     gene            1..15";
        "                     /gene=\"abcA\"";
        "                     /locus_tag=\"DEMO_0001\"";
        "     CDS             1..15";
        "                     /gene=\"abcA\"";
        "                     /codon_start=\"2\"";
        "                     /product=\"hypothetical\"" ]
      |> A.GenBank.of_string in
    Testing.check_int "every feature of the record is loaded"
      ~expected:3 (List.length (features ann));
    Testing.check_string "the feature id is derived from /locus_tag"
      ~expected:"DEMO_0001"
      (match feature_at ann "gene" with
       | Some (_, f) -> Option.value ~default:"(none)" f.A.Annotation.id
       | None -> "(no gene)");
    Testing.check_string "every GenBank feature is pinned under source"
      ~expected:"annotation->source->CDS"
      (match feature_at ann "CDS" with
       | Some (path, _) -> A.Annotation.path_to_string path
       | None -> "(no CDS)");
    (* /codon_start is the GenBank spelling of GFF3's phase, 1-based against the
       0-based phase.  It is currently never read, so a CDS that does not start
       in frame 0 fails validate_translation for the wrong reason. *)
    Testing.check_equal
      ~known_bug:"Annotations.ml:1154 hardcodes phase = None"
      "/codon_start=2 becomes phase 1"
      ~to_string:(function Some n -> string_of_int n | None -> "none")
      ~expected:(Some 1)
      (match feature_at ann "CDS" with
       | Some (_, f) -> f.A.Annotation.phase
       | None -> None))

(* Feature sequence extraction.  The demo ORIGIN is
   atgcccgggtaagcgactagcgcatcgtca, 30 bases, and the reference the GenBank
   reader builds from it is upper-cased by the default linter. *)

let test_feature_sequence () =
  Testing.section "Feature sequence extraction" (fun () ->
    let cds location extra =
      genbank ([ Printf.sprintf "     CDS             %s" location ] @ extra)
      |> A.GenBank.of_string in
    let dna ann =
      match feature_at ann "CDS" with
      | Some (_, f) -> A.Annotation.feature_dna ann f
      | None -> "(no CDS)"
    and protein ann =
      match feature_at ann "CDS" with
      | Some (_, f) -> A.Annotation.feature_protein ann f
      | None -> "(no CDS)" in
    Testing.check_string "a forward feature yields its own bases"
      ~expected:"ATGCCCGGGTAAGCG" (dna (cds "1..15" []));
    (* rc is checked independently below, so using it here keeps this check
       about the strand handling rather than about the complement table. *)
    Testing.check_string "a reverse feature is reverse-complemented"
      ~expected:(Sequences.Lint.rc "ACTAGCGCATCGTCA") (dna (cds "complement(16..30)" []));
    Testing.check_string "a joined feature is stitched in interval order"
      ~expected:"ATGCCCGGGGCGCATCGT" (dna (cds "join(1..9,20..28)" []));
    Testing.check_string "translation stops at the first in-frame stop"
      ~expected:"MPG" (protein (cds "1..15" []));
    Testing.check_string "a joined CDS translates across the junction"
      ~expected:"MPGAHR" (protein (cds "join(1..9,20..28)" []));
    (* The table is the feature's /transl_table when it carries one.  Table 2
       reads AGA/AGG as stops, which Table 1 does not, so the two disagree on a
       sequence containing one. *)
    Testing.check_string "the translation table defaults to the standard code"
      ~expected:"1"
      (Sequences.Translation.to_string
         (match feature_at (cds "1..15" []) "CDS" with
          | Some (_, f) -> A.Annotation.feature_table (cds "1..15" []) f
          | None -> Sequences.Translation.Table_1));
    Testing.check_string "/transl_table overrides the default"
      ~expected:"11"
      (let ann = cds "1..15" [ "                     /transl_table=\"11\"" ] in
       Sequences.Translation.to_string
         (match feature_at ann "CDS" with
          | Some (_, f) -> A.Annotation.feature_table ann f
          | None -> Sequences.Translation.Table_1));
    (* Extraction needs a reference; asking without one is a programmer error,
       not a data error, so it raises rather than returning "". *)
    Testing.check_raises ~re:".*no reference set.*"
      "extracting without a reference raises"
      (fun () ->
        let ann = A.GFF3.of_string (gff3 [ "chr1\tdemo\tgene\t1\t9\t.\t+\t.\tID=g1" ]) in
        match feature_at ann "gene" with
        | Some (_, f) -> A.Annotation.feature_dna ann f
        | None -> ""))

(* Attribute handling. *)

let test_attributes () =
  Testing.section "Attribute handling" (fun () ->
    let ann =
      gff3 [ "chr1\tdemo\tgene\t100\t500\t.\t+\t.\tID=g1;Note=alpha%2Cbeta;Desc=x%20y" ]
      |> A.GFF3.of_string in
    let attr key =
      match feature_at ann "gene" with
      | Some (_, f) ->
        (match A.Annotation.attr_get ann f key with
         | Some (v :: _) -> v
         | _ -> "(absent)")
      | None -> "(no gene)" in
    Testing.check_string "percent-encoded values are decoded on read"
      ~expected:"alpha,beta" (attr "Note");
    Testing.check_string "a percent-encoded space is decoded on read"
      ~expected:"x y" (attr "Desc");
    (* Decoding without re-encoding is lossy: the comma re-emerges as a value
       separator on the next read, so the value silently splits in two. *)
    Testing.check
      ~known_bug:"Annotations.ml:669 never re-encodes what url_decode decoded"
      "a comma inside a value is re-encoded on write"
      (fun () -> count_substring "%2C" (A.GFF3.to_string ann) > 0);
    (* GFF3 column 9 has no way to spell a valueless qualifier: the grammar wants
       at least one value after '='.  A GenBank /pseudo therefore cannot survive
       a round trip through GFF3. *)
    Testing.check_does_not_raise
      ~known_bug:"Annotations_Parse.mly:171 requires at least one Attr_VALUE"
      "an empty attribute value can be read back"
      (fun () ->
        gff3 [ "chr1\tdemo\tgene\t100\t500\t.\t+\t.\tID=g1;pseudo=" ] |> A.GFF3.of_string))

(* GFF3 fidelity. *)

let test_gff3_fidelity () =
  Testing.section "GFF3 fidelity" (fun () ->
    let scored =
      gff3 [ "chr1\tdemo\tgene\t100\t500\t42.5\t+\t.\tID=g1" ] |> A.GFF3.of_string in
    Testing.check
      ~known_bug:"parse_row never reads fields.(5) and row_of_feature hardcodes \".\""
      "the score column survives a round trip"
      (fun () -> count_substring "42.5" (A.GFF3.to_string scored) > 0);
    (* A joined CDS is ONE feature.  The GFF3 writer emits one row per interval
       and the reader never re-merges rows sharing an ID, so it comes back as
       N separate features. *)
    let joined =
      genbank [
        "     CDS             join(1..9,20..28)";
        "                     /gene=\"abcA\"";
        "                     /locus_tag=\"DEMO_0001\"" ]
      |> A.GenBank.of_string in
    Testing.check_int "a joined CDS is a single feature with two intervals"
      ~expected:2
      (match feature_at joined "CDS" with
       | Some (_, f) -> List.length f.A.Annotation.intervals
       | None -> 0);
    let written = A.GFF3.to_string joined in
    Testing.check
      ~known_bug:"row_of_feature emits one row per interval; walk_dfs never re-merges"
      "a joined CDS is written as one GFF3 row"
      (fun () -> count_substring "\tCDS\t" written = 1);
    (* The feature's identity -- here derived from /locus_tag, not carried as an
       ID attribute -- is never written, so nothing downstream can rejoin the
       rows above into the one feature they came from. *)
    Testing.check
      ~known_bug:"row_of_feature emits neither ID= nor Parent=, so identity is lost"
      "a feature id derived from /locus_tag is written as ID="
      (fun () -> count_substring "ID=" written > 0);
    (* Every GenBank feature lives at annotation->source->X, and an Annotation.t
       holds exactly one hierarchy, so GFF3 output of a GenBank register cannot
       be read back at all.  This is the structural reason GFF3 cannot serve as
       the register's text twin. *)
    Testing.check_does_not_raise
      ~known_bug:"GenBank pins features at depth 3; GFF3's default hierarchy rejects them"
      "a GenBank register survives a GFF3 round trip"
      (fun () -> A.GFF3.to_string joined |> A.GFF3.of_string))

(* Attribute ordering. *)

let test_attribute_order () =
  Testing.section "Attribute ordering" (fun () ->
    let ann =
      genbank [
        "     CDS             1..15";
        "                     /product=\"hypothetical\"";
        "                     /gene=\"abcA\"" ]
      |> A.GenBank.of_string in
    let keys = ref [] in
    (match feature_at ann "CDS" with
     | Some (_, f) -> A.Annotation.attr_iter ann (fun k _ -> List.accum keys k) f
     | None -> ());
    (* The comment at Annotations.ml:426 claims source order.  attrs_of_qualifiers
       folds a StringMap alphabetically and AttrMap is keyed by the resulting
       intern ids, so what comes out is global first-intern order instead.  Any
       format wanting stable output must sort explicitly rather than rely on
       this. *)
    Testing.check_string
      ~known_bug:"attrs_of_qualifiers folds a StringMap, so order is intern order"
      "attributes are emitted in source order"
      ~expected:"product,gene" (List.rev !keys |> String.concat ","))

(* Insertion invariants.  These are not defects -- they are the contract a
   tabular reader has to satisfy, pinned so that a change to it is deliberate. *)

let test_add_invariants () =
  Testing.section "Annotation.add invariants" (fun () ->
    let h = A.Hierarchy.of_string "(gene ((mRNA (exon))))" in
    let ann = A.Annotation.create h in
    let feature seq =
      { A.Annotation.empty_feature with
        A.Annotation.seq = A.Annotation.intern_seq ann seq;
        intervals = [ { T.low = 0; length = 10 } ] } in
    Testing.check_string "a hierarchy round-trips through to_string"
      ~expected:"(gene ((mRNA (exon))))" (A.Hierarchy.to_string h);
    (* The DFS-ordered-insertion invariant: an internal path segment must already
       exist as the most recent node at the previous level.  This is exactly why
       a filtered tabular file cannot be read back without either synthesising
       the missing parents or refusing the input. *)
    Testing.check_raises ~re:".*No parent of category.*"
      "adding a child whose parent is absent raises"
      (fun () -> A.Annotation.add ann ~path:[ "annotation"; "gene"; "mRNA" ] (feature "chr1"));
    Testing.check_raises ~re:".*not valid against hierarchy.*"
      "adding a feature at a path outside the hierarchy raises"
      (fun () -> A.Annotation.add ann ~path:[ "annotation"; "nonesuch" ] (feature "chr1"));
    Testing.check_does_not_raise "adding a root-level feature succeeds"
      (fun () -> A.Annotation.add ann ~path:[ "annotation"; "gene" ] (feature "chr1")))

(* Sequence handling that AnnoTools extraction depends on.  These live in
   Sequences rather than Annotations, but they decide whether --extract-dna
   would return the right bases, so they are pinned here until that action
   exists and can own them. *)

let test_extraction_prerequisites () =
  Testing.section "Extraction prerequisites (Sequences)" (fun () ->
    Testing.check_string "reverse complement of an unambiguous sequence"
      ~expected:"ACCGGGCAT" (Sequences.Lint.rc "ATGCCCGGT");
    (* dnaize folds every non-ACGT byte to N.  AnnoTools.ml:342 loads the
       reference with this default, so an IUPAC-containing reference is N-masked
       before any feature sequence is ever extracted from it. *)
    Testing.check_string "the default linter folds IUPAC codes to N"
      ~expected:"ACGTNNNN"
      (Sequences.Lint.dnaize ~keep_lowercase:false ~keep_dashes:false "acgtrykm");
    (* ... and rc must complement the ambiguity codes too, or relaxing the linter
       would turn honest Ns into WRONG bases on the minus strand: R (A/G) has to
       become Y (C/T), not stay R.  The two therefore had to be fixed together. *)
    Testing.check_string "reverse complement handles IUPAC ambiguity codes"
      ~expected:"YR" (Sequences.Lint.rc "YR");
    (* Reversed, then complemented pairwise: N D H V B K M R Y W S U T G C A
       becomes N H D B V M K Y R W S A A C G T.  U complements to A, which is
       why it is excluded from the involution check below. *)
    Testing.check_string "every IUPAC code has a complement"
      ~expected:"NHDBVMKYRWSAACGT"
      (Sequences.Lint.rc "ACGTUSWYRMKBVHDN");
    (* Complementing is an involution, so rc . rc is the identity.  This is the
       check that would catch a table entry that maps two codes to the same
       complement. *)
    let iupac = "ACGTSWYRMKBVHDNacgtswyrmkbvhdn" in
    Testing.check_string "reverse complement is an involution"
      ~expected:iupac (Sequences.Lint.rc (Sequences.Lint.rc iupac));
    Testing.check_string "case is preserved by reverse complement"
      ~expected:"yrYR" (Sequences.Lint.rc "YRyr");
    (* Gaps and anything outside the nucleotide alphabet are carried through, so
       an aligned sequence can be reverse-complemented without losing its
       columns. *)
    Testing.check_string "gaps survive reverse complement"
      ~expected:"C-G" (Sequences.Lint.rc "C-G"))

let run () =
  test_locations ();
  test_genbank_records ();
  test_feature_sequence ();
  test_attributes ();
  test_gff3_fidelity ();
  test_attribute_order ();
  test_add_invariants ();
  test_extraction_prerequisites ()
