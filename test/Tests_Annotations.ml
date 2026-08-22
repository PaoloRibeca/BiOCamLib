(*
    Tests_Annotations.ml -- (c) 2026 Paolo Ribeca, <paolo.ribeca@gmail.com>

    This file is part of BiOCamLib, the OCaml foundations upon which
    a number of the bioinformatics tools I developed are built.

    Tests_Annotations.ml exercises the Annotations subsystem and the
    parts of Sequences that AnnoTools leans on.  Checks carrying a
    [~known_bug] marker pin a defect listed among the open items of the
    Annotations section of DocsYard/BiOCamLib/docs/BiOCamLib-design.tex;
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
  List.map (fun ((_: string option), (i: T.simple_interval_t)) ->
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
    Testing.check_string "a join whose parts are each complemented is one reverse feature"
      ~expected:"-"
      (location "join(complement(20..28),complement(1..9))" |> snd |> strand_to_string);
    (* INSDC 3.4.3 gives these as two spellings of ONE feature, so they must
       resolve identically.  They arrive in opposite orders -- a distributed
       complement lists its parts 5' to 3' along the feature, the wrapped form
       lists them along the sequence -- and everything downstream assumes the
       latter, so the former has to be put back. *)
    Testing.check_string "a distributed complement stores its parts in genomic order"
      ~expected:(location "complement(join(1..9,20..28))" |> fst |> intervals_to_string)
      (location "join(complement(20..28),complement(1..9))" |> fst |> intervals_to_string);
    (* Trans-splicing: one part forward, one part reverse.  A feature_t carries a
       single strand, so this is not representable and is refused.  It used to
       keep whichever strand came last, which silently reverse-complemented the
       parts that disagreed with it -- a wrong answer rather than no answer.
       Representing it properly would need a strand per interval. *)
    Testing.check_raises ~re:".*[Mm]ixed-strand.*"
      "a mixed-strand join is refused rather than silently flattened"
      (fun () -> location "join(1..9,complement(20..28))");
    (* GenBank spells a between-bases site lo^hi, so it has no use for the
       inverted pair the shared 1-based helper tolerates on behalf of the
       formats that cannot spell one.  Here 200..199 is simply malformed. *)
    Testing.check_raises ~re:".*between-bases site is spelled.*"
      "an inverted GenBank range is refused rather than read as a zero-length site"
      (fun () -> location "200..199"))

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
    (* /codon_start is 1-based against the 0-based phase, so 2 means phase 1. *)
    Testing.check_equal "/codon_start=2 becomes phase 1"
      ~to_string:(function Some n -> string_of_int n | None -> "none")
      ~expected:(Some 1)
      (match feature_at ann "CDS" with
       | Some (_, f) -> f.A.Annotation.phase
       | None -> None);
    Testing.check_equal "a CDS with no /codon_start has no phase"
      ~to_string:(function Some n -> string_of_int n | None -> "none")
      ~expected:None
      (let plain =
         genbank [ "     CDS             1..15"; "                     /gene=\"abcA\"" ]
         |> A.GenBank.of_string in
       match feature_at plain "CDS" with
       | Some (_, f) -> f.A.Annotation.phase
       | None -> Some 99);
    Testing.check_raises ~re:".*codon_start.*"
      "an out-of-range /codon_start is refused"
      (fun () ->
        genbank [
          "     CDS             1..15";
          "                     /codon_start=\"7\"" ]
        |> A.GenBank.of_string))

(* GenBank round trip.  Unlike GFF3, GenBank keeps a joined feature as one
   feature and can spell every location this AST can hold, so it is the format
   whose round trip should be exact. *)

let test_genbank_round_trip () =
  Testing.section "GenBank round trip" (fun () ->
    let round_trip feature_lines =
      let once = genbank feature_lines |> A.GenBank.of_string in
      A.GenBank.to_string once,
      A.GenBank.to_string (A.GenBank.of_string (A.GenBank.to_string once)) in
    let locations text =
      List.filter_map (fun l ->
        let l = String.trim l in
        match String.Split.on_char_as_list ' ' l with
        | k :: rest when k = "CDS" || k = "misc_feature" ->
          Some (String.concat "" rest)
        | _ -> None) (String.Split.on_char_as_list '\n' text) in
    let cases = [
      "a plain range", [ "     CDS             1..15" ], "1..15";
      "a reverse range", [ "     CDS             complement(16..30)" ], "complement(16..30)";
      "a join", [ "     CDS             join(1..9,20..28)" ], "join(1..9,20..28)";
      "a reverse join",
        [ "     CDS             complement(join(1..9,20..28))" ],
        "complement(join(1..9,20..28))";
      (* The writer used to render this as the reversed range 101..100, which its
         own reader then refused. *)
      "a zero-length site", [ "     misc_feature    100^101" ], "100^101"
    ] in
    List.iter (fun (name, lines_, expected) ->
      let first, second = round_trip lines_ in
      Testing.check_string (Printf.sprintf "%s survives the writer" name)
        ~expected (locations first |> String.concat ",");
      Testing.check_string (Printf.sprintf "%s is stable on a second pass" name)
        ~expected:first second) cases;
    (* /codon_start is now read into phase, and has to come back out again. *)
    let with_phase, _ =
      round_trip [
        "     CDS             1..15";
        "                     /codon_start=\"2\"" ] in
    Testing.check "a /codon_start survives the writer"
      (fun () -> count_substring "/codon_start=\"2\"" with_phase = 1))

(* GenBank header structure. *)

let test_genbank_headers () =
  Testing.section "GenBank headers" (fun () ->
    (* GenBank has three levels: a keyword in column 1, a SUB-keyword in column
       3, and continuation lines in column 13.  Treating every indented line as
       a continuation folded ORGANISM into SOURCE's value, so a record's
       organism came back as part of a run-on sentence rather than as a field of
       its own. *)
    let ann =
      lines [
        "LOCUS       demo01     30 bp    DNA              circular UNK";
        "DEFINITION  Synthetic test entry.";
        "SOURCE      synthetic construct";
        "  ORGANISM  synthetic construct";
        "            other sequences; artificial sequences.";
        "FEATURES             Location/Qualifiers";
        "     source          1..30";
        "//";
        "" ]
      |> A.GenBank.of_string in
    Testing.check_string "SOURCE keeps only its own value"
      ~expected:"synthetic construct"
      (match A.Annotation.get_metadata ann "SOURCE" with v :: _ -> v | [] -> "(absent)");
    (* The sub-keyword becomes a field in its own right.  Its own continuation
       lines still fold into it, which is ordinary continuation behaviour --
       GenBank's distinction between the organism name and the lineage below it
       is not modelled by a flat metadata map. *)
    Testing.check_string "ORGANISM becomes a field of its own"
      ~expected:"synthetic construct other sequences; artificial sequences."
      (match A.Annotation.get_metadata ann "ORGANISM" with v :: _ -> v | [] -> "(absent)");
    (* Written back, a sub-keyword sits in column 3 and follows the keyword it
       belongs to -- the metadata map is ordered by key, so without that it
       would come out at column 1 and in alphabetical position. *)
    let written = A.GenBank.to_string ann in
    Testing.check "ORGANISM is written back indented, under SOURCE"
      (fun () ->
        let ls = String.Split.on_char_as_list '\n' written in
        let rec after_source = function
          | a :: b :: _ when String.length a >= 6 && String.sub a 0 6 = "SOURCE" ->
            String.length b >= 10 && String.sub b 0 10 = "  ORGANISM"
          | _ :: rest -> after_source rest
          | [] -> false in
        after_source ls);
    Testing.check_string "the header structure is stable on a second pass"
      ~expected:written (A.GenBank.to_string (A.GenBank.of_string written)))

(* GFF3 and the sequence it can carry. *)

let test_gff3_fasta () =
  Testing.section "GFF3 ##FASTA" (fun () ->
    (* ##FASTA is a standard GFF3 directive: it ends the annotation and says the
       rest of the file is sequence.  Without it a GenBank record -- which is
       self-contained -- lost its ORIGIN on the way through GFF3. *)
    let with_seq =
      gff3 [ "chr1\tdemo\tgene\t1\t9\t.\t+\t.\tID=g1";
             "##FASTA";
             ">chr1";
             "ATGCCCGGGTAAGCG" ]
      |> A.GFF3.of_string in
    Testing.check_string "a ##FASTA section becomes the annotation's reference"
      ~expected:"ATGCCCGGGTAAGCG"
      (match A.Annotation.reference with_seq with
       | None -> "(no reference)"
       | Some r -> Sequences.Reference.find r (T.Forward "chr1") |> fst);
    (* Which means extraction works straight off a GFF3 file, with no separate
       --from-fasta. *)
    Testing.check_string "a feature's DNA can be extracted from it"
      ~expected:"ATGCCCGGG"
      (match feature_at with_seq "gene" with
       | Some (_, f) -> A.Annotation.feature_dna with_seq f
       | None -> "(no gene)");
    Testing.check "the writer emits ##FASTA when a reference is attached"
      (fun () -> count_substring "##FASTA" (A.GFF3.to_string with_seq) = 1);
    Testing.check "a register with no reference emits no ##FASTA"
      (fun () ->
        let plain = gff3 [ "chr1\tdemo\tgene\t1\t9\t.\t+\t.\tID=g1" ] |> A.GFF3.of_string in
        count_substring "##FASTA" (A.GFF3.to_string plain) = 0);
    Testing.check_string "the sequence survives a GFF3 round trip"
      ~expected:"ATGCCCGGGTAAGCG"
      (let back = A.GFF3.to_string with_seq |> A.GFF3.of_string in
       match A.Annotation.reference back with
       | None -> "(no reference)"
       | Some r -> Sequences.Reference.find r (T.Forward "chr1") |> fst);
    (* A row after the directive is sequence, not a feature: the walk stops
       there rather than trying to read FASTA as tab-separated rows. *)
    Testing.check_int "nothing after ##FASTA is read as a feature"
      ~expected:1 (List.length (features with_seq)))

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
    (* The two INSDC spellings of one minus-strand joined feature have to give
       the same sequence and the same protein.  They did not: the parts arrive
       in opposite orders, so the halves came out swapped. *)
    Testing.check_string "both spellings of a reverse join give the same DNA"
      ~expected:(dna (cds "complement(join(1..9,20..28))" []))
      (dna (cds "join(complement(20..28),complement(1..9))" []));
    Testing.check_string "both spellings of a reverse join give the same protein"
      ~expected:(protein (cds "complement(join(1..9,20..28))" []))
      (protein (cds "join(complement(20..28),complement(1..9))" []));
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
    (* A CDS whose 5' end is partial does not begin at a start codon at all, so
       rewriting its first codon to M invents a residue the record does not
       claim -- TTG is Leu here, not Met.  Fixing it properly needs the
       partiality the LOCATION parser already sees but discards at the boundary
       with feature_t (endpoint_t carries fuzzy_left/fuzzy_right; feature_t has
       nowhere to put them), which is the same missing slot the feature-table
       writer needs and is tracked as an open item in the design note. *)
    Testing.check_string
      ~known_bug:"5' partiality is parsed and then dropped, so feature_protein cannot see it"
      "a 5'-partial CDS does not have its first codon rewritten to methionine"
      ~expected:"LPG"
      (protein
         (genbank ~seq:"ttgcccgggtaagcgactagcgcatcgtca"
            [ "     CDS             <1..15" ]
          |> A.GenBank.of_string));
    (* Extraction needs a reference, so asking without one raises rather than
       returning "".  It raises as Initialize, not Algorithm: it is an ordinary
       mistake by the caller, and Exception.handle would otherwise treat it as a
       library bug -- printing a backtrace and inviting a report for something
       the user fixes by loading a reference. *)
    Testing.check_raises ~re:".*no reference sequence is attached.*"
      "extracting without a reference raises"
      (fun () ->
        let ann = A.GFF3.of_string (gff3 [ "chr1\tdemo\tgene\t1\t9\t.\t+\t.\tID=g1" ]) in
        match feature_at ann "gene" with
        | Some (_, f) -> A.Annotation.feature_dna ann f
        | None -> ""))

(* Feature selection. *)

let test_selection () =
  Testing.section "Feature selection" (fun () ->
    let ann =
      genbank [
        "     gene            1..15";
        "                     /gene=\"abcA\"";
        "                     /locus_tag=\"DEMO_0001\"";
        "     CDS             1..15";
        "                     /gene=\"abcA\"";
        "                     /product=\"hypothetical\"";
        "     gene            complement(16..30)";
        "                     /gene=\"xyzB\"";
        "                     /locus_tag=\"DEMO_0002\"" ]
      |> A.GenBank.of_string in
    let count s = A.Selection.count ann s in
    let re f r = A.Selection.Regexps [ f, Str.regexp r ] in
    Testing.check_int "the default selection matches every feature"
      ~expected:4 (count A.Selection.All);
    Testing.check_int "a structural field selects on the leaf category"
      ~expected:1 (count (re "type" "CDS"));
    Testing.check_int "a structural field selects on the strand"
      ~expected:1 (count (re "strand" "-"));
    Testing.check_int "an unknown field name is read as an attribute key"
      ~expected:1 (count (re "gene" "xyz"));
    Testing.check_int "an attribute absent from a feature does not match it"
      ~expected:2 (count (re "locus_tag" "DEMO"));
    Testing.check_int "an empty field name matches the feature label"
      ~expected:1 (count (re "" "DEMO_0002"));
    Testing.check_int "labels select by feature id"
      ~expected:2 (count (A.Selection.Labels (StringSet.of_list [ "DEMO_0001"; "DEMO_0002" ])));
    (* Several criteria are ANDed, so adding one can only narrow the result. *)
    Testing.check_int "several criteria are ANDed"
      ~expected:1
      (count (A.Selection.Regexps [ "type", Str.regexp "gene"; "gene", Str.regexp "abc" ]));
    Testing.check_int "negation is the complement within the register"
      ~expected:3 (count (A.Selection.Not (re "type" "CDS")));
    Testing.check_int "negating everything selects nothing"
      ~expected:0 (count (A.Selection.Not A.Selection.All));
    (* The regexps are Str's, so they are unanchored substring searches -- worth
       pinning, because it is the difference between "gene" matching only the
       gene category and it also matching a category containing that word. *)
    Testing.check_int "a regexp is an unanchored search"
      ~expected:4 (count (re "path" "source"));
    Testing.check_string "a criterion describes itself for diagnostics"
      ~expected:"not (regexps {type})"
      (A.Selection.to_string (A.Selection.Not (re "type" "CDS"))))

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
    (* Decoding without re-encoding was lossy: the comma re-emerged as a value
       separator on the next read, so the value silently split in two. *)
    Testing.check "a comma inside a value is re-encoded on write"
      (fun () -> count_substring "%2C" (A.GFF3.to_string ann) > 0);
    (* The property that actually matters is not which bytes get encoded but
       that a value survives being written and read again. *)
    let round_tripped key =
      let again = A.GFF3.to_string ann |> A.GFF3.of_string in
      match feature_at again "gene" with
      | Some (_, f) ->
        (match A.Annotation.attr_get again f key with
         | Some vs -> String.concat "|" vs
         | None -> "(absent)")
      | None -> "(no gene)" in
    Testing.check_string "a value containing a comma survives a round trip"
      ~expected:"alpha,beta" (round_tripped "Note");
    Testing.check_string "a value containing a space survives a round trip"
      ~expected:"x y" (round_tripped "Desc");
    (* Column 9 has no way to spell a valueless qualifier other than a bare '=',
       so the grammar has to accept one: a GenBank /pseudo or
       /ribosomal_slippage could otherwise be written but not read back. *)
    Testing.check_does_not_raise "an empty attribute value can be read back"
      (fun () ->
        gff3 [ "chr1\tdemo\tgene\t100\t500\t.\t+\t.\tID=g1;pseudo=" ] |> A.GFF3.of_string);
    Testing.check_string "an empty attribute value reads back as present-but-empty"
      ~expected:""
      (let one =
         gff3 [ "chr1\tdemo\tgene\t100\t500\t.\t+\t.\tID=g1;pseudo=" ] |> A.GFF3.of_string in
       match feature_at one "gene" with
       | Some (_, f) ->
         (match A.Annotation.attr_get one f "pseudo" with
          | Some (v :: _) -> v
          | Some [] -> "(no values)"
          | None -> "(absent)")
       | None -> "(no gene)");
    (* A separator that arrives percent-encoded must not be decoded into a
       structural role: %3D is a literal '=' inside a value, not the pair's
       separator. *)
    Testing.check_string "an encoded separator stays inside the value"
      ~expected:"a=b;c"
      (let tricky =
         gff3 [ "chr1\tdemo\tgene\t100\t500\t.\t+\t.\tID=g1;Note=a%3Db%3Bc" ]
         |> A.GFF3.of_string in
       let again = A.GFF3.to_string tricky |> A.GFF3.of_string in
       match feature_at again "gene" with
       | Some (_, f) ->
         (match A.Annotation.attr_get again f "Note" with
          | Some (v :: _) -> v
          | _ -> "(absent)")
       | None -> "(no gene)");
    (* A value that arrives with an UNENCODED space has to be readable: GFF3
       permits one, and third-party files carry them routinely
       (product=hypothetical protein).  The lexer used to treat a space as a
       token separator, so this was a parse error. *)
    Testing.check_does_not_raise "a value with an unencoded space can be read"
      (fun () ->
        gff3 [ "chr1\tdemo\tgene\t100\t500\t.\t+\t.\tproduct=hypothetical protein" ]
        |> A.GFF3.of_string);
    Testing.check_string "and keeps the space"
      ~expected:"hypothetical protein"
      (let one =
         gff3 [ "chr1\tdemo\tgene\t100\t500\t.\t+\t.\tproduct=hypothetical protein" ]
         |> A.GFF3.of_string in
       match feature_at one "gene" with
       | Some (_, f) ->
         (match A.Annotation.attr_get one f "product" with
          | Some (v :: _) -> v
          | _ -> "(absent)")
       | None -> "(no gene)");
    (* The same rule used to swallow the space in a comma-separated list, which
       was corruption rather than a refusal and so the more dangerous half of
       the same defect.  The space belongs to the value that follows it. *)
    Testing.check_string "a space after a comma is not swallowed"
      ~expected:"alpha| beta"
      (let one =
         gff3 [ "chr1\tdemo\tgene\t100\t500\t.\t+\t.\tName=alpha, beta" ] |> A.GFF3.of_string in
       match feature_at one "gene" with
       | Some (_, f) ->
         (match A.Annotation.attr_get one f "Name" with
          | Some vs -> String.concat "|" vs
          | None -> "(absent)")
       | None -> "(no gene)"))

(* GFF3 fidelity. *)

let test_gff3_fidelity () =
  Testing.section "GFF3 fidelity" (fun () ->
    let scored =
      gff3 [ "chr1\tdemo\tgene\t100\t500\t42.5\t+\t.\tID=g1" ] |> A.GFF3.of_string in
    Testing.check "the score column survives a round trip"
      (fun () -> count_substring "42.5" (A.GFF3.to_string scored) > 0);
    Testing.check_equal "a score reads back as a float"
      ~to_string:(function Some f -> string_of_float f | None -> "none")
      ~expected:(Some 42.5)
      (match feature_at scored "gene" with
       | Some (_, f) -> f.A.Annotation.score
       | None -> None);
    (* '.' is "no score", which is not the same as a score of zero. *)
    Testing.check_equal "an absent score stays absent"
      ~to_string:(function Some f -> string_of_float f | None -> "none")
      ~expected:None
      (let unscored =
         gff3 [ "chr1\tdemo\tgene\t100\t500\t.\t+\t.\tID=g1" ] |> A.GFF3.of_string in
       match feature_at unscored "gene" with
       | Some (_, f) -> f.A.Annotation.score
       | None -> Some 0.);
    Testing.check_raises ~re:".*[Ss]core.*" "a non-numeric score is refused"
      (fun () ->
        gff3 [ "chr1\tdemo\tgene\t100\t500\tnonsense\t+\t.\tID=g1" ] |> A.GFF3.of_string);
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
    (* GFF3 spells a discontinuous feature as several rows sharing one [ID],
       so two rows is the correct rendering of a joined CDS -- what matters is
       that they carry the identity that lets a reader put them back together,
       which for a long time they did not. *)
    Testing.check_int "a joined CDS is written as one row per interval"
      ~expected:2 (count_substring "\tCDS\t" written);
    (* The identity comes from /locus_tag, which takes precedence over /gene. *)
    Testing.check "a feature id derived from /locus_tag is written as ID="
      (fun () -> count_substring "ID=DEMO_0001" written > 0);
    Testing.check "and both rows of the joined CDS carry the same one"
      (fun () -> count_substring "ID=DEMO_0001" written = 2);
    (* Which is the property that matters: the rows go back together. *)
    Testing.check_int "so the rows read back as one feature with two intervals"
      ~expected:2
      (let back = A.GFF3.of_string ~hierarchy:(A.Annotation.hierarchy joined) written in
       match feature_at back "CDS" with
       | Some (_, f) -> List.length f.A.Annotation.intervals
       | None -> 0);
    (* Column 8 is per ROW: it says how many bases of that row's first codon sit
       in the rows before it.  Stamping the feature's phase on every row of a
       multi-exon CDS is right only for the first.  With a 10-base first exon
       and phase 0, the second row starts two bases into a codon. *)
    Testing.check_string "phase is recomputed for each row of a multi-exon CDS"
      ~expected:"0,2"
      (let spliced =
         genbank [
           "     CDS             join(1..10,20..28)";
           "                     /codon_start=\"1\"" ]
         |> A.GenBank.of_string in
       String.Split.on_char_as_list '\n' (A.GFF3.to_string spliced)
       |> List.filter_map (fun l ->
         match String.Split.on_char_as_list '\t' l with
         | _ :: _ :: "CDS" :: _ :: _ :: _ :: _ :: phase :: _ -> Some phase
         | _ -> None)
       |> String.concat ",");
    (* A GenBank register now keeps its shape through GFF3, because the writer
       derives [Parent] from the forest.  It has to be read back under the
       hierarchy it was written from: a GenBank register hangs everything off a
       [source] feature, and GFF3's default schema has no [source], so the two
       genuinely disagree.  That is a schema to supply, not a defect -- which is
       what [--hierarchy] and [--dialect] are for. *)
    Testing.check_does_not_raise "a GenBank register survives a GFF3 round trip"
      (fun () ->
        A.GFF3.to_string joined
        |> A.GFF3.of_string ~hierarchy:(A.Annotation.hierarchy joined));
    Testing.check_string "and comes back at the same path"
      ~expected:"annotation->source->CDS"
      (let back =
         A.GFF3.to_string joined
         |> A.GFF3.of_string ~hierarchy:(A.Annotation.hierarchy joined) in
       match feature_at back "CDS" with
       | Some (path, _) -> A.Annotation.path_to_string path
       | None -> "(no CDS)");
    (* Read under GFF3's own default instead and it is refused, which is the
       correct answer rather than a silent reshaping. *)
    Testing.check_raises "reading it under GFF3's default hierarchy is refused"
      (fun () -> ignore (A.GFF3.to_string joined |> A.GFF3.of_string));
    (* Which is the difference the tabular format exists for.  GFF3 carries the
       edges between features and never the schema those edges satisfy, so it
       has to be told; a tabular document states its hierarchy in its metadata
       and so reads back knowing nothing about it.  The same register, the same
       absence of an explicit hierarchy, opposite outcomes. *)
    Testing.check_string "a tabular document reads back without being told its hierarchy"
      ~expected:"annotation->source->CDS"
      (let back = A.Tabular.to_string joined |> A.Tabular.of_string in
       match feature_at back "CDS" with
       | Some (path, _) -> A.Annotation.path_to_string path
       | None -> "(no CDS)");
    Testing.check_string "and brings the hierarchy itself back with it"
      ~expected:(A.Hierarchy.to_string (A.Annotation.hierarchy joined))
      (A.Hierarchy.to_string
         (A.Annotation.hierarchy (A.Tabular.to_string joined |> A.Tabular.of_string))))

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
    (* attrs_of_qualifiers folds a StringMap and AttrMap is keyed by the
       resulting intern ids, so what comes out is global first-intern order,
       not the order the qualifiers appeared in.  Preserving true per-feature
       source order would take a list rather than a map, so this is pinned
       rather than fixed -- and a format that wants a predictable order has to
       sort explicitly, which is what the tabular writer does. *)
    Testing.check_string
      ~known_bug:"AttrMap is keyed by intern id, so per-feature source order is not kept"
      "attributes are emitted in source order"
      ~expected:"product,gene" (List.rev !keys |> String.concat ",");
    (* What IS guaranteed is that the order is deterministic: the same input
       gives the same output, which is what a diffable text format needs. *)
    Testing.check "attribute order is at least deterministic"
      (fun () ->
        let keys_of a =
          let acc = ref [] in
          (match feature_at a "CDS" with
           | Some (_, f) -> A.Annotation.attr_iter a (fun k _ -> List.accum acc k) f
           | None -> ());
          List.rev !acc in
        let build () =
          genbank [
            "     CDS             1..15";
            "                     /product=\"hypothetical\"";
            "                     /gene=\"abcA\"" ]
          |> A.GenBank.of_string in
        keys_of (build ()) = keys_of (build ())))

(* The tabular format. *)

(* The three table headers.  Each opens its table and, being distinct, names it:
   that single rule is the format's whole framing. *)
let features_header =
  "#id\t#parent\t#seq\t#path\t#feature_id\t#source\t#score\t#strand\t#phase\t#intervals"
let attributes_header = "#id\t#key\t#value"
let metadata_header = "#key\t#value"

(* Reverse the DATA rows of the table opened by [wanted], leaving its header
   where it is.  Used to show that row order carries no meaning: the parent
   column is what rebuilds the forest. *)
let reverse_section wanted doc =
  let inside = ref false and out = ref [] and held = ref [] in
  let release () =
    List.iter (fun l -> List.accum out l) !held;
    held := [] in
  List.iter (fun line ->
    if line <> "" && line.[0] = '#' then begin
      release ();
      inside := line = wanted;
      List.accum out line
    end else if !inside && line <> "" then
      (* Accumulated in reverse, then released in that order. *)
      held := line :: !held
    else begin
      release ();
      List.accum out line
    end) (String.Split.on_char_as_list '\n' doc);
  release ();
  List.rev !out |> String.concat "\n"

let test_tabular () =
  Testing.section "Tabular format" (fun () ->
    let describe ann =
      (* A canonical summary of everything a feature carries, so that a round
         trip is compared on content rather than on byte-identical output. *)
      let acc = ref [] in
      A.Annotation.iter_paths (fun ~path f ->
        let attrs = ref [] in
        A.Annotation.attr_iter ann
          (fun k vs -> List.accum attrs (k ^ "=" ^ String.concat "," vs)) f;
        List.accum acc
          (Printf.sprintf "%s|%s|%s|%s|%s|%s|%s"
             (A.Annotation.path_to_string path) (A.Annotation.seq_name ann f)
             (List.map (fun (i: T.simple_interval_t) ->
                Printf.sprintf "%d+%d" i.low i.length) f.A.Annotation.intervals
              |> String.concat ",")
             (match f.A.Annotation.strand with
              | Some (T.Forward _) -> "+" | Some (T.Reverse _) -> "-" | None -> ".")
             (match f.A.Annotation.phase with Some n -> string_of_int n | None -> ".")
             (match f.A.Annotation.score with Some s -> string_of_float s | None -> ".")
             (List.sort compare !attrs |> String.concat ";"))) ann;
      List.rev !acc |> String.concat "\n" in
    let round_trip ann = A.Tabular.to_string ann |> A.Tabular.of_string in
    let check_round_trip name ann =
      Testing.check_string (Printf.sprintf "%s survives a tabular round trip" name)
        ~expected:(describe ann) (describe (round_trip ann));
      Testing.check_string (Printf.sprintf "%s renders identically on a second pass" name)
        ~expected:(A.Tabular.to_string ann) (A.Tabular.to_string (round_trip ann)) in
    let gb =
      genbank [
        "     gene            1..15";
        "                     /gene=\"abcA\"";
        "                     /locus_tag=\"DEMO_0001\"";
        "     CDS             join(1..9,20..28)";
        "                     /gene=\"abcA\"";
        "                     /codon_start=\"2\"";
        "                     /product=\"hypothetical protein, putative\"";
        "     misc_feature    10^11";
        "                     /note=\"insertion site\"" ]
      |> A.GenBank.of_string in
    check_round_trip "a GenBank register" gb;
    let gf =
      gff3 [
        "chr1\tdemo\tgene\t100\t500\t42.5\t+\t.\tID=gene1;Name=ABC1";
        "chr1\tdemo\tmRNA\t100\t500\t.\t+\t.\tID=mRNA1;Parent=gene1";
        "chr1\tdemo\texon\t100\t200\t.\t+\t.\tID=ex1;Parent=mRNA1";
        "chr1\tdemo\tCDS\t150\t200\t.\t+\t0\tID=cds1;Parent=mRNA1" ]
      |> A.GFF3.of_string in
    check_round_trip "a GFF3 register" gf;
    (* The three things GFF3's column 9 cannot express, which is most of why
       this format exists. *)
    let awkward =
      gff3 [ "chr1\tdemo\tgene\t100\t500\t.\t+\t.\t\
              ID=g1;Note=alpha%2Cbeta;multi=a,b,c;pseudo=;dot=%2E" ]
      |> A.GFF3.of_string in
    check_round_trip "values a packed column cannot hold" awkward;
    Testing.check_string "a multi-valued attribute stays multi-valued"
      ~expected:"a,b,c"
      (let back = round_trip awkward in
       match feature_at back "gene" with
       | Some (_, f) ->
         (match A.Annotation.attr_get back f "multi" with
          | Some vs -> String.concat "," vs
          | None -> "(absent)")
       | None -> "(no gene)");
    Testing.check_string "a value that is a bare dot is not read as absent"
      ~expected:"."
      (let back = round_trip awkward in
       match feature_at back "gene" with
       | Some (_, f) ->
         (match A.Annotation.attr_get back f "dot" with
          | Some (v :: _) -> v
          | _ -> "(absent)")
       | None -> "(no gene)");
    (* The hierarchy travels in the metadata table, so a register read back does
       not need it supplied again -- without that the format would be convenient
       rather than lossless. *)
    Testing.check_string "the hierarchy travels with the data"
      ~expected:(A.Hierarchy.to_string (A.Annotation.hierarchy gb))
      (A.Hierarchy.to_string (A.Annotation.hierarchy (round_trip gb)));
    (* The format reserves a [!] namespace for its own metadata keys and uses
       [#!] section banners in the one-document form, so an annotation's own key
       must not be able to impersonate either.  [add_metadata] accepts any key
       and GFF3 pragmas reach it, so both are constructible. *)
    List.iter (fun hostile_key ->
      Testing.check_string
        (Printf.sprintf "a metadata key of %S cannot impersonate the format's own" hostile_key)
        ~expected:"trap"
        (let hostile = A.Annotation.add_metadata gb ~key:hostile_key ~value:"trap" in
         match A.Annotation.get_metadata (round_trip hostile) hostile_key with
         | v :: _ -> v
         | [] -> "(absent)")) [ "#!features"; "!hierarchy"; "!format-version" ];
    Testing.check_string "annotation metadata travels with the data"
      ~expected:"Synthetic test entry."
      (match A.Annotation.get_metadata (round_trip gb) "DEFINITION" with
       | v :: _ -> v
       | [] -> "(absent)");
    (* Identity is chained through the parent, so two identical exons under two
       different transcripts are distinguishable -- the case that makes a hash
       of the feature alone insufficient, and that GENCODE hits constantly. *)
    let shared =
      gff3 [
        "chr1\tdemo\tgene\t100\t500\t.\t+\t.\tID=g1";
        "chr1\tdemo\tmRNA\t100\t500\t.\t+\t.\tID=t1;Parent=g1";
        "chr1\tdemo\texon\t100\t200\t.\t+\t.\tID=e1;Parent=t1";
        "chr1\tdemo\tmRNA\t100\t500\t.\t+\t.\tID=t2;Parent=g1";
        "chr1\tdemo\texon\t100\t200\t.\t+\t.\tID=e2;Parent=t2" ]
      |> A.GFF3.of_string in
    Testing.check "identical exons under different transcripts get different ids"
      (fun () ->
        let ids =
          String.Split.on_char_as_list '\n' (A.Tabular.to_string shared)
          |> List.filter_map (fun l ->
            match String.Split.on_char_as_list '\t' l with
            | id :: _ :: _ :: path :: _ when path = "gene->mRNA->exon" -> Some id
            | _ -> None) in
        List.length ids = 2 && List.nth ids 0 <> List.nth ids 1);
    check_round_trip "a register with shared exons" shared;
    (* Row order carries no meaning: the parent column is what rebuilds the
       forest, so either file may be sorted and still read back. *)
    (* Reordering the rows reorders the siblings that come back -- the forest is
       rebuilt from the parent links, and nothing records which sibling came
       first -- so the claim being checked is that the same features return, not
       that they return in the same order. *)
    let as_set d = String.Split.on_char_as_list '\n' d |> List.sort compare |> String.concat "\n" in
    Testing.check_string "the features table may be reordered"
      ~expected:(as_set (describe shared))
      (as_set
         (describe
            (A.Tabular.of_string (reverse_section features_header (A.Tabular.to_string shared)))));
    Testing.check_string "the attributes table may be reordered"
      ~expected:(describe shared)
      (describe
         (A.Tabular.of_string (reverse_section attributes_header (A.Tabular.to_string shared))));
    (* A malformed table is refused rather than half-read. *)
    (* A [#] line matching none of the three headers names no table, so it can
       be neither a row nor something to skip. *)
    Testing.check_raises ~re:".*names no known table.*"
      "a header naming no known table is refused"
      (fun () -> A.Tabular.of_string (metadata_header ^ "\n#wrong\theader\n"));
    Testing.check_raises ~re:".*before any table header.*"
      "a row appearing before the first header is refused"
      (fun () -> A.Tabular.of_string "orphan\trow\n");
    (* A row the walk never arrives at, and an attributes row attaching to
       nothing, would both be dropped without a word.  The house rule is that a
       defined format never fails silently. *)
    let doc_with rows attrs =
      String.concat "\n"
        ([ metadata_header; "!format-version\t1"; "!hierarchy\t(source (gene, CDS))";
           features_header ]
         @ rows @ [ attributes_header ] @ attrs @ [ "" ]) in
    Testing.check_raises ~re:".*unreachable.*"
      "a feature whose parent is not in the table is refused"
      (fun () ->
        A.Tabular.of_string
          (doc_with
             [ "aaaaaaaaaaaaaaaa\t.\tdemo01\tsource\t.\t.\t.\t.\t.\t1..30";
               "bbbbbbbbbbbbbbbb\tnosuchparent\tdemo01\tsource->gene\t.\t.\t.\t.\t.\t1..15" ] []));
    (* A two-cycle has no root, so neither node is ever reached: the
       completeness check is what catches it, not the reached-twice guard. *)
    Testing.check_raises ~re:".*unreachable.*"
      "a cycle in the parent links is refused"
      (fun () ->
        A.Tabular.of_string
          (doc_with
             [ "aaaaaaaaaaaaaaaa\tbbbbbbbbbbbbbbbb\tdemo01\tsource\t.\t.\t.\t.\t.\t1..30";
               "bbbbbbbbbbbbbbbb\taaaaaaaaaaaaaaaa\tdemo01\tsource\t.\t.\t.\t.\t.\t1..30" ] []));
    Testing.check_raises ~re:".*attributes table names feature.*"
      "an attributes row attaching to no feature is refused"
      (fun () ->
        A.Tabular.of_string
          (doc_with
             [ "aaaaaaaaaaaaaaaa\t.\tdemo01\tsource\t.\t.\t.\t.\t.\t1..30" ]
             [ "cccccccccccccccc\tnote\torphan" ]));
    (* A between-bases site names two CONSECUTIVE positions.  Accepting any pair
       meant a hand-edited 100^999 parsed happily and was re-emitted as 100^101
       -- rewritten rather than diagnosed, in a format whose whole point is that
       you can edit it by hand. *)
    Testing.check_raises ~re:".*consecutive.*"
      "a between-bases site whose positions are not consecutive is refused"
      (fun () ->
        A.Tabular.of_string
          (doc_with [ "aaaaaaaaaaaaaaaa\t.\tdemo01\tsource\t.\t.\t.\t.\t.\t100^999" ] []));
    (* 1-based means positions start at 1.  A lo of 0 used to yield low = -1,
       which every writer then re-emitted happily and which surfaced only much
       later, as an internal error from the reference rather than a diagnosis. *)
    Testing.check_raises ~re:".*positions start at 1.*"
      "a non-positive coordinate is refused"
      (fun () ->
        A.Tabular.of_string
          (doc_with [ "aaaaaaaaaaaaaaaa\t.\tdemo01\tsource\t.\t.\t.\t.\t.\t0..500" ] []));
    Testing.check_raises ~re:".*positions start at 1.*"
      "a non-positive coordinate is refused by the GFF3 reader too"
      (fun () -> gff3 [ "chr1\tdemo\tgene\t0\t500\t.\t+\t.\tID=g1" ] |> A.GFF3.of_string);
    (* CRLF.  The module's own GFF3 reader strips a trailing CR; this one did
       not, so the CR landed inside the LAST field of every row -- for the
       attributes table, inside the value itself, which was then interned and
       re-emitted downstream as ID=gene1%0D.  Silent corruption, not an error. *)
    Testing.check_string "a CRLF document reads with no carriage returns left in it"
      ~expected:"alpha,beta"
      (let crlf =
         String.Split.on_char_as_list '\n' (A.Tabular.to_string awkward)
         |> String.concat "\r\n" in
       let back = A.Tabular.of_string crlf in
       match feature_at back "gene" with
       | Some (_, f) ->
         (match A.Annotation.attr_get back f "Note" with
          | Some (v :: _) -> v
          | _ -> "(absent)")
       | None -> "(no gene)");
    (* A metadata row is <key><TAB><value>, so a pair that is empty on both
       sides renders as a line that TRIMS to nothing -- and a bare ## pragma in
       a GFF3 file produces exactly that pair.  Filtering blank lines by trim
       therefore dropped a real entry. *)
    Testing.check_int "a metadata entry that is empty on both sides survives"
      ~expected:1
      (let empty_pragma =
         A.GFF3.of_string (gff3 [ "chr1\tdemo\tgene\t1\t9\t.\t+\t.\tID=g1" ])
         |> (fun a -> A.Annotation.add_metadata a ~key:"" ~value:"") in
       List.length (A.Annotation.get_metadata (round_trip empty_pragma) ""));
    (* The score column has to be exact here: this format claims to hold
       everything the binary archive holds, and %.12g silently rounds on the
       first crossing from an archive or from a column 6 with more digits. *)
    Testing.check_string "a score survives with full double precision"
      ~expected:"0.1234567890123457"
      (let precise =
         gff3 [ "chr1\tdemo\tgene\t100\t500\t0.1234567890123457\t+\t.\tID=g1" ]
         |> A.GFF3.of_string in
       match feature_at (round_trip precise) "gene" with
       | Some (_, f) ->
         (match f.A.Annotation.score with
          | Some s -> Printf.sprintf "%.16g" s
          | None -> "(absent)")
       | None -> "(no gene)");
    (* The parent column and the path column describe one forest twice, and
       Annotation.add places a feature by its path alone -- so if they disagree,
       which description wins depends on row order, and row order carrying no
       meaning is the whole point of the format. *)
    Testing.check_raises ~re:".*does not sit directly below.*"
      "a row whose parent and path disagree is refused"
      (fun () ->
        A.Tabular.of_string
          (doc_with
             [ "aaaaaaaaaaaaaaaa\t.\tdemo01\tsource\t.\t.\t.\t.\t.\t1..30";
               "bbbbbbbbbbbbbbbb\taaaaaaaaaaaaaaaa\tdemo01\tgene\t.\t.\t.\t.\t.\t1..15" ] []));
    (* The register is rebuilt around the hierarchy the file declares, so
       everything on the carrier that is not the hierarchy has to come across.
       The reference already did; the metadata was dropped without a word. *)
    Testing.check_string "the carrier's metadata survives a read into an empty register"
      ~expected:"kept"
      (let carrier =
         A.Annotation.add_metadata (A.Annotation.create (A.Annotation.hierarchy gb))
           ~key:"CARRIED" ~value:"kept" in
       let merged = A.Tabular.read carrier (A.Tabular.to_string gb) in
       match A.Annotation.get_metadata merged "CARRIED" with
       | v :: _ -> v
       | [] -> "(dropped)");
    (* The reference is register state, and a GenBank record carries its own, so
       a format that claims to be the register's text twin has to carry it too
       -- otherwise a GenBank -> tabular pipeline loses the sequence silently
       and every later extraction fails for want of it.  It travels as FASTA
       beside the tables: a sequence is not tabular data. *)
    Testing.check_string "the reference travels with the tables"
      ~expected:"ATGCCCGGGTAAGCGACTAGCGCATCGTCA"
      (let back = round_trip gb in
       match A.Annotation.reference back with
       | None -> "(no reference)"
       | Some r -> Sequences.Reference.find r (T.Forward "demo01") |> fst);
    (* Which is the property that actually matters: extraction has to keep
       working on the far side of a round trip. *)
    Testing.check_string "a feature's DNA is unchanged by a round trip"
      ~expected:
        (match feature_at gb "CDS" with
         | Some (_, f) -> A.Annotation.feature_dna gb f
         | None -> "(no CDS)")
      (let back = round_trip gb in
       match feature_at back "CDS" with
       | Some (_, f) -> A.Annotation.feature_dna back f
       | None -> "(no CDS)");
    Testing.check_string "a feature's protein is unchanged by a round trip"
      ~expected:
        (match feature_at gb "CDS" with
         | Some (_, f) -> A.Annotation.feature_protein gb f
         | None -> "(no CDS)")
      (let back = round_trip gb in
       match feature_at back "CDS" with
       | Some (_, f) -> A.Annotation.feature_protein back f
       | None -> "(no CDS)");
    (* A per-sequence translation table is recorded only when it is not the
       standard one, so an ordinary annotation carries no such rows -- but when
       there is one it has to survive, or the protein changes. *)
    Testing.check_string "a non-standard per-sequence translation table survives"
      ~expected:"11"
      (let with_table =
         A.Annotation.set_reference gb
           (Sequences.Reference.add_from_fasta_string ~linter:Fun.id
              ~tables:(StringMap.singleton "demo01" Sequences.Translation.Table_11)
              Sequences.Reference.empty ">demo01\nACGTACGTAC\n") in
       match A.Annotation.reference (round_trip with_table) with
       | None -> "(no reference)"
       | Some r ->
         Sequences.Reference.find r (T.Forward "demo01")
         |> snd |> Sequences.Translation.to_string);
    (* A register with no reference writes no FASTA at all, rather than an
       empty one, and reads back none. *)
    Testing.check "an annotation with no reference carries none"
      (fun () ->
        let gff_only = gff3 [ "chr1\tdemo\tgene\t1\t9\t.\t+\t.\tID=g1" ] |> A.GFF3.of_string in
        A.Annotation.reference (round_trip gff_only) = None);
    Testing.check_raises ~re:".*has no .* table.*"
      "a document missing a table is refused"
      (fun () -> A.Tabular.of_string (metadata_header ^ "\n"));
    (* The feature's own identifier is not always derivable from an attribute --
       the GenBank reader names a record's source feature after its LOCUS -- so
       it needs a column of its own.  Without one it was silently dropped, and
       the second render disagreed with the first. *)
    Testing.check_string "a feature id that is in no attribute still survives"
      ~expected:"demo01"
      (let back = round_trip gb in
       match List.nth_opt (features back) 0 with
       | Some (_, f) -> Option.value ~default:"(none)" f.A.Annotation.id
       | None -> "(no features)"))

(* The NCBI submission feature table. *)

let test_feature_table () =
  Testing.section "NCBI feature table" (fun () ->
    let tbl feature_lines = genbank feature_lines |> A.GenBank.of_string |> A.Tbl.to_string in
    let lines_of s =
      String.Split.on_char_as_list '\n' s |> List.filter (fun l -> l <> "") in
    let coordinate_lines s =
      lines_of s |> List.filter (fun l -> String.length l > 0 && l.[0] <> '\t' && l.[0] <> '>') in
    Testing.check_string "the block header names the sequence"
      ~expected:">Feature demo01"
      (match lines_of (tbl [ "     CDS             1..15" ]) with
       | first :: _ -> first
       | [] -> "(empty)");
    Testing.check_string "a forward feature is written low then high"
      ~expected:"1\t15\tCDS"
      (match coordinate_lines (tbl [ "     CDS             1..15" ]) with
       | _source :: cds :: _ -> cds
       | _ -> "(missing)");
    (* There is no strand column: the minus strand IS the inverted range. *)
    Testing.check_string "a reverse feature is written high then low"
      ~expected:"30\t16\tCDS"
      (match coordinate_lines (tbl [ "     CDS             complement(16..30)" ]) with
       | _source :: cds :: _ -> cds
       | _ -> "(missing)");
    Testing.check_string "extra intervals are bare coordinate lines"
      ~expected:"1\t9\tCDS|20\t28"
      (match coordinate_lines (tbl [ "     CDS             join(1..9,20..28)" ]) with
       | _source :: a :: b :: _ -> a ^ "|" ^ b
       | _ -> "(missing)");
    (* Intervals run 5' to 3' along the FEATURE, so a minus-strand join lists
       them in the reverse of the genomic order they are stored in. *)
    Testing.check_string "a reverse join lists its intervals in feature order"
      ~expected:"28\t20\tCDS|9\t1"
      (match coordinate_lines (tbl [ "     CDS             complement(join(1..9,20..28))" ]) with
       | _source :: a :: b :: _ -> a ^ "|" ^ b
       | _ -> "(missing)");
    Testing.check "a qualifier line carries three empty leading columns"
      (fun () ->
        lines_of (tbl [ "     CDS             1..15"; "                     /gene=\"abcA\"" ])
        |> List.exists (fun l -> l = "\t\t\tgene\tabcA"));
    (* A valueless qualifier simply omits the fifth column, which is how a
       feature table says "present, no value". *)
    Testing.check "a valueless qualifier omits its value column"
      (fun () ->
        lines_of (tbl [ "     CDS             1..15"; "                     /pseudo=\"\"" ])
        |> List.exists (fun l -> l = "\t\t\tpseudo"));
    (* /codon_start is the only slot a feature table has for a phase, so a phase
       that arrived in a GFF3 column has to be rendered as one. *)
    Testing.check "a phase with no /codon_start is written as one"
      (fun () ->
        let from_gff3 =
          gff3 [ "chr1\tdemo\tgene\t100\t500\t.\t+\t.\tID=g1";
                 "chr1\tdemo\tmRNA\t100\t500\t.\t+\t.\tID=t1;Parent=g1";
                 "chr1\tdemo\tCDS\t100\t500\t.\t+\t2\tID=c1;Parent=t1" ]
          |> A.GFF3.of_string |> A.Tbl.to_string in
        lines_of from_gff3 |> List.exists (fun l -> l = "\t\t\tcodon_start\t3"));
    (* A zero-length site has no spelling here at all, and quietly widening it
       to one base would be a submission that says something the annotation did
       not. *)
    Testing.check_raises ~re:".*zero-length.*"
      "a zero-length feature is refused rather than widened"
      (fun () -> tbl [ "     misc_feature    10^11" ]))

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

(* Insertion cost.  Not a benchmark: a guard on the ASYMPTOTICS.  Sibling lists
   are held most-recent-first precisely so that Annotation.add is O(1) at each
   level; restore the append-and-walk-to-the-end version and every functional
   check here still passes while a flat million-feature file goes from seconds
   to hours.  The threshold is deliberately loose -- linear predicts about 4,
   quadratic about 16 -- so that a loaded machine cannot make it flap. *)

let test_insertion_cost () =
  Testing.section "Insertion cost" (fun () ->
    let h = A.Hierarchy.of_string "gene" in
    let build n =
      let ann = ref (A.Annotation.create h) in
      let seq = A.Annotation.intern_seq !ann "chr1" in
      let started = Unix.gettimeofday () in
      for i = 0 to n - 1 do
        ann :=
          A.Annotation.add !ann ~path:[ "annotation"; "gene" ]
            { A.Annotation.empty_feature with
              A.Annotation.seq;
              intervals = [ { T.low = i * 100; length = 50 } ] }
      done;
      Unix.gettimeofday () -. started in
    (* Large enough that both measurements sit well clear of scheduler noise --
       at a few thousand features the smaller one lands near a millisecond and
       the ratio is mostly jitter. *)
    let _warm = build 5000 in
    let small = build 20000 in
    let large = build 80000 in
    let ratio = large /. Float.max small 1e-3 in
    Testing.check
      (Printf.sprintf
         "quadrupling the feature count costs far less than quadratically \
          (%.0f ms -> %.0f ms, %.1fx)" (small *. 1000.) (large *. 1000.) ratio)
      (fun () -> ratio < 8.);
    (* Order still has to be insertion order, which is the property the reversed
       representation is quietly relying on. *)
    Testing.check_string "features come back in insertion order"
      ~expected:"0,100,200,300,400"
      (let ann = ref (A.Annotation.create h) in
       let seq = A.Annotation.intern_seq !ann "chr1" in
       for i = 0 to 4 do
         ann :=
           A.Annotation.add !ann ~path:[ "annotation"; "gene" ]
             { A.Annotation.empty_feature with
               A.Annotation.seq;
               intervals = [ { T.low = i * 100; length = 50 } ] }
       done;
       let acc = ref [] in
       A.Annotation.iter (fun ~path:_ f ->
         match f.A.Annotation.intervals with
         | i :: _ -> List.accum acc (string_of_int i.T.low)
         | [] -> ()) !ann;
       List.rev !acc |> String.concat ","))

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

(* FASTA line wrapping.  The three outputs that emit sequence disagree on
   purpose: GFF3 is read by third-party tools that expect the conventional
   60-column wrap, whereas a tabular document is one whole record per line
   throughout -- a wrapped tail would be the only part of it [awk] and [sort]
   could not take a line at a time -- and an extracted feature is usually on its
   way down a pipe.  [set_fasta_width] overrides all three. *)

let test_fasta_wrapping () =
  Testing.section "FASTA line wrapping" (fun () ->
    (* 150 bases: two full 60-column lines and a short third. *)
    let unit = "ATGCCCGGGTAAGCGACTAGCGCATCGTCA" in
    let seq = unit ^ unit ^ unit ^ unit ^ unit in
    let ann =
      gff3 [ "chr1\tdemo\tgene\t1\t9\t.\t+\t.\tID=g1"; "##FASTA"; ">chr1"; seq ]
      |> A.GFF3.of_string in
    (* The lengths of the sequence lines, i.e. of everything after the deflines,
       which is what wrapping is visible as. *)
    let widths doc =
      let rec after = function
        | [] -> []
        | l :: rest when l <> "" && l.[0] = '>' -> rest
        | _ :: rest -> after rest in
      String.Split.on_char_as_list '\n' doc |> after
      |> List.filter (fun l -> l <> "")
      |> List.map (fun l -> string_of_int (String.length l))
      |> String.concat "," in
    Testing.check_string "GFF3 wraps its ##FASTA section at 60 by default"
      ~expected:"60,60,30" (widths (A.GFF3.to_string ann));
    Testing.check_string "the tabular format emits one line per sequence"
      ~expected:"150" (widths (A.Tabular.to_string ann));
    (* An override is global, so every check below has to put it back. *)
    A.set_fasta_width (Some 20);
    Testing.check_string "an override replaces GFF3's own default"
      ~expected:"20,20,20,20,20,20,20,10" (widths (A.GFF3.to_string ann));
    Testing.check_string "an override reaches the tabular writer too"
      ~expected:"20,20,20,20,20,20,20,10" (widths (A.Tabular.to_string ann));
    A.set_fasta_width (Some 0);
    Testing.check_string "a width of zero unwraps GFF3 as well"
      ~expected:"150" (widths (A.GFF3.to_string ann));
    Testing.check_string "a width of zero leaves the tabular writer unwrapped"
      ~expected:"150" (widths (A.Tabular.to_string ann));
    (* A negative width is a caller error, and is refused before the override is
       touched -- so the width in force is still the zero set above. *)
    Testing.check_raises "a negative width is refused"
      (fun () -> A.set_fasta_width (Some (-1)));
    Testing.check_string "a refused width does not disturb the one in force"
      ~expected:"150" (widths (A.Tabular.to_string ann));
    A.set_fasta_width None;
    Testing.check_string "clearing the override restores GFF3's own default"
      ~expected:"60,60,30" (widths (A.GFF3.to_string ann));
    Testing.check_string "clearing the override restores the tabular default"
      ~expected:"150" (widths (A.Tabular.to_string ann));
    (* Wrapping is a layout choice, not a format change: a document written when
       the tabular writer still wrapped at 60 has to keep reading back, or
       --to-tsv output would stop being readable across a version boundary. *)
    A.set_fasta_width (Some 60);
    let wrapped = A.Tabular.to_string ann in
    A.set_fasta_width None;
    Testing.check_string "a tabular document with a wrapped FASTA still reads back"
      ~expected:seq
      (match A.Annotation.reference (A.Tabular.of_string wrapped) with
       | None -> "(no reference)"
       | Some r -> Sequences.Reference.find r (T.Forward "chr1") |> fst))

(* The 1-based boundary.  Everything in the AST is 0-based half-open and
   everything on the wire is 1-based, and [OneBased] is the whole of the
   conversion.  Now that both directions have names, the property that matters
   can be stated: they are inverse, including over the zero-length site, which
   is the case each of the six writers used to re-derive for itself. *)

let test_one_based () =
  Testing.section "The 1-based boundary" (fun () ->
    let module O = A.OneBased in
    let ivl low length : T.simple_interval_t = { low; length } in
    let show (i: T.simple_interval_t) = Printf.sprintf "%d+%d" i.low i.length in
    (* Outbound: a run of bases is 1-based inclusive. *)
    Testing.check_string "a one-base interval spans one 1-based position"
      ~expected:"1..1" (O.(of_interval (ivl 0 1) |> to_string));
    Testing.check_string "a longer interval spans its length"
      ~expected:"100..999" (O.(of_interval (ivl 99 900) |> to_string));
    (* A zero-length site is the other spelling entirely. *)
    Testing.check_string "a zero-length site is written as a between-bases pair"
      ~expected:"100^101" (O.(of_interval (ivl 100 0) |> to_string));
    (* Inbound and outbound are inverse, which is the point of having both. *)
    Testing.check "the two directions are inverse over ordinary intervals"
      (fun () ->
        List.for_all
          (fun i -> show O.(of_interval i |> to_interval) = show i)
          [ ivl 0 1; ivl 0 10; ivl 99 900; ivl 5 3; ivl 1000000 1 ]);
    Testing.check "and over a zero-length site"
      (fun () ->
        List.for_all
          (fun i -> show O.(of_interval i |> to_interval) = show i)
          [ ivl 1 0; ivl 100 0; ivl 999 0 ]);
    (* The text form is inverse too, so what a format writes it reads. *)
    Testing.check "the text form round-trips a range"
      (fun () -> O.(of_string (to_string (Range (7, 9)))) = O.Range (7, 9));
    Testing.check "and a between-bases site"
      (fun () -> O.(of_string (to_string (Between (7, 8)))) = O.Between (7, 8));
    Testing.check_string "an interval survives being rendered and read back"
      ~expected:"99+900"
      (show O.(of_interval (ivl 99 900) |> to_string |> of_string |> to_interval));
    Testing.check_string "and so does a zero-length site"
      ~expected:"100+0"
      (show O.(of_interval (ivl 100 0) |> to_string |> of_string |> to_interval));
    (* The other convention: the plain pair, where a zero-length interval comes
       out inverted because GFF3 and GTF have no other way to spell it. *)
    Testing.check_string "the bounds of an ordinary interval"
      ~expected:"100,999" (let lo, hi = O.bounds (ivl 99 900) in
                           Printf.sprintf "%d,%d" lo hi);
    Testing.check_string "the bounds of a zero-length interval are inverted"
      ~expected:"101,100" (let lo, hi = O.bounds (ivl 100 0) in
                           Printf.sprintf "%d,%d" lo hi);
    Testing.check "bounds and interval_of_bounds are inverse"
      (fun () ->
        List.for_all
          (fun i ->
            let lo, hi = O.bounds i in
            show (O.interval_of_bounds ~lo ~hi) = show i)
          [ ivl 0 1; ivl 99 900; ivl 5 3 ]);
    Testing.check "including over the inverted zero-length pair"
      (fun () ->
        let i = ivl 100 0 in
        let lo, hi = O.bounds i in
        show (O.interval_of_bounds ~lo ~hi) = show i);
    (* What each direction refuses. *)
    Testing.check_raises "a coordinate below one is refused"
      (fun () -> ignore (O.interval_of_bounds ~lo:0 ~hi:500));
    Testing.check_raises "a reversed range that is not the zero-length pair is refused"
      (fun () -> ignore (O.interval_of_bounds ~lo:10 ~hi:5));
    Testing.check_raises "a between-bases site whose positions are not consecutive is refused"
      (fun () -> ignore (O.to_interval (O.Between (100, 999))));
    Testing.check_raises "a malformed interval string is refused"
      (fun () -> ignore (O.of_string "not-an-interval"));
    Testing.check_raises "and one with a non-numeric bound"
      (fun () -> ignore (O.of_string "a..b")))

(* How a reference is normalised on the way in.  Three readers can attach one
   -- GenBank's ORIGIN block, GFF3's ##FASTA directive and the tabular format's
   sidecar -- and they used to disagree, so the same sequence gave a different
   answer depending on which door it came through.  GenBank folded every
   ambiguity code to N; the other two kept the case, so a soft-masked genome
   translated to X throughout, the codon tables being upper-case only.  All
   three now upper-case and do nothing else, which is what AnnoTools already
   did for a reference given as plain FASTA. *)

let test_reference_linting () =
  Testing.section "Reference normalisation" (fun () ->
    (* Soft masking and ambiguity codes together: the two things a linter can
       destroy, and it destroys them in opposite directions. *)
    let raw = "atgcccgggtaaRYgactagcgcatcgtca" in
    let expected = "ATGCCCGGGTAARYGACTAGCGCATCGTCA" in
    let sequence_of ann =
      match A.Annotation.reference ann with
      | None -> "(no reference)"
      | Some r -> Sequences.Reference.find r (T.Forward "demo01") |> fst in
    let via_genbank = genbank ~seq:raw [] |> A.GenBank.of_string in
    let via_gff3 =
      gff3 [ "demo01\tdemo\tgene\t1\t9\t.\t+\t.\tID=g1";
             "##FASTA"; ">demo01"; raw ]
      |> A.GFF3.of_string in
    Testing.check_string "a GenBank ORIGIN is upper-cased and otherwise left alone"
      ~expected (sequence_of via_genbank);
    Testing.check_string "a GFF3 ##FASTA section likewise"
      ~expected (sequence_of via_gff3);
    Testing.check_string "so the two readers agree on the same sequence"
      ~expected:(sequence_of via_genbank) (sequence_of via_gff3);
    (* The tabular sidecar is the third door, reached by rendering and reading
       back; it has to agree too. *)
    Testing.check_string "and the tabular sidecar agrees after a round trip"
      ~expected (sequence_of (A.Tabular.to_string via_genbank |> A.Tabular.of_string));
    (* What each of the two former behaviours cost, stated as the outputs that
       used to come out wrong. *)
    Testing.check "ambiguity codes survive, where GenBank used to fold them to N"
      (fun () ->
        let s = sequence_of via_genbank in
        String.contains s 'R' && String.contains s 'Y');
    Testing.check_string "and a soft-masked sequence still translates"
      ~expected:"MPG"
      (match feature_at via_gff3 "gene" with
       | Some (_, f) -> A.Annotation.feature_protein via_gff3 f
       | None -> "(no gene)");
    Testing.check_string "rather than to X throughout"
      ~expected:"ATGCCCGGG"
      (match feature_at via_gff3 "gene" with
       | Some (_, f) -> A.Annotation.feature_dna via_gff3 f
       | None -> "(no gene)"))

let run () =
  test_one_based ();
  test_reference_linting ();
  test_locations ();
  test_genbank_records ();
  test_genbank_round_trip ();
  test_genbank_headers ();
  test_gff3_fasta ();
  test_feature_sequence ();
  test_selection ();
  test_attributes ();
  test_gff3_fidelity ();
  test_attribute_order ();
  test_tabular ();
  test_fasta_wrapping ();
  test_feature_table ();
  test_add_invariants ();
  test_insertion_cost ();
  test_extraction_prerequisites ()

